-module(cloud_simulation).
-compile(export_all). %% Probably replace with -export([funcs])

-include_lib("constants.hrl").
-include_lib("node.hrl").

main([N]) ->
    {Iters, []} = string:to_integer(N),
    initial_config(),
    Cloud = spawn(node, init, [self()]),
    monitor(process, Cloud),
    Cloud ! {repopulate, 1.0},
    node_info(Cloud),
    run(Iters, Cloud),
    %fprof:apply(fun run/2, [Iters, Cloud]),
    %fprof:profile(),
    %fprof:analyse(),
    init:stop(),
    ok.

%% Set up environment, seed rng.
%% ok
initial_config() ->
    %error_logger:logfile({open, "log"}),
    <<A:32,B:32,C:32>> = crypto:rand_bytes(12),
    random:seed(A, B, C),
    error_logger:tty(false),
    ok.

%% Print metadata about node Cloud to stderr.
%% Pid -> ok
node_info(Cloud) when is_pid(Cloud) -> node_info(Cloud, print).
node_info(Cloud, Flag) when is_pid(Cloud) ->
    Cloud ! info,
    receive
        #nodeinfo{size = Size, volume = Volume} ->
            if Flag =:= print ->
                    io:format(standard_error, "Cumulative Water Volume: ~pmm³\t", [Volume]),
                    io:format(standard_error, "Dict size (~~# of drops): ~p~n", [Size]);
                true -> ok
            end;
        X -> io:format(standard_error, "Got ~p instead of nodeinfo", [X])
    end.

%% Run simulation on node Cloud. Kill after Iters timesteps or Cloud's drop
%% count below FINAL_DROP_COUNT.
%% Int -> Pid -> ok
run(Iters, Cloud) when is_integer(Iters) -> run(0, Iters, Cloud, 0).
run(Iters, Iters, Cloud, _) ->
    Cloud ! {die, self()},
    wait(Cloud);
run(N, Iters, Cloud, Time) ->
    if N rem 20 =:= 0 ->
            io:format(standard_error, "~p, ", [N+1]),
            io:format(standard_error, "~p ", [Time / (N+1)]),
            node_info(Cloud),
            ok;
        true -> ok
    end,
    {T, _} = timer:tc(fun move_drops/1, [Cloud]),
    Cloud ! {size, self()},
    receive %% the size
        %% Keep going until max iterations are reached or only one drop left
        X when is_integer(X) and (X > ?FINAL_DROP_COUNT) ->
            %error_logger:info_report(io_lib:format("~p", [X])),
            %io:format(standard_error, "~p drops~n", [X]),
            run(N + 1, Iters, Cloud, Time + T / 1000000);
        X ->
            %error_logger:info_report(io_lib:format("Got ~p, Sending death message.~n", [X])),
            io:format(standard_error, "~nGot ~p, Sending death message.~n", [X]),
            run(Iters, Iters, Cloud, Time + T / 1000000)
    end.


%% Tell Cloud to move drops one step. Handle out of bounds drops that are
%% sent back.
%% Pid -> ok
move_drops(Cloud) ->
    Cloud ! move,
    receive
        ok -> ok;
        {route_drops, DropDict} ->
            %io:format(standard_error, "~p drops OOB ", [dict:size(DropDict)]),
            %io:format(standard_error, "~p~n", [DropDict]),
            Keepers = node:handle_boundary_drops(#nodestate{}, DropDict),
            %io:format(standard_error, "Handled boundary cases. (~p out of about ~p)~n",
                %[length(Keepers), dict:size(DropDict)]),
            [Cloud ! {new_drop, D} || D <- Keepers];
            %io:format(standard_error, "Resent drops to Cloud.~n", [])
        X -> io:format(standard_error, "Got ~p instead of route_drops", [X])
    end,
    ok.

%% Wait for Cloud to die.
%% Pid -> ok
wait(Cloud) ->
    receive
        {ok_im_dead, Cloud} -> ok;
        X -> io:format(standard_error, "~p ", [X]),
            wait(Cloud)
    end.
