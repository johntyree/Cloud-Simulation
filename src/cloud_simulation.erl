-module(cloud_simulation).
-compile(export_all). %% Probably replace with -export([funcs])

-include_lib("constants.hrl").
-include_lib("node.hrl").

main([N]) ->
    {Iters, []} = string:to_integer(N),
    initial_config(),
    Cloud = spawn(node, init, [self()]),
    monitor(process, Cloud),
    Cloud ! repopulate,
    V = check_volume(Cloud),
    if V < 4 * ?HALF_SPLIT_SIZE ->
            io:format(standard_error, "Not enough water!~n", []),
            %erlang:halt(1);
            ok;
        true -> ok
    end,
    run(Iters, Cloud),
    %fprof:apply(fun run/2, [Iters, Cloud]),
    %fprof:profile(),
    %fprof:analyse(),
    init:stop(),
    ok.

check_volume(Cloud) when is_pid(Cloud) -> check_volume(Cloud, print).
check_volume(Cloud, Flag) when is_pid(Cloud) ->
    Cloud ! info,
    receive
        #nodeinfo{volume = Volume} ->
            if Flag =:= print ->
                    io:format(standard_error, "Cumulative Water Volume: ~pmm³~n", [Volume]);
                true -> ok
            end
    end,
    Volume.

initial_config() ->
    %error_logger:logfile({open, "log"}),
    <<A:32,B:32,C:32>> = crypto:rand_bytes(12),
    random:seed(A, B, C),
    error_logger:tty(false),
    ok.

run(Iters, Cloud) when is_integer(Iters) -> run(0, Iters, Cloud).
run(Iters, Iters, Cloud) ->
    Cloud ! {die, self()},
    wait(Cloud);
run(N, Iters, Cloud) ->
    if N rem 1 =:= 0 ->
            io:format(standard_error, "~nStep ~p ", [N+1]),
            ok;
        true -> ok
    end,
    move_drops(Cloud),
    Cloud ! {size, self()},
    receive %% the size
        %% Keep going until max iterations are reached or only one drop left
        X when is_integer(X) and (X > ?FINAL_DROP_COUNT) ->
            %error_logger:info_report(io_lib:format("~p", [X])),
            io:format(standard_error, "~p drops in the cloud~n", [X]),
            run(N + 1, Iters, Cloud);
        X ->
            %error_logger:info_report(io_lib:format("Got ~p, Sending death message.~n", [X])),
            io:format(standard_error, "~nGot ~p, Sending death message.~n", [X]),
            run(Iters, Iters, Cloud)
    end.

move_drops(Cloud) ->
    Cloud ! move,
    receive
        ok -> ok;
        {route_drops, DropDict} ->
            io:format(standard_error, "~nGot drops to route.~n", []),
            %io:format(standard_error, "~p~n", [DropDict]),
            Keepers = node:handle_boundary_drops(#nodestate{}, DropDict),
            io:format(standard_error, "Handled boundary cases. (~p out of about ~p)~n",
                [length(Keepers), dict:size(DropDict)]),
            [Cloud ! {new_drop, D} || D <- Keepers],
            io:format(standard_error, "Resent drops to Cloud.~n", [])
    end,
    ok.

wait(Cloud) ->
    receive
        {ok_im_dead, Cloud} -> ok;
        X -> io:format(standard_error, "~p ", [X]),
            wait(Cloud)
    end.
