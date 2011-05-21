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
    V = check_volume(Cloud, print),
    if V < 4 * ?HALF_SPLIT_SIZE ->
            io:format("Not enough water!~n"),
            erlang:halt(1);
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
                    io:format("Cumulative Water Volume: ~pmm³~n", [Volume]);
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
            io:format("~n~p ", [N+1]),
            ok;
        true -> ok
    end,
    Cloud ! move,
    receive
        ok -> ok;
        {route_drops, DropList} ->
            io:format("~nGot drops to route.~n"),
            Keepers = [{{X, Y, Z}, Drop} || {{X, Y, Z}, Drop} <- DropList, Y
                < ?GRIDSIZE_Y ],
            io:format("Removed from bottom.~n"),
            Keepers2 = [node:periodicise_drop(#nodestate{}, {{X, 3,
                            Z}, Drop}) || {{X, Y, Z}, Drop} <- Keepers, Y
                < 0 ],
            io:format("Periodicised.~n"),
            [Cloud ! {new_drop, D} || D <- Keepers2]
    end,
    Cloud ! {size, self()},
    receive %% the size
        %% Keep going until max iterations are reached or only one drop left
        X when is_integer(X) and (X > ?FINAL_DROP_COUNT) ->
            %error_logger:info_report(io_lib:format("~p", [X])),
            io:format("~p ", [X]),
            run(N + 1, Iters, Cloud);
        X ->
            %error_logger:info_report(io_lib:format("Got ~p, Sending death message.~n", [X])),
            io:format("~nGot ~p, Sending death message.~n", [X]),
            run(Iters, Iters, Cloud)
    end.

wait(Cloud) ->
    receive
        {ok_im_dead, Cloud} -> ok;
        X -> io:format("~p ", [X]),
            wait(Cloud)
    end.
