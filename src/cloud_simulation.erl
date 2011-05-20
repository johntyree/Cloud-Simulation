-module(cloud_simulation).
-compile(export_all). %% Probably replace with -export([funcs])

-include_lib("constants.hrl").
-include_lib("node.hrl").

main([N]) ->
    {Iters, []} = string:to_integer(N),
    initial_config(),
    Cloud = spawn_link(node, init, [self()]),
    Cloud ! repopulate,
    Cloud ! info,
    receive
        ok -> ok;
        #nodeinfo{volume = Volume} when Volume < 4 * ?HALF_SPLIT_SIZE ->
            io:format("Cumulative Water Volume: ~pmm³~n", [Volume]),
            io:format("Not enough water!~n"),
            erlang:halt(1);
        #nodeinfo{volume = Volume} ->
            io:format("Cumulative Water Volume: ~pmm³~n", [Volume])
    end,
    run(Iters, Cloud),
    %fprof:apply(fun run/2, [Iters, Cloud]),
    %fprof:profile(),
    %fprof:analyse(),
    init:stop(),
    ok.

initial_config() ->
    %error_logger:logfile({open, "log"}),
    <<A:32,B:32,C:32>> = crypto:rand_bytes(12),
    random:seed(A, B, C),
    error_logger:tty(false),
    ok.

run(0, Cloud) ->
    Cloud ! {die, self()},
    wait(Cloud);
run(N, Cloud) when is_integer(N) ->
    if N rem 20 =:= 0 ->
            io:format("~n~p ", [N]),
            ok;
        true -> ok
    end,
    Cloud ! move,
    Cloud ! {size, self()},
    flush(1), %% The 'ok' message after moving the drops.
    receive %% the size
        %% Keep going until max iterations are reached or only one drop left
        X when is_integer(X) and (X > ?FINAL_DROP_COUNT) ->
            %error_logger:info_report(io_lib:format("~p", [X])),
            io:format("~p ", [X]),
            run(N - 1, Cloud);
        X ->
            %error_logger:info_report(io_lib:format("Got ~p, Sending death message.~n", [X])),
            io:format("~nGot ~p, Sending death message.~n", [X]),
            run(0, Cloud)
    end.

wait(Cloud) ->
    receive
        {ok_im_dead, Cloud} -> ok;
        X -> io:format("~p ", [X]),
            wait(Cloud)
    end.
