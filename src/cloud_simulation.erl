-module(cloud_simulation).
-compile(export_all). %% Probably replace with -export([funcs])

-include_lib("constants.hrl").

main([N]) ->
    {Iters, []} = string:to_integer(N),
    initial_config(),
    Cloud = spawn(node, init, [self()]),
    Cloud ! repopulate,
    run(Iters, Cloud),
    init:stop(),
    ok.

initial_config() ->
    %error_logger:logfile({open, "log"}),
    error_logger:tty(false).

run(0, Cloud) ->
    Cloud ! {die, self()},
    wait(Cloud);
run(N, Cloud) when is_integer(N) ->
    if N rem 5 =:= 0 ->
            io:format("~n~p ", [N]);
        true -> ok
    end,
    Cloud ! move,
    Cloud ! {size, self()},
    flush(1), %% The 'ok' message after moving the drops.
    receive %% the size
        %% Keep going until max iterations are reached or only one drop left
        X when is_integer(X) and (X > 1) ->
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
