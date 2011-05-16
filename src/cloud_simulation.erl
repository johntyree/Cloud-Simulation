-module(cloud_simulation).
-compile(export_all). %% Probably replace with -export([funcs])

-include_lib("constants.hrl").

main([N]) ->
    {Iters, []} = string:to_integer(N),
    initial_config(),
    Cloud = spawn(node, init, [self()]),
    Cloud ! repopulate,
    run(Iters, Cloud),
    init:stop().

initial_config() ->
    error_logger:logfile({open, "log"}).

run(0, Cloud) ->
    Cloud ! {die, self()},
    wait(Cloud);
run(N, Cloud) when is_integer(N) ->
    Cloud ! move,
    Cloud ! {size, self()},
    receive
        %% Keep going until max iterations are reached
        X when X > 1 ->
            %error_logger:info_report(io_lib:format("~p", [X])),
            run(N - 1, Cloud);
        _X ->
            %error_logger:info_report(io_lib:format("Got ~p, Sending death message.~n", [X])),
            run(0, Cloud)
    end.

wait(Cloud) ->
    receive
        {ok_im_dead, Cloud} -> ok;
        _ -> wait(Cloud)
    end.
