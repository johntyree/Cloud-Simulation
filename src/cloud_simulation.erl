-module(cloud_simulation).
-compile(export_all). %% Probably replace with -export([funcs])

-include("constants.hrl").

main(0) ->
    io:format("~p~n", [gaussian(0, 1)]),
    %io:format("~p ", [random_nonzero()]),
    erlang:halt(0);
main(N) ->
    io:format("~p ", [gaussian(0, 1)]),
    %io:format("~p ", [random_nonzero()]),
    main(N-1).

