-module(cloud_simulation).
-compile(export_all). %% Probably replace with -export([funcs])

-include_lib("constants.hrl").

main(Args) ->
    initial_config(),
    L = deity:init(Args),

    [io:format("~p ~p ~p~n", [X, Y, Z]) || {{X, Y}, {_, Z}} <- L],
    erlang:halt(0).

initial_config() ->
    error_logger:logfile({open, "log"}).

run(0) ->
    {X, Y} = drop:new_position(),
    io:format("~p ~p~n", [X, Y]),
    %io:format("~p~n", [drop:new_size()]),
    %io:format("~p~n", [gaussian(100, 4.1)]),
    %io:format("~p~n", [random:uniform()]),
    erlang:halt(0);
run(N) ->
    {X, Y} = drop:new_position(),
    io:format("~p ~p ", [X, Y]),
    %io:format("~p ", [gaussian(100, 4.1)]),
    %io:format("~p ", [random:uniform()]),
    run(N-1).
