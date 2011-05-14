#!/usr/bin/escript

-record(d, {
        x,
        y,
        size
    }).

main(_) ->
    L = [#d{x=1, y=2}, #d{x=1}, #d{x=2}],
    R = [#d{x=8, y=2}, #d{x=1, size = 90}, #d{x=2}],
    %io:format("~p~n", [count_em(L)]),
    %io:format("~p~n", [count_em(R)]),
    io:format("~p~n", [dict:merge(fun (K, V1, V2) -> V2 end, count_em(L),
                count_em(R))]),

    %io:format("~p~n", [orddict:new()]),
    ok.


count_em(List) ->
    L = [extract(X) || X <- List],
    count_em(L, dict:new()).

count_em([], D) ->
    %return the groups
    D;
count_em([{X, Y, R}|T], D) ->
    D2 = dict:append({X, Y}, R, D),
    count_em(T, D2 ).


extract(R = #d{x = X, y = Y}) -> {X, Y, R}.

%[orddict:append({X, Y}, Record, D) || X <- L],

