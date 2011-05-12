#!/usr/bin/env escript

-define(DEPTH, 5).
-define(WIDTH, 4).

main(_) ->
    S = self(),
    [spawn(fun() -> keepGoing(?DEPTH, S, 1) end) || _X <- lists:seq(1,?WIDTH)],
    Kids = trunc(math:pow(?WIDTH, ?DEPTH)),
    announce(S, Kids, 0),
    X = collectKids(Kids),
    %[io:format("~p: ~pms~n", [Pid, Time/1000]) || {Pid, Time} <- X],
    ok.

collectKids(N) -> collectKids(N, []).
collectKids(0, Acc) ->
    Acc;
collectKids(N, Acc) ->
    receive
        {Pid,Sent} ->
            %io:format("| Got ~p~n", [N]),
            collectKids(N-1, [{Pid, epochtime() - Sent}|Acc])
    end.

keepGoing(Depth, Parent, N) when N < Depth ->
    S = self(),
    [spawn(fun() -> keepGoing(Depth, S, N+1) end) || _X <- lists:seq(1,?WIDTH)],
    Kids = trunc(math:pow(?WIDTH, ?DEPTH-N)),
    %announce(S, Kids, N),
    [forward_message(Parent) || _X <- lists:seq(1,Kids)];
keepGoing(Depth, Parent, _N) ->
    %io:format("I'm ~p's kid at Level ~p!~n", [Parent, Depth]),
    Parent ! themsg().

forward_message(Parent) -> receive Pid -> Parent ! Pid end.

themsg() -> {self(), epochtime()}.

epochtime() ->
    {MegaS, S, MilliS} = now(),
    MegaS*1000000000000 + S*1000000 + MilliS.

announce(S, Kids, N) ->
    io:format("~~ Level ~p ~p, waiting on ~p kids...~n", [N, S, Kids]).
