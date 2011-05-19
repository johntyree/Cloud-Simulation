-define(GRIDSIZE_X, 5).
-define(GRIDSIZE_Y, 5).
-define(GRIDSIZE_Z, 1).
-define(INITIAL_DENSITY, 0.6).
-define(RELATIVE_HUMIDITY, ?INITIAL_DENSITY).
-define(TIMEOUT, 500).

saturation_pressure(C) when C >= -50, C =< 102 ->
    T = 273.16 + C,
    math:pow(10,
        -7.90298
        * (373.16/T - 1)
        + 5.02808
        * math:log10(373.16/T)
        - 0.00000013816
        * (math:pow(10, 11.344
                * (1 - T/373.16))
                - 1)
        + 0.0081328
        * (math:pow(10, -3.49149
                * (373.16/T - 1))
                - 1)
        + math:log10(1013.246)).


% Fast gaussian generator algorithm from (Leva 1992)
% http://portal.acm.org/citation.cfm?id=138364
gaussian(Mu, Sigma) ->
    S = 0.449871,
    T = -0.386595,
    A = 0.19600,
    B = 0.25472,
    R1 = 0.27597,
    R2 = 0.27846,
    U = random_uniform_nonboundary(0, 1), % To avoid singularity
    V = 1.7156 * (random_uniform_nonboundary(0, 1) - 0.5),
    X = U - S,
    Y = abs(V) - T,
    Q = X * X + Y * (A * Y - B * X),
    if Q < R1 ->
            Sigma * (V / U) + Mu;
        Q > R2 ->
            gaussian(Mu, Sigma);
        true ->
            case (V * V) > (-4 * U * U * math:log(U)) of
                true -> gaussian(Mu, Sigma);
                false -> Sigma * (V / U) + Mu
            end
    end.

random_uniform_nonboundary(L, U) ->
    <<A:32,B:32,C:32>> = crypto:rand_bytes(12),
    random:seed(A, B, C),
    X = (U - L) * random:uniform() + L,
    %crypto:random_bytes
    if L == X; U == X -> %% To match integers
            random_uniform_nonboundary(L, U);
        true ->
            X
    end.

%% Inclusive!
random_int(U) -> random_int(0, U).
%% Can't have negatives
random_int(L, U) when is_integer(L), is_integer(U), L =< U ->
    crypto:rand_uniform(0, U-L+1) + L.


bisect(Int) ->
    case Int rem 2 of
        0 -> Left = Right = Int / 2;
        1 -> Left = 1 + (Right = Int div 2)
    end,
    {Left, Right}.

%% Inclusive! Not like modulo.
make_periodic(_Val, Min, Max) when Max == Min -> Min;
make_periodic(Val, Min, Max) when Max < Min -> make_periodic(Val, Max, Min);
make_periodic(Val, Min, Max) when Min =< Val, Val =< Max -> Val;
make_periodic(Val, Min, Max) when Val < Min ->
    make_periodic(Val + (Max - Min + 1), Min, Max);
make_periodic(Val, Min, Max) when Val > Max ->
    make_periodic(Val - (Max - Min + 1), Min, Max).

print_dict(D) ->
    dict:map(fun(K, V) -> io:format("~p: ~p~n", [K, V]) end, D),
    ok.

flush() ->
    receive
        X -> [X|flush()]
    after 0 ->
        []
    end.
flush(N) when is_integer(N) -> flush(N, false, []).
flush(N, print) when is_integer(N) -> flush(N, print, []);
flush(N, _) when is_integer(N) -> flush(N, false, []).
flush(0, _, Acc) -> lists:reverse(Acc);
flush(N, print, Acc) ->
    receive
        X ->
            io:format("~p ", [X]),
            flush(N-1, print, [X|Acc])
    end;
flush(N, Flag, Acc) ->
    receive
        X -> flush(N-1, Flag, [X|Acc])
    end.

area(Radius) when is_number(Radius) -> math:pi() * Radius * Radius.
radius(Area) when is_number(Area) -> math:sqrt(Area / math:pi()).
