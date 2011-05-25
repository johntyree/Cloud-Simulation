-define(GRIDSIZE_X, 50).
-define(GRIDSIZE_Y, 200).
-define(GRIDSIZE_Z, 1).

%% Used to determine frequence of new drops appearing
-define(RELATIVE_HUMIDITY, 1).

%% All movement scaled by this amount
-define(SCALE, 0.001).

%% Expand radius search by this factor to determine collisions with neighbors
-define(COLLISION_SCALE_CHECK, 10).

%% 50% chance to split at this size
-define(HALF_SPLIT_SIZE, 2.5).
-define(SPLIT_STEEPNESS, 3.5).
%% Sigmoid model f(x) = 1 - 1/(1 + (1 - splitsize + dropsize)^steepness

%% Unused right now
-define(FINAL_DROP_COUNT, 1).
-define(TIMEOUT, 500).

%% Lateral and vertical windspeeds
%-define(WINDSPEED, 500.0). % mm/s
-define(WINDSPEED, 1000.0). % mm/s
-define(UPDRAFT, 2000.0). % mm/s

%% Return saturation water vapor pressure for C degrees celsius
%% float -> float
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
%% float -> float -> float
gaussian(Mu, Sigma) when is_float(Mu), is_float(Sigma) ->
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

%% Random float generator in domain (L, U)
%% float
random_uniform_nonboundary(U, U) when is_number(U) -> U;
random_uniform_nonboundary(L, U) when is_number(U), is_number(L) ->
    X = (U - L) * random:uniform() + L,
    %crypto:random_bytes
    if L == X; U == X -> %% To match integers
            random_uniform_nonboundary(L, U);
        true ->
            X
    end.

%% Random int generator in domain [L,U] (default L = 0)
%% Inclusive!
%% int -> int
%% int -> int -> int
random_int(U) when is_integer(U) -> random_int(0, U).
%% Can't have negatives
random_int(L, U) when is_integer(L), is_integer(U), L =< U ->
    crypto:rand_uniform(0, U-L+1) + L;
    %random:uniform(U-L+1) + (L-1).
random_int(L, U) when is_integer(L), is_integer(U), L > U ->
    random_int(U, L).

%% Return random int in domain [RawL, RawU] with distribution such that the
%% mean value is RawL, + RawU / 2. Used to scale movement down discretely.
%% float -> int
%% float -> float -> int
scaled_random_int(U) when is_float(U) -> scaled_random_int(0, U).
scaled_random_int(RawL, RawU) when is_float(RawL), is_float(RawU), RawL =< RawU ->
    L = RawL * ?SCALE,
    U = RawU * ?SCALE,
    FL = floor(L),
    CU = ceiling(U),
    BumpUp = abs(L - FL),
    BumpDown = abs(U - CU),
    Fraction = random:uniform(),
    %io:format("FL  ~p    CU  ~p    BumpDown  ~p    BumpUp  ~p    Fraction ~p~n", [FL, CU, BumpDown, BumpUp, Fraction]),
    case random_int(FL, CU) of
        Base when Base =:= FL, BumpUp > Fraction ->
            %io:format("BUMP UP~n"),
            Base + 1;
        Base when Base =:= CU, BumpDown > Fraction ->
            %io:format("BUMP DOWN~n"),
            Base - 1;
        Base -> Base
    end.

%% Divide int evenly into two integer parts, favoring left when int is odd.
%% int -> {int, int}
bisect(Int) ->
    case Int rem 2 of
        0 -> Left = Right = Int / 2;
        1 -> Left = 1 + (Right = Int div 2)
    end,
    {Left, Right}.

%% Limit Val to domain of (Max, Min) such that Max + 1 = Min.
%% Inclusive! Not like modulo.
%% number -> number -> number -> number
make_periodic(_Val, Min, Max) when Max == Min -> Min;
make_periodic(Val, Min, Max) when Max < Min -> make_periodic(Val, Max, Min);
make_periodic(Val, Min, Max) when Min =< Val, Val =< Max -> Val;
make_periodic(Val, Min, Max) when Val < Min ->
    make_periodic(Val + (Max - Min + 1), Min, Max);
make_periodic(Val, Min, Max) when Val > Max ->
    make_periodic(Val - (Max - Min + 1), Min, Max).

positive_periodic(Val, _Min, Max) when Val > Max ->
    make_periodic(Val, round(Max * 0.75), Max);
positive_periodic(Val, Min, _Max) when Val < Min -> undefined;
positive_periodic(Val, _Min, _Max) -> Val.


%% For values of Val outside of (Min, Max), reflect positive values back into
%% the domain, return undefined for negative values.
%% Int -> Int -> Int -> Int | undefined
positive_mirrored(Val, _Min, Max) when is_integer(Max), is_integer(Val), Val > Max ->
    round(Max * (1 - random:uniform() * 0.25));
positive_mirrored(Val, Min, _Max) when is_integer(Min), is_integer(Val), Val < Min ->
    undefined;
positive_mirrored(Val, _Min, _Max) when is_integer(Val) -> Val.


%% Nicely print out a dict
%% dict -> ok
print_dict(D) ->
    dict:map(fun(K, V) -> io:format("~p: ~p~n", [K, V]) end, D),
    ok.

%% Throw out all received messages or N received messages. Print them if N is
%% set and atom() =:= print
%% With no arguments, returns immediately. Otherwise, obviously, waits
%% indefinitely for N messages to arrive.
%% ok
%% Int -> ok
%% Int -> atom() -> ok
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
    after ?TIMEOUT ->
            io:format("Still waiting for ~p messages.~n", [N]),
            flush(N, print, Acc)
    end;
flush(N, Flag, Acc) ->
    receive
        X -> flush(N-1, Flag, [X|Acc])
    after ?TIMEOUT ->
            io:format("Still waiting for ~p messages.~n", [N]),
            flush(N, Flag, Acc)
    end.

%% Return the radius of a sphere of volume Volume
%% float -> float
radius(Volume) when is_float(Volume) -> math:pow(0.75 * Volume / math:pi(), 1/3).
%% Return the volume of a sphere of radius Radius
%% float -> float
volume(Radius) when is_float(Radius) -> 4/3 * math:pi() * Radius * Radius *
    Radius.

%% For 1000 calls to X(), return the fraction that are true.
%% fun -> float
percent_true(X) ->
    length([ true || true <- [ X() || _ <- lists:seq(1,1000)]]) / 1000.

%% For 1000 calls to X(), return the average value returned
%% fun -> float
avg_value(X) -> lists:sum([ X() || _ <- lists:seq(1,1000)]) / 1000.

%% Round towards negative infinity
%% float -> int
floor(X) when is_float(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

%% Round towards positive infinity
%% float -> int
ceiling(X) when is_float(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

%% Round away from 0
%% float -> int
round_out(X) when is_float(X), X < 0 -> trunc(X) - 1;
round_out(X) when is_float(X), X > 0 -> trunc(X) + 1;
round_out(0) -> 0.

