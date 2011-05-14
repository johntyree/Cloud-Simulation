-define(GRIDSIZE_X, 30).
-define(GRIDSIZE_Y, 30).
-define(GRIDSIZE_Z, 1).
-define(INITIAL_DENSITY, 0.3).
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

random_int(U) when is_integer(U), U >= 0 -> random_int(0, U).
random_int(L, U) when is_integer(U), is_integer(L) -> crypto:rand_uniform(L, U).


bisect(Int) ->
    case Int rem 2 of 
        0 -> Left = Right = Int / 2;
        1 -> Left = 1 + (Right = Int / 2)
    end,
    {Left, Right}.
