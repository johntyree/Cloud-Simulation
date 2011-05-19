-module(drop).
-compile(export_all). %% Probably replace with -export([funcs])
-author("John Tyree").

-include_lib("constants.hrl").
-include_lib("drop.hrl").

%% Make a new drop
new() ->
    Size = new_size(),
    #dropstate{size = Size}.

coalesce(S1 = #dropstate{}, S2 = #dropstate{}) ->
    NewSize = radius(volume(S1#dropstate.size) + volume(S2#dropstate.size)),
    S1#dropstate{size = NewSize}.

%% Take any size drop, return a list of either one or two drops depending on
%% size and probability to split.
% dropstate -> [dropstate]
split(S = #dropstate{size = Size}) ->
    case it_splits(Size) of
        true ->
            NewSize = volume(radius(Size) / 2),
            [S#dropstate{size = NewSize}, S#dropstate{size = NewSize}];
        false ->
            [S#dropstate{size = Size}]
    end.

%% Return true if the drop splits, based on stochastically based on size.
%% int -> bool
it_splits(Size) when is_number(Size), Size =< ?HALF_SPLIT_SIZE - 1 -> false;
it_splits(Size) when is_number(Size), Size >= 0 ->
    1 - (1 / (1 + math:pow(1 - ?HALF_SPLIT_SIZE + Size, ?SPLIT_STEEPNESS)))
    > random_uniform_nonboundary(0,1).

move({P, D}) ->
    NewPosition = migrate(P, random_direction()),
    {NewPosition, D}.

%coords(S = #dropstate{x = X, y = Y, z = Z}) -> {X, Y, Z}.

%%%%%%%%%%%%%%%%%%%%%%%
%%                   %%
%% Private Functions %%
%%                   %%
%%%%%%%%%%%%%%%%%%%%%%%

%% Change the actual position,
%% This is basically just zipWith((+), T1, T2) where T's are three-tuples
%% ret: {coords}
migrate({X, Y, Z}, {DX, DY, DZ}) ->
    NewX = X + DX,
    NewY = Y + DY,
    NewZ = Z + DZ,
    {NewX, NewY, NewZ}.

%% Chose a random x and y movement from -1,0,1
random_direction() -> random_direction(1).
random_direction(Step) ->
    {random_int(-Step, Step), random_int(-Step, Step), 0}.
    %% For 3D, add the Z dimension
    %{random_int(-Step, Step), random_int(-Step, Step), random_int(-Step, Step)}.

%%  Stochastic decision making
%% when two drops are at the same site
handle_collision(_Us, _Them) ->
    ok.

%% Size of a new drop
%% http://ga.water.usgs.gov/edu/raindropsizes.html
%% Range 0.001mm, 0.05mm. "Rain" at 0.5mm.
new_size() ->
    X = gaussian(0.025, 0.006),
    if X =< 0 ->
            new_size();
        true ->
            X
    end.

%% Retrieve the state of a drop
get_state(Pid) ->
    ?TIMEOUT = constants:timeout(),
    Ref = make_ref(),
    Pid ! {send_state, Ref, self()},
    receive
        {send_state, Ref, S} -> S
    after ?TIMEOUT ->
            error_logger:format("~p: No response from Drop ~p.", [self(), Pid]),
            exit(timeout)
    end.
