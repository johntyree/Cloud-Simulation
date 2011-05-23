-module(drop).
-compile(export_all). %% Probably replace with -export([funcs])
-author("John Tyree").

-include_lib("constants.hrl").
-include_lib("drop.hrl").

%% Make a new drop
new() -> new(new_size()).
new(Size) when is_number(Size), Size >= 0 -> #dropstate{size = Size}.

coalesce(S1 = #dropstate{size = Size}, #dropstate{size = Size2}) when
is_float(Size), is_float(Size2) ->
    NewSize = radius(volume(Size) + volume(Size2)),
    S1#dropstate{size = NewSize}.

coalesce_test(S1 = #dropstate{}, S2 = #dropstate{}) ->
    case {S1#dropstate.size, S2#dropstate.size} of
        {A, B} when (A > 2.5) or (B > 2.5) -> false;
        {A, B} when (A < 0.4) or (B < 0.4) -> true;
        {A, B} when (A > 1) or (B > 1) -> (random_uniform_nonboundary(0,1)
                > 0.2);
        {_A, _B} -> true
end.

%% Take any size drop, return a list of either one or two drops depending on
%% size and probability to split.
% dropstate -> [dropstate]
split(S = #dropstate{size = Size}) when is_float(Size) ->
    case it_splits(Size) of
        true ->
            NewSize = radius(volume(Size) / 2),
            [S#dropstate{size = NewSize}, S#dropstate{size = NewSize}];
        false ->
            [S#dropstate{size = Size}]
    end.

%% Return true if the drop splits, based on stochastically based on size.
%% int -> bool
it_splits(Size) when is_float(Size), Size =< ?HALF_SPLIT_SIZE - 1 -> false;
it_splits(Size) when is_float(Size), Size >= 0 ->
    1 - (1 / (1 + math:pow(1 - ?HALF_SPLIT_SIZE + Size, ?SPLIT_STEEPNESS)))
    > random_uniform_nonboundary(0,1).

%% Mathematica's FindFit to give model params for drag equation of a sphere.
%% http://en.wikipedia.org/wiki/Terminal_velocity
%% Density of rain = 1000 kg/m³
%% Density of air  = 1.025 kg/m³ at 6000ft.
%% < 0.018mm > 0 mm/s (all movement due to wind)
%% 0.05mm -> 70 mm/s
%% 0.1mm -> 700 mm/s
%% 1mm -> 5500 mm/s
%% MILLIMETERS PER SECOND, FOLKS
terminal_velocity(#dropstate{size = Size}) when is_float(Size) -> terminal_velocity(Size);
terminal_velocity(Size) when is_float(Size), Size > (?HALF_SPLIT_SIZE / 27) ->
    (-100 + 618.051 * math:pow(Size - (?HALF_SPLIT_SIZE / 60), 0.5)) * 10;
terminal_velocity(Size) when is_float(Size), Size > 0.04 -> 1298.16 * math:pow((-0.0258 + 10
            * Size), 8) * 10;
terminal_velocity(_) -> 0.

%%%%%%%%%%%%%%%%%%%%%%%
%%                   %%
%% Private Functions %%
%%                   %%
%%%%%%%%%%%%%%%%%%%%%%%


%% Size of a new drop
%% http://ga.water.usgs.gov/edu/raindropsizes.html
%% Range 0.001mm, 0.05mm. "Rain" at 0.5mm.
%% Changing this to have larger drop sizes initially. Otherwise we don't get
%% there without ENORMOUS domains
new_size() ->
    X = gaussian(0.035, 0.004),
    %X = gaussian(0.05, 0.01),
    if X =< 0 ->
            new_size();
        true ->
            X
    end.

