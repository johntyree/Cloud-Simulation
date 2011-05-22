-module(drop).
-compile(export_all). %% Probably replace with -export([funcs])
-author("John Tyree").

-include_lib("constants.hrl").
-include_lib("drop.hrl").

%% Make a new drop
new() -> new(new_size()).
new(Size) when is_number(Size), Size >= 0 -> #dropstate{size = Size}.

coalesce(S1 = #dropstate{}, S2 = #dropstate{}) ->
    NewSize = radius(volume(S1#dropstate.size) + volume(S2#dropstate.size)),
    S1#dropstate{size = NewSize}.

%% Take any size drop, return a list of either one or two drops depending on
%% size and probability to split.
% dropstate -> [dropstate]
split(S = #dropstate{size = Size}) ->
    case it_splits(Size) of
        true ->
            NewSize = radius(volume(Size) / 2),
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

%% Mathematica's FindFit to give model params for drag equation of a sphere.
%% http://en.wikipedia.org/wiki/Terminal_velocity
%% Density of rain = 1000 kg/m³
%% Density of air  = 1.025 kg/m³ at 6000ft.
terminal_velocity(#dropstate{size = Size}) -> terminal_velocity(Size);
terminal_velocity(Size) when Size > (?HALF_SPLIT_SIZE / 29) ->
    (-100 + 618.051 * math:pow(Size - (?HALF_SPLIT_SIZE /
                60), 0.5)) * 100.

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
    %X = gaussian(0.025, 0.006),
    X = gaussian(0.25, 0.01),
    if X =< 0 ->
            new_size();
        true ->
            X
    end.

