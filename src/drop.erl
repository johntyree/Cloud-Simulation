-module(drop).
-compile(export_all). %% Probably replace with -export([funcs])
-author("John Tyree").

-include_lib("constants.hrl").
-include_lib("drop.hrl").

%% Make a new drop
new() ->
    Position = new_position(),
    Size = new_size(),
    #dropstate{position = Position,
        size = Size}, Round).


coalesce(S1 = #dropstate{}, S2 = #dropstate{}) ->
    NewSize = S1#dropstate.size + S2#dropstate.size,
    S1#dropstate{size = NewSize}.

split(S = #dropstate{}) ->
    NewSize = S#dropstate.size / 2,
    {S#state{size = NewSize}, S#state{size = NewSize}}.

move(S = #dropstate{}) ->
    NewPosition = migrate(S#dropstate.position, random_direction()),
    S#dropstate{position = NewPosition}.
    {S1, NewX} = migrate(X, DX, ?GRIDSIZE_X),
    {S2, NewY} = migrate(Y, DY, ?GRIDSIZE_Y),
    {S3, NewZ} = migrate(Z, DZ, ?GRIDSIZE_Z),
    case lists:all(fun({S,_}) -> S =:= ok end, [NewX, NewY, NewZ]) of
        true -> {ok, {NewX, NewY, NewZ}};
        false -> {out_of_bounds, {NewX, NewY, NewZ}}
    end.





%%%%%%%%%%%%%%%%%%%%%%%
%%                   %%
%% Private Functions %%
%%                   %%
%%%%%%%%%%%%%%%%%%%%%%%

% Change the actual position, taking boundary conditions into account
%ret: {status, {coords}}
migrate({X, Y, Z}, {DX, DY, DZ}) ->
    NewX = migrate(X, DX),
    NewY = migrate(Y, DY),
    NewZ = migrate(Z, DZ),
    {NewX, NewY, NewZ}.
migrate(New, Delta) -> New + Delta.

% Chose a random x and y movement from -1,0,1
random_direction() -> random_direction(1).
random_direction(Step) -> {random_int(-Step, Step), random_int(-Step, Step)}.

%%  Stochastic decision making
% when two drops are at the same site
handle_collision(_Us, _Them) ->
    ok.
% Location of a new drop
new_position() ->
    {random_int(?GRIDSIZE_X - 1), random_int(?GRIDSIZE_Y - 1)}.
% Size of a new drop ->
% http://ga.water.usgs.gov/edu/raindropsizes.html
% Range 0.001mm, 0.05mm. "Rain" at 0.5mm.
new_size() ->
    X = gaussian(0.025, 0.006),
    if X =< 0 ->
            new_size();
        true ->
            X
    end.

% Retrieve the state of a drop
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
