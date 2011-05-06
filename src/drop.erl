-module(drop).
-compile(export_all). %% Probably replace with -export([funcs])
-author("John Tyree").

-include_lib("constants.hrl").

%% Make a new drop
init(Server, Round) ->
    Position = new_position(),
    Size = new_size(),
    Server ! {newdrop, self(), {Position, Size}},
    loop(#state{server = Server,
            position = Position,
            size = Size}, Round).

loop(S = #state{}, Round) ->
    receive
        {ok, NextRound} ->
            if NextRound =/= Round ->
                    NewS = move(S);
                true ->
                    NewS = S
            end,
            loop(NewS, NextRound);
        {collision, {_From, Pid}} ->
            case handle_collision(S, get_state(Pid)) of
                coalesce ->
                    NewS = coalesce(S, Pid);
                true ->
                    NewS = S
            end,
            loop(NewS, Round);
        {die, From, Reason} ->
            From ! {Reason, S},
            exit(Reason)
    after ?TIMEOUT -> % What's taking so long? Print error and die.
            error_logger:error_msg("~p: Dead after ~p seconds.", [self(), ?TIMEOUT/1000])
            % TODO
            %exit(timeout)
    end.

move(S = #state{}) ->
    NewPosition = migrate(S#state.position, random_direction()),
    Ref = make_ref(),
    S#state.server ! {move, Ref, {self(), NewPosition}},
    S#state{position = NewPosition}.

coalesce(S = #state{}, Pid) ->
    ?TIMEOUT = constants:timeout(),
    Pid ! {die, self(), coalesced},
    receive
        {coalesced, #state{size = Size}} ->
            NewSize = S#state.size + Size,
            S#state{size = NewSize}
    after ?TIMEOUT ->
            error_logger:error_msg("~p: No response from Drop ~p.", [self(),
                    Pid]),
            exit(timeout)
    end.

%split(S = #state{}) ->
    %?TIMEOUT = constants:timeout(),
    %NewSize = S#state.size / 2,
    %NewDrop = spawn(move, S#state{
            %size = NewSize
        %}),
    %loop(S#state{
            %size = NewSize
        %}).

%% Private Functions

% Change the actual position, taking boundary conditions into account
migrate({X, Y}, {DX, DY}) ->
    NewX = migrate(X, DX, ?GRIDSIZE_X),
    NewY = migrate(Y, DY, ?GRIDSIZE_Y),
    {NewX, NewY}.
migrate(New, Delta, Max) when New + Delta >= Max -> New + Delta - Max;
migrate(New, Delta, Max) when New + Delta < 0 -> New + Delta + Max;
migrate(New, Delta, _Max) -> New + Delta.

% Chose a random x and y movement from -1,0,1
random_direction() -> {random_int(-1, 1), random_int(-1, 1)}.

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
