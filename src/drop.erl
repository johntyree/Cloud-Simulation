-module(drop).
-compile(export_all). %% Probably replace with -export([funcs])
-author("John Tyree").

-include_lib("constants.hrl").

%% Make a new drop
init(Server, Round) ->
    Position = new_position(),
    Size = new_size(),
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
            io:format("~p: Dead after ~p seconds.", [self(), ?TIMEOUT/1000]),
            exit(timeout)
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
            io:format("~p: No response from Drop ~p.", [self(),
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
random_direction() -> {random:uniform(3)-2, random:uniform(3)-2}.

%%  Stochastic decision making
% when two drops are at the same site
handle_collision(_Us, _Them) ->
    ok.
% Location of a new drop
new_position() ->
    {random:uniform(30)-1, random:uniform(30)-1}.
% Size of a new drop ->
new_size() ->
    ok.

% Retrieve the state of a drop
get_state(Pid) ->
    ?TIMEOUT = constants:timeout(),
    Ref = make_ref(),
    Pid ! {send_state, Ref, self()},
    receive
        {send_state, Ref, S} -> S
    after ?TIMEOUT ->
            io:format("~p: No response from Drop ~p.", [self(), Pid]),
            exit(timeout)
    end.
