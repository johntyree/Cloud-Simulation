-module(drop).
-compile(export_all). %% Probably replace with -export([funcs])
-author("John Tyree").

-record(state,
    {server,
        position,
        size}).

Timeout = 5000.

random_direction() -> {random:uniform(3)-2, random:uniform(3)-2}.

% Change the actual position, taking boundary conditions into account
migrate({X, Y}, {DX, DY}) ->
    NewX = migrate(X, DX, constants:gridsize_X()),
    NewY = migrate(Y, DY, constants:gridsize_Y()),
    {NewX, NewY}.
migrate(New, Delta, Max) when New + Delta >= Max -> New + Delta - Max;
migrate(New, Delta, Max) when New + Delta < 0 -> New + Delta + Max;

move(S#state) ->
    Timeout = constants:timeout(),
    NewPosition = migrate(S#state.position, random_direction()),
    server ! {self(), move, NewPosition},
    receive
        {From, ok} ->
            loop(S#state{
                    server = From,
                    position = NewPosition
                });
        {From, collision, Pid} ->
            Pid ! {self(), die}
            receive
                {#state{size = Size}} ->
                    NewSize = S#state.size + Size;
        {From, die} ->
            From ! S,
            exit(coalesced)
    after Timeout -> % What's taking so long? Print error and die.
            io:format("~p: Dead after ~p seconds.", [self(), Timeout])
            exit(timeout)
    end.

