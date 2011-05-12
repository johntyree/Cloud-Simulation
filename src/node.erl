-module(node).
-compile(export_all). %% Probably replace with -export([funcs])
-author("John Tyree").

-include_lib("constants.hrl").
-include_lib("node.hrl").
-include_lib("drop.hrl").

init() ->
    Drops = [drop:new() || _ <- lists:seq( 1, trunc(?INITIAL_DENSITY *
                ?GRIDSIZE_X * ?GRIDSIZE_Y))],
    init(#nodestate{drops = Drops}).
init({X1, Y1, Z1, X2, Y2, Z2}, Drops, Parent) ->
    init(#nodestate{
            x1 = X1,
            y1 = Y1,
            z1 = Z1,
            x2 = X2,
            y2 = Y2,
            z2 = Z2,
            drops = Drops,
            parent = Parent}).
init(S = #nodestate{}) ->
    loop(S).

drop_loop(S = #nodestate{}) ->
    receive
        step ->
            Drops = move_drops(S#nodestate.drops),
            {Keep, Transfer} = filter_drops(Drops),
            S#nodestate.parent ! Transfer,
            drop_loop(S = #nodestate{drops = Keep});

        {newdrop, D = #dropstate{}} ->
            drop_loop(S = #nodestate{drops = [D|NewDrops]});

        merge ->
            S#nodestate.parent ! Drops,
            exit(merged)
    end.

filter_drops(Drops) -> filter_drops(Drops, [], []).
filter_drops([], Local, NonLocal) -> {Local, NonLocal};
filter_drops([D|Drops], Local, NonLocal) ->
    case is_local(D) of
        true -> filter_drops(Drops, [D|Local], NonLocal);
        false -> filter_drops(Drops, Local, [D|NonLocal])
    end.




            case handle_collision(S, get_state(Pid)) of
                coalesce ->
                    NewS = coalesce(S, Pid);
                true ->
                    NewS = S
            end,

%% Old and not implemented yet
%split(N = #state{
%init({Xoffset, Yoffset, Zoffset}, Drops, L, R, B, T, I, O) ->
        %size = Size,
        %offset = {Xoff, Yoff, Zoff},
        %drops = Drops,
        %neighbors = {Up, Down, Left, Right, In, Out}}) ->
    %{{LeftX, BottomY, InnerZ},
        %{RightX, TopY, OuterZ}} = divide_domain(Size),
    %LeftXoffset = Xoffset,
    %RightXoffset = LeftX + Xoffset,
    %BottomYoffset = Yoffset,
    %TopYoffset = BottomY + Yoffset,
    %InnerZoffset = Zoffset,
    %OuterZoffset = InnerZ + Zoffset
    %Neighbor = spawn(state, init, [
            %{NewSize,
            %NewOffset,
            %NewDrops,
            %NewNeighbors}])


divide_domain({Xsize, Ysize, Zsize}) ->
    {LeftX, RightX} = bisect(Xsize),
    {BottomY, TopY} = bisect(Ysize),
    {InnerZ, OuterZ} = bisect(Zsize),
    {{LeftX, BottomY, InnerZ}, {RightX, TopY, OuterZ}}.
