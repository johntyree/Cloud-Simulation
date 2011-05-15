-module(node).
-compile(export_all). %% Probably replace with -export([funcs])
-author("John Tyree").

-include_lib("constants.hrl").
-include_lib("node.hrl").
-include_lib("drop.hrl").

populate_domain(S = #nodestate{}) -> populate_domain(S, ?INITIAL_DENSITY).
populate_domain(S = #nodestate{}, Density) ->
    XRange = lists:seq(S#nodestate.x1, S#nodestate.x2),
    YRange = lists:seq(S#nodestate.y1, S#nodestate.y2),
    ZRange = lists:seq(S#nodestate.z1, S#nodestate.z2),
    %% All permutations means all grid points in the domain in this case.
    %% Should create AREA * INITIAL_DENSITY drops on average
    dict:from_list(
        [{{X,Y,Z}, drop:new()} || X <- XRange, Y <- YRange, Z <- ZRange,
            Density > random_uniform_nonboundary(0, 1)]).

%% Return state with one drop added, possibly on top of another one
create_drop(S = #nodestate{}) ->
    NewDrops = add_drop({new_position(S), drop:new()}, S#nodestate.drops),
    S#nodestate{drops = NewDrops}.
new_position(#nodestate{ x1 = X1, x2 = X2, y1 = Y1, y2 = Y2, z1 = Z1, z2 =
        Z2}) ->
    {random_int(X1, X2), random_int(Y1, Y2), random_int(Z1, Z2)}.

init() ->
    S = #nodestate{},
    init(S#nodestate{drops = populate_domain(S)}).
init({X1, Y1, Z1, X2, Y2, Z2}, Drops, Parent) when X1 > X2, Y1 > Y2, Z1 > Z2 ->
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
    drop_loop(S).

drop_loop(S = #nodestate{}) ->
    receive
        move ->
            Drops = move_drops(S#nodestate.drops),
            {Keep, Transfer} = filter_drops(S, Drops),
            S#nodestate.parent ! Transfer,
            %drop_loop(S = #nodestate{drops = Keep});
            #nodestate{drops = Keep};

        {newdrop, D = #dropstate{}} ->
            Drops = add_drop(S#nodestate.drops, D),
            %drop_loop(S = #nodestate{drops = Drops});
            #nodestate{drops = Drops};

        merge ->
            S#nodestate.parent ! S#nodestate.drops,
            exit(merged);

        db ->
            S#nodestate.parent ! S
    end.

%% When two drops meet, we do something.
%% Return new dict of drops
handle_collision(D, OldDrops) -> handle_collision(D, OldDrops, []).
handle_collision(D, [], Acc) -> [D|Acc];
handle_collision(D, [OldDrop|OldDrops], NewDrops) ->
    %% Phase 1, we just coalesce them and call it a day.
    NewDrop = drop:coalesce(D, OldDrop),
    handle_collision(NewDrop, OldDrops, NewDrops).

%% Attempt to add the new drop to the dict, if drops already present at
%% that location handle the collision.
%% Return new dict of drops
add_drop(Drops, [N|NewDrops]) ->
    add_drop(add_drop(Drops, N), NewDrops);
add_drop(Drops, NewDrop) ->
    NewDrops = dict:append(drop:to_key(NewDrop), fun handle_collision/2,
        NewDrop, Drops),
    NewDrops.

%% Move all drops to their new locations.
%% Return new dict of drops
move_drops(Drops) -> dict:map(fun(P, D) -> drop:move({P, D}) end, Drops).

%% Filter out the drops that have left our domain.
%% Return {OurDrops, NotOurDrops}
%% If the parent is undefined, boundaries become periodic in all directions for now.
filter_drops(S = #nodestate{parent = P}, Drops) when P =:= undefined ->
    {Local, NonLocal} = filter_drops(S, Drops, [], []),
    Localized = periodicise_drops(S, NonLocal),
    {add_drop(Local, Localized), []};
filter_drops(S = #nodestate{}, Drops) -> filter_drops(S, Drops, [], []).
filter_drops(_S, [], Local, NonLocal) -> {Local, NonLocal};
filter_drops(S, [D|Drops], Local, NonLocal) ->
    case is_local(S, D) of
        true -> filter_drops(S, Drops, [D|Local], NonLocal);
        false -> filter_drops(S, Drops, Local, [D|NonLocal])
    end.

periodicise_drops(S, Drops) ->
    dict:map(fun(D) -> periodicise_drop(S, D) end, Drops).
periodicise_drop(
    #nodestate{ x1 = X1, x2 = X2, y1 = Y1, y2 = Y2, z1 = Z1, z2 = Z2},
    {{X, Y, Z}, Drop}) ->
    NewX = make_periodic(X, X1, X2),
    NewY = make_periodic(Y, Y1, Y2),
    NewZ = make_periodic(Z, Z1, Z2),
    {{NewX, NewY, NewZ}, Drop}.


%% Return true if the drop D is in domain of S, else false.
is_local(
    #nodestate{ x1 = X1, x2 = X2, y1 = Y1, y2 = Y2, z1 = Z1, z2 = Z2},
    {{X, Y, Z}, _Drop}) ->
    (X < X1) or (Y < Y1) or (Z < Z1)
    or (X > X2) or (Y > Y2) or (Z > Z2).

%% Split our domain into 2^dimentions new domains by spawning new drop nodes.
%% This process becomes a router node with the new nodes as children.
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

%% Split our domain into eight pieces
divide_domain({Xsize, Ysize, Zsize}) ->
    {LeftX, RightX} = bisect(Xsize),
    {BottomY, TopY} = bisect(Ysize),
    {InnerZ, OuterZ} = bisect(Zsize),
    {{LeftX, BottomY, InnerZ}, {RightX, TopY, OuterZ}}.
