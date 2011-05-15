-module(node).
-compile(export_all). %% Probably replace with -export([funcs])
-author("John Tyree").

-include_lib("constants.hrl").
-include_lib("node.hrl").
-include_lib("drop.hrl").

initial_config() ->
    error_logger:logfile({open, "log"}).

init() -> init(#nodestate{}).
init(Parent) when is_pid(Parent) -> init(#nodestate{parent = Parent});
init(S = #nodestate{}) ->
    initial_config(),
    drop_loop(S).
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

drop_loop(S = #nodestate{}) ->
    error_logger:info_report(S),
    flush(),
    receive
        move ->
            error_logger:info_report("Movin"),
            Drops = move_drops(S#nodestate.drops),
            error_logger:info_report("Moved: ~p~n~n Filterin'", [Drops]),
            {Keep, Transfer} = filter_drops(S, Drops),
            error_logger:info_report("Filtered:~n~p~p~n", [Keep, Transfer]),
            S#nodestate.parent ! Transfer,
            error_logger:info_report("Recurse with Keepers"),
            drop_loop(S#nodestate{drops = Keep});

        {newdrop, D} ->
            Drops = add_drop(D, S#nodestate.drops),
            drop_loop(S#nodestate{drops = Drops});

        merge ->
            S#nodestate.parent ! S#nodestate.drops,
            exit(merged);

        repopulate ->
            drop_loop(populate_domain(S));
        {repopulate, Density} ->
            drop_loop(populate_domain(S, Density));

        reload ->
            ?MODULE:drop_loop(S);

        print ->
            drop_loop(S)
    end.

%% Return nodestate with ~ AREA * INITIAL_DENSITY drops
%% nodestate -> nodestate
populate_domain(S = #nodestate{}) -> populate_domain(S, ?INITIAL_DENSITY).
populate_domain(S = #nodestate{}, Density) ->
    XRange = lists:seq(S#nodestate.x1, S#nodestate.x2),
    YRange = lists:seq(S#nodestate.y1, S#nodestate.y2),
    ZRange = lists:seq(S#nodestate.z1, S#nodestate.z2),
    %l All permutations means all grid points in the domain in this case.
    %% Should create AREA * INITIAL_DENSITY drops on average
    DropList = [{{X,Y,Z}, drop:new()} || X <- XRange, Y <- YRange, Z <- ZRange,
        Density > random_uniform_nonboundary(0, 1)],
    S#nodestate{drops = add_drops(DropList, dict:new())}.

%% Return nodestate with one drop added, possibly on top of another one
create_drop(S = #nodestate{}) ->
    NewDrops = add_drop({new_position(S), drop:new()}, S#nodestate.drops),
    S#nodestate{drops = NewDrops}.
new_position(#nodestate{ x1 = X1, x2 = X2, y1 = Y1, y2 = Y2, z1 = Z1, z2 =
        Z2}) ->
    {random_int(X1, X2), random_int(Y1, Y2), random_int(Z1, Z2)}.

%% When two drops meet, we do something.
%% dropstate -> [dropstate] -> [dropstate]
%% Return new list of drops
handle_collision(D, []) -> D;
handle_collision(D, OldDrops) when is_list(OldDrops) ->
    error_logger:info_report("Handle collision between ~p and ~p", [D,
            OldDrops]),
    io:format("Handle collision between ~p and ~p", [D,
            OldDrops]),
    io:format("~p and ~p", [is_list(D),
            is_list(OldDrops)]),
    %% Phase 1, we just coalesce them and call it a day.
    [lists:foldl(fun drop:coalesce/2, D, OldDrops)].

%% Attempt to add the new drop to the dict, if drops already present at
%% that location handle the collision.
%% Return new dict of drops
%% [Drop] -> DropDict -> DropDict
add_drops([], Drops) -> Drops;
add_drops([N|NewDrops], Drops) ->
    io:format("add_drops([~p|~p], ~p)..", [N, NewDrops, Drops]),
    add_drops(NewDrops, add_drop(N, Drops)).
add_drop({Coord, NewDrop}, Drops) ->
    case dict:find(Coord, Drops) of
        {ok, DropList} ->
            NewDropList = handle_collision(NewDrop, DropList),
            dict:store(Coord, NewDropList, Drops);
        error ->
            dict:store(Coord, [NewDrop], Drops)
    end.


%% Move all drops to their new locations.
%% DropDict -> DropDict
move_drops(Drops) ->
    Coords = dict:fetch_keys(Drops),
    move_drops(Coords, Drops, dict:new()).
move_drops([], _, NewDrops) -> NewDrops;
move_drops([C|Coords], OldDrops, NewDrops) ->
    %% List of drops at coord C.
    DropList = dict:fetch(C, OldDrops),
    %% List of {NewCoord, Drop} to be added
    MovedDropList = lists:map(fun(D) -> drop:move({C, D}) end, DropList),
    %% Cumulative dict of new drops
    NewDropDict = add_drops(MovedDropList, NewDrops),
    move_drops(Coords, OldDrops, NewDropDict).


%% Filter out the drops that have left our domain.
%% nodestate -> DropDict -> {Local DropDict, Nonlocal DropDict}
%% If the parent is undefined, boundaries become periodic in all directions for now.
filter_drops(S = #nodestate{parent = P}, Drops) when P =:= undefined ->
    io:format("Parent Undefined~n"),
    {Local, NonLocal} = filter_drops(S, dict:to_list(Drops), [], []),
    io:format("Filtered Drops:~n"),
    io:format("~p~n", [{Local, NonLocal}]),
    io:format("Periodicisin' ~p~n", [{Local, NonLocal}]),
    Localized = periodicise_drops(S, NonLocal),
    io:format("Periodicised:~n~p~n", [Localized]),
    io:format("Adding to keepers~n"),
    {add_drops(dict:to_list(Localized), Local), []};
filter_drops(S = #nodestate{}, Drops) ->
    io:format("Got drop dict, making list~n"),
    filter_drops(S, dict:to_list(Drops), [], []).
filter_drops(_S, [], Local, NonLocal) ->
    io:format("List empty~n"),
    {dict:from_list(Local), dict:from_list(NonLocal)};
filter_drops(S, [D|Drops], Local, NonLocal) ->
    io:format("Testing Drop:"),
    case is_local(S, D) of
        true ->
            io:format("true ~p~n", [D]),
            filter_drops(S, Drops, [D|Local], NonLocal);
        false ->
            io:format("false ~p~n", [D]),
            filter_drops(S, Drops, Local, [D|NonLocal])
    end.


%% nodestate -> DropDict -> DropDict
%% TODO: Could be made faster by filtering the dict for local coords first.
periodicise_drops(S, NewDrops) ->
    Coords = dict:fetch_keys(NewDrops),
    periodicise_drops(S, Coords, NewDrops, dict:new()).
periodicise_drops(_S, [], _, NewDrops) -> NewDrops;
periodicise_drops(S, [C|Coords], OldDrops, NewDrops) ->
    %% List of drops at coord C.
    DropList = dict:fetch(C, OldDrops),
    %% List of {NewCoord, Drop} to be added
    MovedDropList = lists:map(fun(D) -> periodicise_drop(S, {C, D}) end, DropList),
    %% Cumulative dict of new drops
    NewDropDict = add_drops(MovedDropList, NewDrops),
    periodicise_drops(S, Coords, OldDrops, NewDropDict).
periodicise_drop(
    #nodestate{ x1 = X1, x2 = X2, y1 = Y1, y2 = Y2, z1 = Z1, z2 = Z2},
    {{X, Y, Z}, Drop}) ->
    NewX = make_periodic(X, X1, X2),
    NewY = make_periodic(Y, Y1, Y2),
    NewZ = make_periodic(Z, Z1, Z2),
    {{NewX, NewY, NewZ}, Drop}.


%% Return true if the drop D is in domain of S, else false.
%% nodestate -> {{Coords}, Drop} -> Bool
is_local(
    #nodestate{ x1 = X1, x2 = X2, y1 = Y1, y2 = Y2, z1 = Z1, z2 = Z2},
    {{X, Y, Z}, _Drop}) ->
    not ((X < X1) or (Y < Y1) or (Z < Z1)
        or (X > X2) or (Y > Y2) or (Z > Z2)).

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
