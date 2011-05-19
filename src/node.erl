-module(node).
-compile(export_all). %% Probably replace with -export([funcs])
-author("John Tyree").

-include_lib("constants.hrl").
-include_lib("node.hrl").
-include_lib("drop.hrl").

%initial_config() ->
    %error_logger:logfile({open, "log_node"}).

init() -> init(#nodestate{}).
init({X1, Y1, Z1, X2, Y2, Z2}, Drops, Parent) when X1 > X2, Y1 > Y2, Z1 > Z2 ->
    init(#nodestate{
            x1 = X1,
            y1 = Y1,
            z1 = Z1,
            x2 = X2,
            y2 = Y2,
            z2 = Z2,
            drops = Drops,
            parent = Parent
        }).
init(Parent) when is_pid(Parent) -> init(#nodestate{deity = Parent});
init(S = #nodestate{}) ->
    %initial_config(),
    {ok, F} = file:open("log", [write]),
    group_leader(F, self()),
    drop_loop(S).

drop_loop(S = #nodestate{}) ->
    %error_logger:info_report(io_lib:format("~p", [dict:to_list(S#nodestate.drops)])),
    io:format("~w~n", [dict:to_list(S#nodestate.drops)]),
    receive
        move ->
            Drops = move_drops(S#nodestate.drops),
            {Keep, Transfer} = filter_drops(S, Drops),
            if is_pid(S#nodestate.parent) ->
                    if Transfer =/= [] ->
                            S#nodestate.parent ! {route_drops, Transfer};
                        true -> ok
                    end,
                    S#nodestate.parent ! ok;
                true -> ok
            end,
            if is_pid(S#nodestate.deity) ->
                    S#nodestate.deity ! ok;
                true -> ok
            end,
            drop_loop(S#nodestate{drops = Keep});

        {newdrop, D} ->
            Drops = add_drop(D, S#nodestate.drops),
            drop_loop(S#nodestate{drops = Drops});

        merge ->
            if is_pid(S#nodestate.parent) ->
                    S#nodestate.parent ! S#nodestate.drops;
                true -> ok
            end,
            exit(merged);

        {die, Pid} ->
            %error_logger:info_report(io_lib:format("Final drop list:~n~p", [dict:to_list(S#nodestate.drops)])),
            io:format("Final drop list:~n~p", [dict:to_list(S#nodestate.drops)]),
            Pid ! {ok_im_dead, self()},
            ok;

        repopulate ->
            drop_loop(populate_domain(S));
        {repopulate, Density} ->
            drop_loop(populate_domain(S, Density));

        reload ->
            ?MODULE:drop_loop(S);

        {size, Pid} ->
            if is_pid(Pid) ->
                    S#nodestate.deity ! dict:size(S#nodestate.drops);
                true -> ok
            end,
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
    DropList = [{{X,Y,Z}, [drop:new()]} || X <- XRange, Y <- YRange, Z <- ZRange,
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
handle_collision(D, []) -> [D];
handle_collision(D, OldDrops) when is_list(OldDrops) ->
    % * io:format("Handle collision between ~p and ~p ", [D, OldDrops]),
    % * io:format("(~p and ~p)~n", [is_list(D), is_list(OldDrops)]),
    %% Phase 1, we just coalesce them and call it a day.
    [lists:foldl(fun drop:coalesce/2, D, OldDrops)].

%% Attempt to add a list of new drops to the dict.
%% if drops already present at the locations, handle the collisions.
%% Return new dict of drops
%% [{Coord, [Drop]}] -> DropDict -> DropDict
add_drops([], DropDict) -> DropDict;
add_drops([{_Coord, []}|OtherDs], DropDict) -> add_drops(OtherDs, DropDict);
add_drops([{Coord, [D|Ds]}|OtherDs], DropDict) ->
    NewDrops = [{Coord, Ds}|OtherDs],
    add_drops(NewDrops, add_drop({Coord, D}, DropDict));
add_drops([{Coord, D}|OtherDs], DropDict) ->
    add_drops(OtherDs, add_drop({Coord, D}, DropDict)).

%% Attempt to add the new drop to the dict.
%% if drops already present at that location handle the collision.
%% Return new dict of drops
%% {Coord, Drop} -> DropDict -> DropDict
add_drop({Coord, NewDrop}, DropDict) when not is_list(NewDrop) ->
    case dict:find(Coord, DropDict) of
        {ok, DropList} ->
            NewDropList = handle_collision(NewDrop, DropList),
            % * io:format("Replacing ~p with ~p~n", [DropList, NewDropList]),
            dict:store(Coord, NewDropList, DropDict);
        error ->
            % * io:format("Storing ~p~n", [[NewDrop]]),
            dict:store(Coord, [NewDrop], DropDict)
    end.


%% Move all drops to their new locations.
%% DropDict -> DropDict
-spec move_drops(dict()) -> dict().
-spec move_drops([], any(), dict()) -> dict()
    ; ([{integer(), integer(), integer()},...], dict(), dict()) -> dict().
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
    % * io:format("Parent Undefined~n"),
    {Local, NonLocal} = filter_drops(S, dict:to_list(Drops), [], []),
    % * io:format("Filtered Drops:~n"),
    % * io:format("~p~n", [{Local, NonLocal}]),
    % * io:format("Periodicisin' ~p~n", [{Local, NonLocal}]),
    Localized = periodicise_drops(S, NonLocal),
    % * io:format("Periodicised:~n~p~n", [Localized]),
    % * io:format("Adding to keepers~n~p~n~p~n", [dict:to_list(Localized), Local]),
    {add_drops(dict:to_list(Localized), Local), []};
filter_drops(S = #nodestate{}, Drops) ->
    % * io:format("Got drop dict, making list~n"),
    filter_drops(S, dict:to_list(Drops), [], []).
filter_drops(_S, [], Local, NonLocal) ->
    % * io:format("List empty~n"),
    {dict:from_list(Local), dict:from_list(NonLocal)};
filter_drops(S, [D|Drops], Local, NonLocal) ->
    % * io:format("Testing Drop:"),
    case is_local(S, D) of
        true ->
            % * io:format("true ~p~n", [D]),
            filter_drops(S, Drops, [D|Local], NonLocal);
        false ->
            % * io:format("false ~p~n", [D]),
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
    % * io:format("List of drops at coord ~p:~n~p~n", [C, DropList]),
    %% List of {NewCoord, Drop} to be added
    MovedDropList = lists:map(fun(D) -> periodicise_drop(S, {C, D}) end, DropList),
    % * io:format("List of {NewCoord, Drop} to be added:~n~p~n", [MovedDropList]),
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
