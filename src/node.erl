-module(node).
-compile(export_all). %% Probably replace with -export([funcs])
-author("John Tyree").

-include_lib("constants.hrl").
-include_lib("node.hrl").
-include_lib("drop.hrl").

%% Set up environment, seed rng.
%% ok
initial_config() ->
    %{ok, F} = file:open("log", [write]),
    %group_leader(F, self()),
    %error_logger:logfile({open, "log_node"}).
    <<A:32,B:32,C:32>> = crypto:rand_bytes(12),
    random:seed(A, B, C),
    ok.

%% Create a 10x10x0 domain for testing.
%% Does not return immediately.
%% nodestate
mini() ->
    X1 = 0, Y1 = 0, Z1 = 0, X2 = 10, Y2 = 10, Z2 = 1,
    put(domain_range, {X1, Y1, Z1, X2, Y2, Z2}),
    #nodestate{
            x1 = X1,
            y1 = Y1,
            z1 = Z1,
            x2 = X2,
            y2 = Y2,
            z2 = Z2
        }.

%% Create a nodestate with default values
%% Does not return immediately.
%% nodestate
init() -> init(#nodestate{}).
%% Create a nodestate from submitted values
%% Does not return immediately.
%% nodestate
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
%% Create a nodestate with deity set to Parent, rest default.
%% Pid -> nodestate
init(Parent) when is_pid(Parent) -> init(#nodestate{deity = Parent});
%% Start a node loop at nodestate S.
%% Does not return immediately.
%% nodestate -> ok
init(S = #nodestate{x1 = X1, y1 = Y1, z1 = Z1, x2 = X2, y2 = Y2, z2 = Z2}) ->
    initial_config(),
    put(x1, X1),
    put(y1, Y1),
    put(z1, Z1),
    put(x2, X2),
    put(y2, Y2),
    put(z2, Z2),
    put(domain_range, {X1, Y1, Z1, X2, Y2, Z2}),
    %fprof:apply(fun drop_loop/1, [S]).
    drop_loop(S).

%% TODO: split these out into functions
%% Run drop management loop for nodestate S.
%% Does not return immediately.
%% nodestate -> ok
drop_loop(S = #nodestate{}) ->
    receive
        %% Move the drops, filter the out of bounds ones and send them up to
        %% parent
        move ->
            Gust = case random:uniform() of
                X when X > 0.98 -> 2.0;
                X -> X * 1
            end,
            put(gust, Gust),
            Drops = move_drops(S#nodestate.drops),
            %io:format("Moved Drops.~n"),
            {Keep, Transfer} = filter_drops(S, Drops),
            %io:format("Filtered Drops.~n"),
            if is_pid(S#nodestate.parent) ->
                    if Transfer =/= [] ->
                            S#nodestate.parent ! {route_drops, Transfer};
                        true -> ok
                    end,
                    S#nodestate.parent ! ok;
                true -> ok
            end,
            if is_pid(S#nodestate.deity) ->
                    if Transfer =/= [] ->
                            S#nodestate.deity ! {route_drops, Transfer},
                            %io:format("Sent drops to deity.~n"),
                            ok;
                        true -> S#nodestate.deity ! ok
                    end;
                true -> ok
            end,
            NewState = spawn_new_drops(S#nodestate{drops = Keep}),
            io:format("~w~n", [NewState#nodestate.drops]),
            drop_loop(NewState);

        {new_drop, D} ->
            %io:format("Got drop!~n"),
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
            %io:format("Final drop list:~n~p", [dict:to_list(S#nodestate.drops)]),
            Pid ! {ok_im_dead, self()},
            exit(died);

        repopulate ->
            NewState = populate_domain(S),
            drop_loop(NewState);
        {repopulate, Density} ->
            NewState = populate_domain(S, Density),
            drop_loop(NewState);

        info ->
            if is_pid(S#nodestate.parent) ->
                    S#nodestate.parent ! info(S);
                true -> ok
            end,
            if is_pid(S#nodestate.deity) ->
                    S#nodestate.deity ! info(S);
                true -> ok
            end,
            drop_loop(S);

        reload ->
            ?MODULE:drop_loop(S);

        {size, Pid} ->
            if is_pid(Pid) ->
                    S#nodestate.deity ! dict:size(S#nodestate.drops);
                true -> ok
            end,
            drop_loop(S)
    end.

%% Return nodestate with AREA * RELATIVE_HUMIDITY drops replacing all
%% previous drops.
%% nodestate -> nodestate
populate_domain(S = #nodestate{}) -> populate_domain(S, ?RELATIVE_HUMIDITY).
populate_domain(S = #nodestate{}, Density) ->
    spawn_new_drops(S#nodestate{drops = dict:new()}, Density).

%% Return metadata about nodestate S
%% nodestate -> nodeinfo
info(S = #nodestate{}) ->
    #nodeinfo{
        volume = water_volume(S),
        size = dict:size(S#nodestate.drops)
    }.

%% Return cumulative water volume contained in nodestate.
%% nodestate -> float
water_volume(#nodestate{drops = Dropdict}) ->
    %Coords = dict:fetch_keys(Dropdict),
    Drop = dict:fold(fun(_, Ds, D) -> lists:foldl(fun drop:coalesce/2, D,
                    Ds) end, drop:new(0.0), Dropdict),
    drop:volume(Drop#dropstate.size).

%% Return number of coordinates (area) of nodestate.
%% nodestate -> integer
spatial_volume(S = #nodestate{}) ->
    (S#nodestate.x2 - S#nodestate.x1 + 1) *
    (S#nodestate.y2 - S#nodestate.y1 + 1) *
    (S#nodestate.z2 - S#nodestate.z1 + 1).

%% This version is faster for large domains...
%% Create new drops in the domain according to RELATIVE_HUMIDITY.
%% Return nodestate with area * relative_humidity drops added
%% nodestate -> nodestate
spawn_new_drops(S = #nodestate{}) ->
    spawn_new_drops(S, ?RELATIVE_HUMIDITY * 0.1).
%% nodestate -> Density -> nodestate
spawn_new_drops(S = #nodestate{}, Density) when is_float(Density) ->
    DropList = lists:map(fun(_) -> {new_position(S), drop:new()} end, lists:seq(1, round(Density *
                spatial_volume(S)))),
    S#nodestate{drops = add_drops(DropList, S#nodestate.drops)}.

%% Return nodestate with area * relative_humidity drops added
%% nodestate -> nodestate
xspawn_new_drops(S = #nodestate{}) ->
    spawn_new_drops(S, ?RELATIVE_HUMIDITY * 0.1).
xspawn_new_drops(S = #nodestate{}, Density) ->
    XRange = lists:seq(S#nodestate.x1, S#nodestate.x2),
    YRange = lists:seq(S#nodestate.y1, S#nodestate.y2),
    ZRange = lists:seq(S#nodestate.z1, S#nodestate.z2),
    % All permutations means all grid points in the domain in this case.
    %% Should create AREA * RELATIVE_HUMIDITY drops on average
    DropList = [{{X,Y,Z}, [drop:new()]} || X <- XRange, Y <- YRange, Z <- ZRange,
        Density > random_uniform_nonboundary(0, 1)],
    %io:format("Adding ~p new drops~n",[length(DropList)]),
    S#nodestate{drops = add_drops(DropList, S#nodestate.drops)}.

%% Add a single, randomly placed drop to the domain
%% nodestate -> nodestate
create_drop(S = #nodestate{}) ->
    NewDrops = add_drop({new_position(S), drop:new()}, S#nodestate.drops),
    S#nodestate{drops = NewDrops}.
new_position(S = #nodestate{ x1 = X1, x2 = X2, y1 = Y1, y2 = Y2, z1 = Z1, z2 =
        Z2}) when is_integer(X1), is_integer(X2), is_integer(Y1), is_integer(Y2),
is_integer(Z1), is_integer(Z2) ->
    R = random_int(spatial_volume(S)-1),
    XRange = X2 - X1 + 1,
    YRange = Y2 - Y1 + 1,
    ZRange = Z2 - Z1 + 1,
    X = (R rem XRange) + X1,
    Y = (trunc(R / XRange) rem YRange) + Y1,
    Z = (trunc(R / (XRange * YRange)) rem ZRange) + Z1,
    {X, Y, Z}.

%% When two drops meet, we do something.
%% dropstate -> [dropstate] -> [dropstate]
%% Return new list of drops
%% dropstate -> [dropstate] -> [dropstate]
handle_collision(D, []) -> [D];
handle_collision(D, OldDrops) when is_list(OldDrops), not is_list(D) ->
    % * io:format("Handle collision between ~p and ~p ", [D, OldDrops]),
    %% Phase 1, we just coalesce them and call it a day
    %P1 = [lists:foldl(fun drop:coalesce/2, D, OldDrops)],
    P1 = maybe_walk(fun drop:coalesce/2, D, OldDrops),
    %% Phase 2, if a drop is too big, it may split into two
    P2 = lists:flatmap(fun drop:split/1, P1),
    P2.

%% Walk over a list and cumulatively do something to it, with each
%% interaction determined by chance (coalesce_test).
%% Return a list of drops after the incoming drop has interacted with each of
%% the other ones.
%% The incoming drop STAYS AT THE HEAD so you can pull it off again.
%% Fun(A, A) -> MaybeFun(A, A) -> [A] -> [A]
maybe_walk(Fun, Elem, OldDrops) when is_list(OldDrops) ->
    maybe_walk(Fun, Elem, OldDrops, []).
maybe_walk(_Fun, Elem, [], Acc) -> [Elem|Acc];
maybe_walk(Fun, Elem, [H|T], Acc) ->
    case drop:coalesce_test(Elem, H) of
        true ->
            maybe_walk(Fun, Fun(H, Elem), T, Acc);
        false ->
            maybe_walk(Fun, Elem, T, [H|Acc])
    end.

%% Add a list of new drops to the dict.
%% if drops already present at the locations, handle the collisions.
%% Return new dict of drops
%% [{Coord, [Drop]}] -> DropDict -> DropDict
add_drops([], DropDict) -> DropDict;
add_drops([{_Coord, []}|OtherDs], DropDict) -> add_drops(OtherDs, DropDict);
add_drops([{Coord, [D|Ds]}|OtherDs], DropDict) ->
    NewDrops = [{Coord, Ds}|OtherDs],
    add_drops(NewDrops, add_drop({Coord, D}, DropDict));
add_drops([{Coord, D}|OtherDs], DropDict) when is_tuple(Coord) ->
    add_drops(OtherDs, add_drop({Coord, D}, DropDict)).

%% Add a single drop to the dict. NOT in a list.
%% {Coord, NewDrop} -> DropDict -> DropDict
add_drop({Coord, NewDrop}, DropDict) when not is_list(NewDrop) ->
    if NewDrop#dropstate.size * ?COLLISION_SCALE_CHECK > 1 ->
            %io:format(standard_error, "Collision testing~n~p~n~p~n",
            %[{Coord,NewDrop},DropDict]),
            {OurDrop, CheckedDict} = drop_intersection({Coord,NewDrop}, DropDict),
            %CheckedDict = dict:new(),
            %io:format(standard_error, "OLD~n~w~nNEW~n~w~n",
            %[dict:to_list(DropDict), dict:to_list(CheckedDict)]);
            ok;
        true->
            OurDrop = NewDrop,
            CheckedDict = DropDict
    end,
    case dict:find(Coord, CheckedDict) of
        {ok, DropList} ->
            NewDropList = handle_collision(OurDrop, DropList),
            %% This SHOULD be the only time that a drop can grow, so we will
            %% only test for size here.
            %% TODO
            % * io:format("Replacing ~p with ~p~n", [DropList, NewDropList]),
            NewDrops = dict:store(Coord, NewDropList, CheckedDict),
            NewDrops;
        error ->
            % * io:format("Storing ~p~n", [[OurDrop]]),
            NewDrops = dict:store(Coord, [OurDrop], CheckedDict),
            NewDrops
    end.

%% Give it a Drop and a dict and it will stochastically soak up drops that
%% intersect with NewDrop. Doesn't add the final drop
%% {{Coord}, dropstate} -> DropDict -> {dropstate, DropDict}
drop_intersection({{X1,Y1,Z1}, NewDrop}, DropDict) when is_integer(X1),
is_integer(Y1), is_integer(Z1), not is_list(NewDrop) ->
    {Xmin, Ymin, Zmin, Xmax, Ymax, Zmax} = get(domain_range), %% Set during init()
    %io:format(standard_error, "1 ", []),
    SearchSize = round(NewDrop#dropstate.size * ?COLLISION_SCALE_CHECK),
    RawCoordList = lists:usort(lists:append([
            [{X+X1,Y+Y1,Z+Z1},{Y+Y1,X+X1,Z+Z1},{X+X1,-Y+Y1,Z+Z1},
                {-X+X1,Y+Y1,Z+Z1},{-X+X1,-Y+Y1,Z+Z1},{-Y+Y1,X+X1,Z+Z1},
                {Y+Y1,-X+X1,Z+Z1},{-Y+Y1,-X+X1,Z+Z1}]
            || X <- lists:seq(0,SearchSize),
            Y <- lists:seq(0,SearchSize),
            Z <- lists:seq(0,0), %% Not going to do 3D yet
            X >= Y,
            math:pow((X*X + Y*Y + 0*0), 0.5) =< SearchSize %% Not going to do 3D yet
        ]
    )),
    %io:format(standard_error, "RawCoordList ~p~n", [RawCoordList]),
    CoordList = lists:filter(fun(C) -> is_local(
                    #nodestate{x1 = Xmin, y1 = Ymin, z1 = Zmin, x2 = Xmax, y2 = Ymax,
                        z2 = Zmax},
                    C) end,
        RawCoordList),
    %io:format(standard_error, "CoordList ~p~n", [CoordList]),
    drop_intersection(CoordList, NewDrop, DropDict).
drop_intersection([], Drop, DropDict) ->
    %io:format(standard_error, "2 ", []),
    {Drop, DropDict};
drop_intersection([Coord|CoordList], Drop, DropDict) ->
    case dict:find(Coord, DropDict) of
        {ok, PreCollideDropList} ->
            case handle_collision(Drop, PreCollideDropList) of
                [OurDrop|PostCollideDropList] when PostCollideDropList =:= [] ->
                    %io:format(standard_error, "3 ~p   ~w + ~w = ~w ~w ERASE~n",
                        %[Coord, Drop, PreCollideDropList, OurDrop, PostCollideDropList]),
                    PostCollideDropDict = dict:erase(Coord, DropDict);
                [OurDrop|PostCollideDropList] ->
                    %io:format(standard_error, "3 ~p   ~w + ~w = ~w ~w~n",
                        %[Coord, Drop, PreCollideDropList, OurDrop, PostCollideDropList]),
                    PostCollideDropDict = dict:store(Coord, PostCollideDropList, DropDict)
            end,
            drop_intersection(CoordList, OurDrop, PostCollideDropDict);
        error ->
            drop_intersection(CoordList, Drop, DropDict)
    end.


%% Move all drops to their new locations.
%% DropDict -> DropDict
-spec move_drops(dict()) -> dict().
move_drops(Dropdict) ->
    MovedDropList = dict:fold(
        fun(Coord, Ds, Acc0) ->
                lists:append(lists:flatmap(fun(D) -> rain_mvmt(Coord, D) end,
                        Ds), Acc0)
        end,
        [],
        Dropdict),
    %% Cumulative dict of new drops
    lists:foldl(fun add_drop/2, dict:new(), MovedDropList).


%% Change the actual position,
%% This is basically just zipWith((+), T1, T2) where T's are three-tuples
%% Coord -> Coord
%% Coord -> Coord -> Coord
migrate({X, Y, Z}) when is_integer(X), is_integer(Y), is_integer(Z) -> migrate({X, Y, Z}, random_direction()).
migrate({X, Y, Z}, {DX, DY, DZ}) when is_integer(X), is_integer(Y),
is_integer(Z), is_integer(DX), is_integer(DY), is_integer(DZ)  ->
    NewX = X + DX,
    NewY = Y + DY,
    NewZ = Z + DZ,
    {NewX, NewY, NewZ}.

%% If you give it {Coord, Drop} it returns the same.
%% Otherwise it just maps to migrate(Coord)
%% {Coord} -> {Coord}
%% {Coord} -> dropstate -> {{Coord}, dropstate}
%% Small drops go UP due to updrafts
rain_mvmt(Coord) -> [migrate(Coord)].
rain_mvmt(Coord, Drop = #dropstate{size = Size}) when is_float(Size) ->
    %% MILLIMETERS PER SECOND, FOLKS
    Tvelocity = drop:terminal_velocity(Size),
    %if Tvelocity > 0 ->
            %io:format(standard_error, "Size: ~p Velocity: ~p~n", [Size,
                    %Tvelocity]);
        %true -> ok
    %end,
    Gust = get(gust),
    Drops = drop:split(Drop),
    [{migrate(Coord, random_direction(
                0.0 - Gust * ?WINDSPEED, Gust * -?WINDSPEED * 0.3, % X
                -Tvelocity + Gust * ?UPDRAFT, -Tvelocity * 0.3 + Gust * ?UPDRAFT, % Y
                0.0, 0.0 % Z
            )
        ), D} || D <- Drops].

%% Chose a random x and y movement from -1,0,1
%% Coord
%% float -> Coord
random_direction() -> random_direction(1.0).
random_direction(Step) -> random_direction(-Step, Step, -Step, Step, -Step, Step).
%% Xmin = Ymin = 0
random_direction(Xmin, Xmax, Ymin, Ymax, Zmin, Zmax) when is_float(Xmin),
is_float(Ymin), is_float(Zmin), is_float(Xmax), is_float(Ymax),
is_float(Zmax) ->
    %io:format("~p ~p ~p ~p ~p ~p~n", [Xmin, Xmax, Ymin, Ymax, Zmin, Zmax]),
    %% This scaling is a disaster right now....
    DX = scaled_random_int(Xmin, Xmax),
    DY = scaled_random_int(Ymin, Ymax),
    DZ = scaled_random_int(Zmin, Zmax),
    {DX, DY, DZ}.


%% Filter out the drops that have left our domain.
%% nodestate -> DropDict -> {Local DropDict, Nonlocal DropDict}
filter_drops(S = #nodestate{}, Drops) ->
    % * io:format("Got drop dict, making list~n"),
    filter_drops(S, dict:to_list(Drops), [], []).
filter_drops(_S, [], Local, NonLocal) ->
    % * io:format("List empty~n"),
    %% TODO: Might have to use add_drops here instead.
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

%% Do something with drops that leave the entire domain.
%% nodestate -> DropDict -> DropList
handle_boundary_drops(S, OldDrops) ->
    Coords = dict:fetch_keys(OldDrops),
    handle_boundary_drops(S, Coords, OldDrops, []).
handle_boundary_drops(_S, [], _, NewDropAcc) -> lists:append(NewDropAcc);
handle_boundary_drops(S, [C|Coords], OldDrops, NewDropAcc) ->
    %% List of drops at coord C.
    DropList = dict:fetch(C, OldDrops),
    % * io:format("List of drops at coord ~p:~n~p~n", [C, DropList]),
    %% List of {NewCoord, Drop} to be added
    MovedDropList = lists:foldl(fun(D, Acc) ->
                case handle_boundary_drop(S, {C, D}) of
                    undefined -> Acc;
                    X -> [X|Acc]
                end
        end, [], DropList),
    % * io:format("List of {NewCoord, Drop} to be added:~n~p~n", [MovedDropList]),
    %% Cumulative List of new drops
    %NewDropDict = add_drops(MovedDropList, NewDrops),
    handle_boundary_drops(S, Coords, OldDrops, [MovedDropList|NewDropAcc]).
handle_boundary_drop(
    #nodestate{ x1 = X1, x2 = X2, y1 = Y1, y2 = Y2, z1 = Z1, z2 = Z2},
    {{X, Y, Z}, Drop}) ->
    NewX = positive_mirrored(X, X1, X2),
    NewY = positive_mirrored(Y, Y1, Y2),
    NewZ = positive_mirrored(Z, Z1, Z2),
    case {{NewX, NewY, NewZ}, Drop} of
        {{undefined, _, _}, _} -> undefined;
        {{_, undefined, _}, _} -> undefined;
        {{_, _, undefined}, _} -> undefined;
        _ -> {{NewX, NewY, NewZ}, Drop}
    end.


%% Return true if the drop D is in domain of nodestate S, else false.
%% nodestate -> {{Coords}, Drop} -> Bool
is_local(S = #nodestate{}, {{X, Y, Z}, _Drop}) when is_integer(X),
is_integer(Y), is_integer(Z)  ->
    is_local(S, {X,Y,Z});
is_local( _S = #nodestate{x1 = X1, x2 = X2, y1 = Y1, y2 = Y2, z1 = Z1, z2 =
        Z2}, {X, Y, Z}) when is_integer(X1), is_integer(X2), is_integer(Y1),
is_integer(Y2), is_integer(Z1), is_integer(Z2), is_integer(X), is_integer(Y),
is_integer(Z)  ->
    %io:format(standard_error, "~p ~p~n", [S, {X,Y,Z}]),
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
