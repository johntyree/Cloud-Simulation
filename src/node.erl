-module(node).
-compile(export_all). %% Probably replace with -export([funcs])
-author("John Tyree").

-include_lib("constants.hrl").

-record(state, {
        xoffset = {0, ?GRIDSIZE_X},
        yoffset = {0, ?GRIDSIZE_Y},
        zoffset = {0, ?GRIDSIZE_Z},
        drops = [],
        parent = nil}).

init() ->
    loop(#state).
init({Xoffset, Yoffset, Zoffset}, Drops, Parent) ->
    loop(#state{offset = {Xoffset, Yoffset, Zoffset},
            drops = Drops,
            parent = Parent}).

loop(S#state) ->
    ok.

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

bisect(Int)
    case Int rem 2 of 
        0 -> Left = Right = Int / 2;
        1 -> Left = 1 + (Rightx = Int / 2)
    end,
    {Left, Right}.
