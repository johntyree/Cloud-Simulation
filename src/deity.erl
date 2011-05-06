-module(deity).
-compile(export_all). %% Probably replace with -export([funcs])
-author("John Tyree").

init(N) when N > 0 ->
    Drop_List = populate_domain(N),
    Drop_List.

populate_domain(N) ->
    Collector = spawn(?MODULE, collect_drops, [N, self()]),
    populate_domain(N, [], Collector),
    receive
        {droplist, L} ->
            L
    end.
populate_domain(0, Acc, _) -> Acc;
populate_domain(N, Acc, Collector) ->
    Drop = spawn(drop, init, [Collector, 0]),
    populate_domain(N-1, [Drop|Acc], Collector).

collect_drops(N, Server) when N >= 0, is_pid(Server) ->
    collect_drops(N, [], Server).
collect_drops(0, Acc, Server) ->
    Server ! {droplist, Acc};
collect_drops(N, Acc, Server) ->
    receive
        {newdrop, DropPid, {Position, Size}} ->
            collect_drops(N-1, [{Position, {DropPid, Size}} | Acc], Server)
    end.
