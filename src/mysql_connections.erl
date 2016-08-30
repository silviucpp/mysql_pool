-module(mysql_connections).
-author("silviu").

-include("mysql_pool.hrl").

-define(POOL_CONNECTIONS_TABLE, mysql_pool_connections_table).

-export([init/0, add/1, remove/1, map/1]).

init() ->
    ?POOL_CONNECTIONS_TABLE = ets:new(?POOL_CONNECTIONS_TABLE, [set, named_table, public, {read_concurrency, true}]),
    ok.

add(Pid) ->
    case catch ets:insert(?POOL_CONNECTIONS_TABLE, {Pid, true}) of
        true ->
            ok;
        Error ->
            ?ERROR_MSG(<<"failed to add the pid: ~p in the connections table. error: ~p">>, [Pid, Error]),
            Error
    end.

remove(Pid) ->
    case catch ets:delete(?POOL_CONNECTIONS_TABLE, Pid) of
        true ->
            ok;
        Error ->
            ?ERROR_MSG(<<"pid: ~p not found in the connections table error: ~p">>, [Pid, Error])
    end.

map(Fun) ->
    ets:foldl(fun({Pid, _}, _Acc) -> Fun(Pid) end, ok, ?POOL_CONNECTIONS_TABLE).