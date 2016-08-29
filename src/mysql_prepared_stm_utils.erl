-module(mysql_prepared_stm_utils).
-author("silviu.caragea").

-export([create/1, dispose/1, set/3, del/2, to_list/1]).

create(PoolName) ->
    PoolName = ets:new(PoolName, [set, named_table, public, {read_concurrency, true}]),
    ok.

dispose(PoolName) ->
    ets:delete(PoolName).

set(PoolName, StmName, Query) ->
    ets:insert(PoolName, {StmName, Query}).

del(PoolName, StmName) ->
    ets:delete(PoolName, StmName).

to_list(PoolName) ->
    ets:tab2list(PoolName).