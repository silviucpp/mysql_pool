-module(mysql_pool).
-author("silviu").

-export([
    start/0, stop/0,
    add_pool/3, remove_pool/1,
    prepare/3, unprepare/2,
    query/2, query/3, query/4,
    execute/3, execute/4,
    transaction/2, transaction/3, transaction/4,
    with/2
]).

start() ->
    application:ensure_all_started(mysql_pool).

stop() ->
    application:stop(mysql_pool).

add_pool(PoolName, Size, ConnectionOptions) ->

    ok = mysql_connection_manager:create_pool(PoolName),

    PoolConfig = [
        {name, PoolName},
        {max_count, Size},
        {init_count, Size},
        {start_mfa, {mysql_connection_proxy, start_link, [PoolName, ConnectionOptions]}}],

    pooler:new_pool(PoolConfig).

remove_pool(PoolName) ->
    true = mysql_connection_manager:dispose_pool(PoolName),
    pooler:rm_pool(PoolName).

prepare(PoolName, Stm, Query) ->
    case mysql_connection_manager:pool_add_stm(PoolName, Stm, Query) of
        true ->
            mysql_connection_manager:map_connections(fun(Pid) -> mysql:prepare(Pid, Stm, Query) end);
        Error ->
            {error, Error}
    end.

unprepare(PoolName, Stm) ->
    case mysql_connection_manager:pool_remove_stm(PoolName, Stm) of
        true ->
            mysql_connection_manager:map_connections(fun(Pid) -> mysql:unprepare(Pid, Stm) end);
        Error ->
            {error, Error}
    end.

query(PoolName, Query) ->
    pooler_transaction(PoolName, fun(MysqlConn) -> mysql:query(MysqlConn, Query) end).

query(PoolName, Query, Params) ->
    pooler_transaction(PoolName, fun(MysqlConn) -> mysql:query(MysqlConn, Query, Params) end).

query(PoolName, Query, Params, Timeout) ->
    pooler_transaction(PoolName, fun(MysqlConn) -> mysql:query(MysqlConn, Query, Params, Timeout) end).

execute(PoolName, StatementRef, Params) ->
    pooler_transaction(PoolName, fun(MysqlConn) -> mysql:execute(MysqlConn, StatementRef, Params) end).

execute(PoolName, StatementRef, Params, Timeout) ->
    pooler_transaction(PoolName, fun(MysqlConn) -> mysql:execute(MysqlConn, StatementRef, Params, Timeout) end).

transaction(PoolName, TransactionFun) when is_function(TransactionFun, 1) ->
    pooler_transaction(PoolName, fun(MysqlConn) ->
        mysql:transaction(MysqlConn, TransactionFun, [MysqlConn], infinity)
    end).

transaction(PoolName, TransactionFun, Args) when is_function(TransactionFun, length(Args) + 1) ->
    pooler_transaction(PoolName, fun(MysqlConn) ->
        mysql:transaction(MysqlConn, TransactionFun, [MysqlConn | Args], infinity)
    end).

transaction(PoolName, TransactionFun, Args, Retries) when is_function(TransactionFun, length(Args) + 1) ->
    pooler_transaction(PoolName, fun(MysqlConn) ->
        mysql:transaction(MysqlConn, TransactionFun, [MysqlConn | Args], Retries)
    end).

with(PoolName, Fun) when is_function(Fun, 1) ->
    pooler_transaction(PoolName, Fun).

% internals

pooler_transaction(Pool, Fun) ->
    ProxyPid = pooler:take_member(Pool),
    try
        proxy_exec(ProxyPid, Fun)
    after
        ok = pooler:return_member(Pool, ProxyPid)
    end.

proxy_exec(ProxyPid, Fun) ->
    case mysql_connection_proxy:get_pid(ProxyPid) of
        {ok, Pid} ->
            Fun(Pid);
        Error ->
            Error
    end.