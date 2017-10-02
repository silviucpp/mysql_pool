-module(mysql_pool).

-export([
    start/0,
    start/1,
    stop/0,
    add_pool/3,
    remove_pool/1,
    prepare/3,
    unprepare/2,
    query/2,
    query/3,
    query/4,
    execute/3,
    execute/4,
    query_opt/3,
    query_opt/4,
    query_opt/5,
    execute_opt/4,
    execute_opt/5,
    transaction/2,
    transaction/3,
    transaction/4,
    with/2
]).

-spec start() ->
    ok  | {error, reason()}.

start() ->
    start(temporary).

-spec start(permanent | transient | temporary) ->
    ok | {error, reason()}.

start(Type) ->
    case application:ensure_all_started(mysql_pool, Type) of
        {ok, _} ->
            ok;
        Other ->
            Other
    end.

-spec stop() ->
    ok.

stop() ->
    application:stop(mysql_pool).

-spec add_pool(atom(), integer(), list()) ->
    {ok, pid()} | {error, term()}.

add_pool(PoolName, Size, ConnectionOptions) ->

    ok = mysql_connection_manager:create_pool(PoolName),

    PoolConfig = [
        {name, PoolName},
        {max_count, Size},
        {init_count, Size},
        {queue_max, 50000},
        {start_mfa, {mysql_connection_proxy, start_link, [PoolName, ConnectionOptions]}}],

    pooler:new_pool(PoolConfig).

-spec remove_pool(atom()) ->
    ok | {error, term()}.

remove_pool(PoolName) ->
    true = mysql_connection_manager:dispose_pool(PoolName),
    pooler:rm_pool(PoolName).

-spec prepare(atom(), atom(), binary()) ->
    ok | {error, term()}.

prepare(PoolName, Stm, Query) ->
    case mysql_connection_manager:pool_add_stm(PoolName, Stm, Query) of
        true ->
            try
                mysql_connection_manager:map_connections(PoolName, fun(Pid) -> {ok, Stm} = mysql_connection:prepare(Pid, Stm, Query) end),
                ok
            catch _: Error ->
                {error, Error}
            end;
        Error ->
            {error, Error}
    end.

-spec unprepare(atom(), atom()) ->
    ok | {error, term()}.

unprepare(PoolName, Stm) ->
    case mysql_connection_manager:pool_remove_stm(PoolName, Stm) of
        true ->
            try
                mysql_connection_manager:map_connections(PoolName, fun(Pid) -> ok = mysql_connection:unprepare(Pid, Stm) end),
                ok
            catch _: Error ->
                {error, Error}
            end;
        Error ->
            {error, Error}
    end.

-spec query(atom(), binary()) ->
    term().

query(PoolName, Query) ->
    pooler_transaction(PoolName, fun(MysqlConn) -> mysql_connection:query(MysqlConn, Query) end).

-spec query(atom(), binary(), list()|integer()) ->
    term().

query(PoolName, Query, Params) ->
    pooler_transaction(PoolName, fun(MysqlConn) -> mysql_connection:query(MysqlConn, Query, Params) end).

-spec query(atom(), binary(), list(), integer()) ->
    term().

query(PoolName, Query, Params, Timeout) ->
    pooler_transaction(PoolName, fun(MysqlConn) -> mysql_connection:query(MysqlConn, Query, Params, Timeout) end).

-spec query_opt(atom(), binary(), affected_rows|insert_id|both) ->
    term().

query_opt(PoolName, Query, OptionFlag) ->
    pooler_transaction(PoolName, fun(MysqlConn) -> mysql_connection:query_opt(MysqlConn, Query, OptionFlag) end).

-spec query_opt(atom(), binary(), list()|integer(), affected_rows|insert_id|both) ->
    term().

query_opt(PoolName, Query, Params, OptionFlag) ->
    pooler_transaction(PoolName, fun(MysqlConn) -> mysql_connection:query_opt(MysqlConn, Query, Params, OptionFlag) end).

-spec query_opt(atom(), binary(), list(), integer(), affected_rows|insert_id|both) ->
    term().

query_opt(PoolName, Query, Params, Timeout, OptionFlag) ->
    pooler_transaction(PoolName, fun(MysqlConn) -> mysql_connection:query_opt(MysqlConn, Query, Params, Timeout, OptionFlag) end).

-spec execute(atom(), atom(), list()) ->
    term().

execute(PoolName, StatementRef, Params) ->
    pooler_transaction(PoolName, fun(MysqlConn) -> mysql_connection:execute(MysqlConn, StatementRef, Params) end).

-spec execute(atom(), atom(), list(), integer()) ->
    term().

execute(PoolName, StatementRef, Params, Timeout) ->
    pooler_transaction(PoolName, fun(MysqlConn) -> mysql_connection:execute(MysqlConn, StatementRef, Params, Timeout) end).

-spec execute_opt(atom(), atom(), list(), affected_rows|insert_id|both) ->
    term().

execute_opt(PoolName, StatementRef, Params, OptionFlag) ->
    pooler_transaction(PoolName, fun(MysqlConn) -> mysql_connection:execute_opt(MysqlConn, StatementRef, Params, OptionFlag) end).

-spec execute_opt(atom(), atom(), list(), integer(), affected_rows|insert_id|both) ->
    term().

execute_opt(PoolName, StatementRef, Params, Timeout, OptionFlag) ->
    pooler_transaction(PoolName, fun(MysqlConn) -> mysql_connection:execute_opt(MysqlConn, StatementRef, Params, Timeout, OptionFlag) end).

-spec transaction(atom(), fun()) ->
    term().

transaction(PoolName, TransactionFun) when is_function(TransactionFun, 1) ->
    pooler_transaction(PoolName, fun(MysqlConn) ->
        mysql_connection:transaction(MysqlConn, TransactionFun, [MysqlConn], infinity)
    end).

-spec transaction(atom(), fun(), list()) ->
    term().

transaction(PoolName, TransactionFun, Args) when is_function(TransactionFun, length(Args) + 1) ->
    pooler_transaction(PoolName, fun(MysqlConn) ->
        mysql_connection:transaction(MysqlConn, TransactionFun, [MysqlConn | Args], infinity)
    end).

-spec transaction(atom(), fun(), list(), integer()|infinity) ->
    term().

transaction(PoolName, TransactionFun, Args, Retries) when is_function(TransactionFun, length(Args) + 1) ->
    pooler_transaction(PoolName, fun(MysqlConn) ->
        mysql_connection:transaction(MysqlConn, TransactionFun, [MysqlConn | Args], Retries)
    end).

-spec with(atom(), fun()) ->
    term().

with(PoolName, Fun) when is_function(Fun, 1) ->
    pooler_transaction(PoolName, Fun).

% internals

pooler_transaction(Pool, Fun) ->
    ProxyPid = pooler:take_member(Pool, 5000),
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
