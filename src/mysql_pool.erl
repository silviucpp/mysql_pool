-module(mysql_pool).

-include("mysql_pool.hrl").

-export([
    start/0,
    start/1,
    stop/0,
    add_pool/4,
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

-spec add_pool(pool_id(), non_neg_integer(), non_neg_integer(), list()) ->
    {ok, pid()} | {error, reason()}.

add_pool(PoolName, PoolSize, MaxOverflow, ConnectionOptions) ->
    mysql_connection_manager:create_pool(PoolName, PoolSize, MaxOverflow, ConnectionOptions).

-spec remove_pool(pool_id()) ->
    ok | {error, reason()}.

remove_pool(PoolName) ->
    mysql_connection_manager:dispose_pool(PoolName).

-spec prepare(pool_id(), stm_id(), binary()) ->
    ok | {error, reason()}.

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

-spec unprepare(pool_id(), stm_id()) ->
    ok | {error, reason()}.

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

-spec query(pool_id(), binary()) ->
    term().

query(PoolName, Query) ->
    poolboy_transaction(PoolName, fun(MysqlConn) -> mysql_connection:query(MysqlConn, Query) end).

-spec query(pool_id(), binary(), list()|integer()) ->
    term().

query(PoolName, Query, Params) ->
    poolboy_transaction(PoolName, fun(MysqlConn) -> mysql_connection:query(MysqlConn, Query, Params) end).

-spec query(pool_id(), binary(), list(), integer()) ->
    term().

query(PoolName, Query, Params, Timeout) ->
    poolboy_transaction(PoolName, fun(MysqlConn) -> mysql_connection:query(MysqlConn, Query, Params, Timeout) end, Timeout).

-spec query_opt(pool_id(), binary(), opt_flag()) ->
    term().

query_opt(PoolName, Query, OptionFlag) ->
    poolboy_transaction(PoolName, fun(MysqlConn) -> mysql_connection:query_opt(MysqlConn, Query, OptionFlag) end).

-spec query_opt(pool_id(), binary(), list()|integer(), opt_flag()) ->
    term().

query_opt(PoolName, Query, Params, OptionFlag) ->
    poolboy_transaction(PoolName, fun(MysqlConn) -> mysql_connection:query_opt(MysqlConn, Query, Params, OptionFlag) end).

-spec query_opt(pool_id(), binary(), list(), integer(), opt_flag()) ->
    term().

query_opt(PoolName, Query, Params, Timeout, OptionFlag) ->
    poolboy_transaction(PoolName, fun(MysqlConn) -> mysql_connection:query_opt(MysqlConn, Query, Params, Timeout, OptionFlag) end, Timeout).

-spec execute(pool_id(), stm_id(), list()) ->
    term().

execute(PoolName, StatementRef, Params) ->
    poolboy_transaction(PoolName, fun(MysqlConn) -> mysql_connection:execute(MysqlConn, StatementRef, Params) end).

-spec execute(pool_id(), stm_id(), list(), integer()) ->
    term().

execute(PoolName, StatementRef, Params, Timeout) ->
    poolboy_transaction(PoolName, fun(MysqlConn) -> mysql_connection:execute(MysqlConn, StatementRef, Params, Timeout) end, Timeout).

-spec execute_opt(pool_id(), stm_id(), list(), opt_flag()) ->
    term().

execute_opt(PoolName, StatementRef, Params, OptionFlag) ->
    poolboy_transaction(PoolName, fun(MysqlConn) -> mysql_connection:execute_opt(MysqlConn, StatementRef, Params, OptionFlag) end).

-spec execute_opt(pool_id(), stm_id(), list(), integer(), opt_flag()) ->
    term().

execute_opt(PoolName, StatementRef, Params, Timeout, OptionFlag) ->
    poolboy_transaction(PoolName, fun(MysqlConn) -> mysql_connection:execute_opt(MysqlConn, StatementRef, Params, Timeout, OptionFlag) end, Timeout).

-spec transaction(pool_id(), fun()) ->
    term().

transaction(PoolName, TransactionFun) when is_function(TransactionFun, 1) ->
    poolboy_transaction(PoolName, fun(MysqlConn) ->
        mysql_connection:transaction(MysqlConn, TransactionFun, [MysqlConn], infinity)
    end).

-spec transaction(pool_id(), fun(), list()) ->
    term().

transaction(PoolName, TransactionFun, Args) when is_function(TransactionFun, length(Args) + 1) ->
    poolboy_transaction(PoolName, fun(MysqlConn) ->
        mysql_connection:transaction(MysqlConn, TransactionFun, [MysqlConn | Args], infinity)
    end).

-spec transaction(pool_id(), fun(), list(), integer()|infinity) ->
    term().

transaction(PoolName, TransactionFun, Args, Retries) when is_function(TransactionFun, length(Args) + 1) ->
    poolboy_transaction(PoolName, fun(MysqlConn) ->
        mysql_connection:transaction(MysqlConn, TransactionFun, [MysqlConn | Args], Retries)
    end).

-spec with(pool_id(), fun()) ->
    term().

with(PoolName, Fun) when is_function(Fun, 1) ->
    poolboy_transaction(PoolName, Fun).

% internals

poolboy_transaction(PoolName, Fun) ->
    poolboy_transaction(PoolName, Fun, infinity).

poolboy_transaction(PoolName, Fun, Timeout) ->
    ProxyPid = poolboy:checkout(PoolName, true, Timeout),
    try
        proxy_exec(ProxyPid, Fun)
    after
        ok = poolboy:checkin(PoolName, ProxyPid)
    end.

proxy_exec(ProxyPid, Fun) ->
    case mysql_connection_proxy:get_pid(ProxyPid) of
        {ok, Pid} ->
            Fun(Pid);
        Error ->
            Error
    end.
