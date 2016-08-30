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
    {ok, _} = application:ensure_all_started(mysql_pool),
    ok.

stop() ->
    application:stop(mysql_pool).

-spec(add_pool(PoolName::atom(), Size::integer(), ConnectionOptions::list()) -> {ok, Pid::pid()} | {error, Error::term()}).

add_pool(PoolName, Size, ConnectionOptions) ->

    ok = mysql_connection_manager:create_pool(PoolName),

    PoolConfig = [
        {name, PoolName},
        {max_count, Size},
        {init_count, Size},
        {start_mfa, {mysql_connection_proxy, start_link, [PoolName, ConnectionOptions]}}],

    pooler:new_pool(PoolConfig).

-spec(remove_pool(PoolName::atom()) -> ok | {error, Error::term()}).

remove_pool(PoolName) ->
    true = mysql_connection_manager:dispose_pool(PoolName),
    pooler:rm_pool(PoolName).

-spec(prepare(PoolName::atom(), Stm::atom(), Query::binary()) -> ok | {error, Error::term()}).

prepare(PoolName, Stm, Query) ->
    case mysql_connection_manager:pool_add_stm(PoolName, Stm, Query) of
        true ->
            mysql_connection_manager:map_connections(fun(Pid) -> mysql_connection:prepare(Pid, Stm, Query) end),
            ok;
        Error ->
            {error, Error}
    end.

-spec(unprepare(PoolName::atom(), Stm::atom()) -> ok | {error, Error::term()}).

unprepare(PoolName, Stm) ->
    case mysql_connection_manager:pool_remove_stm(PoolName, Stm) of
        true ->
            mysql_connection_manager:map_connections(fun(Pid) -> mysql_connection:unprepare(Pid, Stm) end),
            ok;
        Error ->
            {error, Error}
    end.

-spec(query(PoolName::atom(), Query::binary()) -> Result::term()).

query(PoolName, Query) ->
    pooler_transaction(PoolName, fun(MysqlConn) -> mysql_connection:query(MysqlConn, Query) end).

-spec(query(PoolName::atom(), Query::binary(), Params::list()|integer()) -> Result::term()).

query(PoolName, Query, Params) ->
    pooler_transaction(PoolName, fun(MysqlConn) -> mysql_connection:query(MysqlConn, Query, Params) end).

-spec(query(PoolName::atom(), Query::binary(), Params::list(), Timeout::integer()) -> Result::term()).

query(PoolName, Query, Params, Timeout) ->
    pooler_transaction(PoolName, fun(MysqlConn) -> mysql_connection:query(MysqlConn, Query, Params, Timeout) end).

-spec(execute(PoolName::atom(), StatementRef::atom(), Params::list()) -> Result::term()).

execute(PoolName, StatementRef, Params) ->
    pooler_transaction(PoolName, fun(MysqlConn) -> mysql_connection:execute(MysqlConn, StatementRef, Params) end).

-spec(execute(PoolName::atom(), StatementRef::atom(), Params::list(), Timeout::integer()) -> Result::term()).

execute(PoolName, StatementRef, Params, Timeout) ->
    pooler_transaction(PoolName, fun(MysqlConn) -> mysql_connection:execute(MysqlConn, StatementRef, Params, Timeout) end).

-spec(transaction(PoolName::atom(), TransactionFun::fun()) -> Result::term()).

transaction(PoolName, TransactionFun) when is_function(TransactionFun, 1) ->
    pooler_transaction(PoolName, fun(MysqlConn) ->
        mysql_connection:transaction(MysqlConn, TransactionFun, [MysqlConn], infinity)
    end).

-spec(transaction(PoolName::atom(), TransactionFun::fun(), Args::list()) -> Result::term()).

transaction(PoolName, TransactionFun, Args) when is_function(TransactionFun, length(Args) + 1) ->
    pooler_transaction(PoolName, fun(MysqlConn) ->
        mysql_connection:transaction(MysqlConn, TransactionFun, [MysqlConn | Args], infinity)
    end).

-spec(transaction(PoolName::atom(), TransactionFun::fun(), Args::list(), Retries::integer()|infinity) -> Result::term()).

transaction(PoolName, TransactionFun, Args, Retries) when is_function(TransactionFun, length(Args) + 1) ->
    pooler_transaction(PoolName, fun(MysqlConn) ->
        mysql_connection:transaction(MysqlConn, TransactionFun, [MysqlConn | Args], Retries)
    end).

-spec(with(PoolName::atom(), Fun::fun()) -> Result::term()).

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