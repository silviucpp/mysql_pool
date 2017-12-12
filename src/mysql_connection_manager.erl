-module(mysql_connection_manager).

-include("mysql_pool.hrl").

-define(ETS_CONNECTIONS_TABLE, mysql_pool_connections_table).

-behaviour(gen_server).

-export([
    start_link/0,
    create_pool/3,
    dispose_pool/1,
    add_connection/2,
    remove_connection/1,
    map_connections/2,
    get_connection_pool/1,
    pool_add_stm/3,
    pool_remove_stm/2,
    pool_get_statements/1,
    pool_get_statement/2,

    % gen_server

    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {pools}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_pool(PoolName, Size, ConnectionOptions) ->
    gen_server:call(?MODULE, {create_pool, PoolName, Size, ConnectionOptions}).

dispose_pool(PoolName) ->
    gen_server:call(?MODULE, {dispose_pool, PoolName}).

add_connection(Pid, PoolName) ->
    case catch ets:insert(?ETS_CONNECTIONS_TABLE, {Pid, PoolName}) of
        true ->
            ok;
        Error ->
            ?ERROR_MSG("failed to add the pid: ~p in the connections table. error: ~p", [Pid, Error]),
            Error
    end.

remove_connection(Pid) ->
    case catch ets:delete(?ETS_CONNECTIONS_TABLE, Pid) of
        true ->
            ok;
        Error ->
            ?ERROR_MSG("pid: ~p not found in the connections table error: ~p", [Pid, Error]),
            Error
    end.

map_connections(PoolName, Fun) ->
    Pids = ets:select(?ETS_CONNECTIONS_TABLE, [{ {'$1', PoolName}, [], ['$1']}]),
    plists:foreach(Fun, Pids, [{processes, schedulers}]).

get_connection_pool(Pid) ->
    case catch ets:lookup(?ETS_CONNECTIONS_TABLE, Pid) of
        [{Pid, Poolname}] ->
            {ok, Poolname};
        Error ->
            Error
    end.

pool_add_stm(PoolName, Stm, Query) ->
    ets:insert(PoolName, {Stm, Query}).

pool_remove_stm(PoolName, Stm) ->
    ets:delete(PoolName, Stm).

pool_get_statements(PoolName) ->
    ets:tab2list(PoolName).

pool_get_statement(PoolName, Stm) ->
    case ets:lookup(PoolName, Stm) of
        [{Stm, Query}] ->
            {ok, Stm, Query};
        _ ->
            null
    end.

%gen_server callbacks

init([]) ->
    process_flag(trap_exit, true),
    EtsTablesOPts = [set, named_table, public, {read_concurrency, true}],
    ?ETS_CONNECTIONS_TABLE = ets:new(?ETS_CONNECTIONS_TABLE, EtsTablesOPts),
    {ok, #state{pools = sets:new()}}.

handle_call({create_pool, PoolName, Size, ConnectionOptions}, _From, #state{pools = P} = State) ->
    case internal_create_pool(PoolName, Size, ConnectionOptions) of
        {ok, _} = R ->
            {reply, R, State#state{pools = sets:add_element(PoolName, P)}};
        Error ->
            {reply, Error, State}
    end;
handle_call({dispose_pool, PoolName}, _From, #state{pools = P} = State) ->
    case internal_dispose_pool(PoolName) of
        ok ->
            {reply, ok, State#state{pools = sets:del_element(PoolName, P)}};
        Error ->
            {reply, Error, State}
    end;
handle_call(Request, _From, State) ->
    ?ERROR_MSG("unexpected request: ~p", [Request]),
    {reply, ok, State}.

handle_cast(Request, State) ->
    ?ERROR_MSG("unexpected cast message: ~p", [Request]),
    {noreply, State}.

handle_info(Info, State) ->
    ?ERROR_MSG("unexpected info message: ~p", [Info]),
    {noreply, State}.

terminate(Reason, #state{pools = Pools}) ->
    ?INFO_MSG("mysql_connection_manager will stop with reason: ~p...", [Reason]),

    Fun = fun(P) ->
        ?INFO_MSG("mysql_connection_manager remove pool: ~p", [P]),
        map_connections(P, fun(C) -> mysql_connection:stop(C) end),
        internal_dispose_pool(P)
    end,
    lists:foreach(Fun, sets:to_list(Pools)),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% internals

internal_create_pool(PoolName, Size, ConnectionOptions) ->
    try
        PoolName = ets:new(PoolName, [set, named_table, public, {read_concurrency, true}]),
        PoolConfig = [
            {name, PoolName},
            {max_count, Size},
            {init_count, Size},
            {queue_max, 50000},
            {start_mfa, {mysql_connection_proxy, start_link, [PoolName, ConnectionOptions]}}
        ],
        pooler:new_pool(PoolConfig)
    catch
        _:Error ->
            ?ERROR_MSG("creating pool: ~p failed with error: ~p stack: ~p", [PoolName, Error, erlang:get_stacktrace()]),
            Error
    end.

internal_dispose_pool(PoolName) ->
    case catch ets:delete(PoolName) of
        true ->
            pooler:rm_pool(PoolName);
        Error ->
            Error
    end.
