-module(mysql_connection_manager).

-include("mysql_pool.hrl").

-define(ETS_CONNECTIONS_TABLE, mysql_pool_connections_table).

-behaviour(gen_server).

-export([
    setup/0,
    start_link/1,
    create_pool/4,
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

-record(state, {app_pid}).

setup() ->
    ?ETS_CONNECTIONS_TABLE = ets:new(?ETS_CONNECTIONS_TABLE, [set, named_table, public, {read_concurrency, true}]),
    ok.

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Args], []).

create_pool(PoolName, Size, MaxOverflow, ConnectionOptions) ->
    gen_server:call(?MODULE, {create_pool, PoolName, Size, MaxOverflow, ConnectionOptions}).

dispose_pool(PoolName) ->
    gen_server:call(?MODULE, {dispose_pool, PoolName}).

add_connection(Pid, PoolName) ->
    case catch ets:insert(?ETS_CONNECTIONS_TABLE, {Pid, PoolName}) of
        true ->
            ok;
        Error ->
            ?LOG_ERROR("failed to add the pid: ~p in the connections table. error: ~p", [Pid, Error]),
            Error
    end.

remove_connection(Pid) ->
    case catch ets:delete(?ETS_CONNECTIONS_TABLE, Pid) of
        true ->
            ok;
        Error ->
            ?LOG_ERROR("pid: ~p not found in the connections table error: ~p", [Pid, Error]),
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

init([AppControllerPid]) ->
    process_flag(trap_exit, true),
    {ok, #state{app_pid = AppControllerPid}}.

handle_call({create_pool, PoolName, PoolSize, MaxOverflow, ConnectionOptions}, _From, #state{app_pid = AppPid} = State) ->
    Response = internal_create_pool(PoolName, PoolSize, MaxOverflow, ConnectionOptions, AppPid),
    {reply, Response, State};
handle_call({dispose_pool, PoolName}, _From, State) ->
    {reply, internal_dispose_pool(PoolName), State};
handle_call(Request, _From, State) ->
    ?LOG_ERROR("unexpected request: ~p", [Request]),
    {reply, ok, State}.

handle_cast(Request, State) ->
    ?LOG_ERROR("unexpected cast message: ~p", [Request]),
    {noreply, State}.

handle_info(Info, State) ->
    ?LOG_ERROR("unexpected info message: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _Pools) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% internals

internal_create_pool(PoolName, Size, MaxOverflow, ConnectionOptions, AppPid) ->
    case mysql_pool_sup:has_pool(PoolName) of
        false ->
            try
                PoolName = ets:new(PoolName, [set, named_table, public, {read_concurrency, true}, {heir, AppPid, ok}]),

                PoolArgs = [
                    {max_overflow, MaxOverflow},
                    {size, Size},
                    {name, {local, PoolName}},
                    {worker_module, mysql_connection_proxy}
                ],
                ChildSpecs = poolboy:child_spec(PoolName, PoolArgs, [PoolName, ConnectionOptions]),
                mysql_pool_sup:add_pool(PoolName, ChildSpecs)
            catch
                _:Error ->
                    ?LOG_ERROR("creating pool: ~p failed with error: ~p", [PoolName, Error]),
                    Error
            end;
        _ ->
            {error, already_started}
    end.

internal_dispose_pool(PoolName) ->
    case mysql_pool_sup:has_pool(PoolName) of
        true ->
            true = ets:delete(PoolName),
            mysql_pool_sup:remove_pool(PoolName);
        _ ->
            {error, not_found}
    end.
