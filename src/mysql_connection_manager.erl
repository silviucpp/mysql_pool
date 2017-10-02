-module(mysql_connection_manager).

-include("mysql_pool.hrl").

-define(SERVER, ?MODULE).
-define(POOL_CONNECTIONS_TABLE, mysql_pool_connections_table).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
    start_link/0,
    add_connection/2,
    remove_connection/1,
    map_connections/2,
    get_connection_pool/1,
    create_pool/1,
    dispose_pool/1,
    pool_add_stm/3,
    pool_remove_stm/2,
    pool_get_statements/1,
    pool_get_statement/2
]).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_connection(Pid, PoolName) ->
    gen_server:call(?MODULE, {add_connection, Pid, PoolName}).

remove_connection(Pid) ->
    gen_server:call(?MODULE, {remove_connection, Pid}).

map_connections(PoolName, Fun) ->
    Pids = ets:select(?POOL_CONNECTIONS_TABLE, [{ {'$1', PoolName}, [], ['$1']}]),
    plists:foreach(Fun, Pids, [{processes, schedulers}]).

get_connection_pool(Pid) ->
    case catch ets:lookup(?POOL_CONNECTIONS_TABLE, Pid) of
        [{Pid, Poolname}] ->
            {ok, Poolname};
        Error ->
            Error
    end.

create_pool(PoolName) ->
    gen_server:call(?MODULE, {create_pool, PoolName}).

dispose_pool(PoolName) ->
    ets_pool_dispose(PoolName).

pool_add_stm(PoolName, Stm, Query) ->
    ets_pool_set(PoolName, Stm, Query).

pool_remove_stm(PoolName, Stm) ->
    ets_pool_del(PoolName, Stm).

pool_get_statements(PoolName) ->
    ets_pool_to_list(PoolName).

pool_get_statement(PoolName, Stm) ->
    ets_pool_get_statement(PoolName, Stm).

%gen_server callbacks

init([]) ->
    ?POOL_CONNECTIONS_TABLE = ets:new(?POOL_CONNECTIONS_TABLE, [set, named_table, protected, {read_concurrency, true}]),
    {ok, #state{}}.

handle_call({add_connection, Pid, PoolName}, _From, State) ->
    {reply, ets_add_connection(Pid, PoolName), State};
handle_call({remove_connection, Pid}, _From, State) ->
    {reply, ets_remove_connection(Pid), State};
handle_call({create_pool, PoolName}, _From, State) ->
    {reply,  ets_pool_create(PoolName), State};
handle_call(Request, _From, State) ->
    ?ERROR_MSG("unexpected request: ~p", [Request]),
    {reply, ok, State}.

handle_cast(Request, State) ->
    ?ERROR_MSG("unexpected cast message: ~p", [Request]),
    {noreply, State}.

handle_info(Info, State) ->
    ?ERROR_MSG("unexpected info message: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%internal methods

ets_add_connection(Pid, PoolName) ->
    case catch ets:insert(?POOL_CONNECTIONS_TABLE, {Pid, PoolName}) of
        true ->
            ok;
        Error ->
            ?ERROR_MSG("failed to add the pid: ~p in the connections table. error: ~p", [Pid, Error]),
            Error
    end.

ets_remove_connection(Pid) ->
    case catch ets:delete(?POOL_CONNECTIONS_TABLE, Pid) of
        true ->
            ok;
        Error ->
            ?ERROR_MSG("pid: ~p not found in the connections table error: ~p", [Pid, Error]),
            Error
    end.

ets_pool_create(PoolName) ->
    case catch ets:new(PoolName, [set, named_table, public, {read_concurrency, true}]) of
        PoolName ->
            ok;
        Error ->
            ?ERROR_MSG("creating pool ~p failed with error: ~p", [PoolName, Error]),
            Error
    end.

ets_pool_dispose(PoolName) ->
    ets:delete(PoolName).

ets_pool_set(PoolName, StmName, Query) ->
    ets:insert(PoolName, {StmName, Query}).

ets_pool_del(PoolName, StmName) ->
    ets:delete(PoolName, StmName).

ets_pool_to_list(PoolName) ->
    ets:tab2list(PoolName).

ets_pool_get_statement(PoolName, Stm) ->
    case ets:lookup(PoolName, Stm) of
        [{Stm, Query}] ->
            {ok, Stm, Query};
        _ ->
            null
    end.
