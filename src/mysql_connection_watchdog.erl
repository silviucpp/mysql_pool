-module(mysql_connection_watchdog).
-author("silviu").

-include("mysql_pool.hrl").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
    start_link/0,
    watch_connection/1,
    map_connection/1
]).

-define(SERVER, ?MODULE).
-define(POOL_CONNECTION_WATCHDOG_TABLE, mysql_pool_connection_watchdog_table).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

watch_connection(Pid) ->
    gen_server:call(?MODULE, {watch_connection, Pid}).

map_connection(Fun) ->
    ets:foldl(fun({Pid, _}, _Acc) -> Fun(Pid) end, ok, ?POOL_CONNECTION_WATCHDOG_TABLE).

%gen_server callbacks

init([]) ->
    ?POOL_CONNECTION_WATCHDOG_TABLE = ets:new(?POOL_CONNECTION_WATCHDOG_TABLE, [set, named_table, protected, {read_concurrency, true}]),
    {ok, #state{}}.

handle_call({watch_connection, Pid}, _From, State) ->
    case catch ets:insert(?POOL_CONNECTION_WATCHDOG_TABLE, {Pid, true}) of
        true ->
            erlang:monitor(process, Pid),
            {reply, ok, State};
        Error ->
            ?ERROR_MSG(<<"failed to monitor the pid: ~p error: ~p">>, [Pid, Error]),
            {reply, {error, Error}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    case catch ets:delete(?POOL_CONNECTION_WATCHDOG_TABLE, Pid) of
        true ->
            ok;
        Error ->
            ?ERROR_MSG(<<"pid: ~p not found in the watchdog table error: ~p">>, [Pid, Error])
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

