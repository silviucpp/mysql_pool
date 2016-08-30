-module(mysql_connection_proxy).
-author("silviu").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/2, get_pid/1]).

-record(state, {pool_name, connection_pid, connection_args}).

start_link(PoolName, Args) ->
    gen_server:start_link(?MODULE, [PoolName, Args], []).

get_pid(ProxyPid) ->
    gen_server:call(ProxyPid, get_connection_pid, infinity).

init([PoolName, Args]) ->
    process_flag(trap_exit, true),
    {ok, #state{connection_args = Args, pool_name = PoolName}}.

handle_call(get_connection_pid, _From, State=#state{connection_pid =Pid}) ->
    case Pid of
        undefined ->
            case get_connection(State#state.pool_name, State#state.connection_args) of
                {ok, NewPid} ->
                    {reply, {ok, NewPid}, State#state{connection_pid = NewPid}};
                UnexpectedResult ->
                    {reply, UnexpectedResult, State}
            end;
        _ ->
            {reply, {ok, Pid}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, _}, State=#state{connection_pid =Pid}) ->
    mysql_connections:remove(Pid),
    {noreply, State#state{connection_pid =undefined}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% internal methods

get_connection(PoolName, Options) ->
    NewOptions = get_connection_options(PoolName, Options),
    case catch mysql:start_link(NewOptions) of
        {ok, Pid} ->
            case mysql_connections:add(Pid) of
                ok ->
                    {ok, Pid};
                UnexpectedError ->
                    exit(Pid, kill),
                    {error, UnexpectedError}
            end;
        UnexpectedError ->
            {error, UnexpectedError}
    end.

get_connection_options(PoolName, Options) ->
    RuntimePrepares = mysql_prepared_stm_utils:to_list(PoolName),

    case mysql_utils:lookup(prepare, Options) of
        null ->
            [{prepare, RuntimePrepares} | Options];
        ConfigPreps when is_list(ConfigPreps) ->
            mysql_utils:replace(prepare, RuntimePrepares ++ ConfigPreps, Options)
    end.
