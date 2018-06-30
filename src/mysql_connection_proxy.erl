-module(mysql_connection_proxy).

-behaviour(gen_server).

-export([

    start_link/1,
    get_pid/1,

    % gen_server

    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    pool_name,
    connection_pid,
    connection_args
}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

get_pid(ProxyPid) ->
    gen_server:call(ProxyPid, get_connection_pid, infinity).

init([PoolName, Args]) ->
    process_flag(trap_exit, true),

    ConnectionPid = case get_connection(PoolName, Args) of
        {ok, NewPid} ->
            NewPid;
        _ ->
            undefined
    end,

    {ok, #state{connection_args = Args, pool_name = PoolName, connection_pid = ConnectionPid}}.

handle_call(get_connection_pid, _From, #state{connection_pid = Pid} = State) ->
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

handle_info({'EXIT', ConnectionPid, _}, #state{connection_pid = ConnectionPid} = State) ->
    mysql_connection_manager:remove_connection(ConnectionPid),
    {noreply, State#state{connection_pid = undefined}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{connection_pid = ConnectionPid}) ->
    case ConnectionPid of
        undefined ->
            ok;
        _ ->
            mysql_connection_manager:remove_connection(ConnectionPid)
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% internal methods

get_connection(PoolName, Options) ->
    NewOptions = get_connection_options(PoolName, Options),
    case catch mysql_connection:start_link(NewOptions) of
        {ok, Pid} ->
            case mysql_connection_manager:add_connection(Pid, PoolName) of
                ok ->
                    {ok, Pid};
                UnexpectedError ->
                    mysql_connection:stop(Pid),
                    {error, UnexpectedError}
            end;
        UnexpectedError ->
            {error, UnexpectedError}
    end.

get_connection_options(PoolName, Options) ->
    RuntimePrepares = mysql_connection_manager:pool_get_statements(PoolName),

    case mysql_utils:lookup(prepare, Options) of
        null ->
            [{prepare, RuntimePrepares} | Options];
        ConfigPreps when is_list(ConfigPreps) ->
            mysql_utils:replace(prepare, RuntimePrepares ++ ConfigPreps, Options)
    end.
