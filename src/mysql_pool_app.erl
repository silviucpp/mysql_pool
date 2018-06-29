-module(mysql_pool_app).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_StartType, _StartArgs) ->

    {ok, Pid} = mysql_pool_sup:start_link(),

    case mysql_utils:env(pools) of
        {ok, Pools} ->
            load_pools(Pools);
        _ ->
            ok
    end,

    {ok, Pid}.

stop(_State) ->
    ok.

load_pools(Pools) ->
    FunPool = fun({PoolName, Options}) ->
        Size = mysql_utils:lookup(size, Options, 20),
        MaxOverflow = mysql_utils:lookup(max_overflow, Options, 0),
        ConnectionOptions = mysql_utils:lookup(connection_options, Options, []),
        {ok, _} = mysql_pool:add_pool(PoolName, Size, MaxOverflow, ConnectionOptions)
    end,
    ok = lists:foreach(FunPool, Pools).