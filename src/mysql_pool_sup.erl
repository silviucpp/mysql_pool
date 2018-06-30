-module(mysql_pool_sup).

-behaviour(supervisor).

-export([
    start_link/1,
    add_pool/2,
    remove_pool/1,
    has_pool/1,
    init/1
]).

start_link(AppPid) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [AppPid]).

add_pool(_PoolName, ChildSpecs) ->
    supervisor:start_child(?MODULE, ChildSpecs).

remove_pool(PoolName) ->
    case supervisor:terminate_child(?MODULE, PoolName) of
        ok ->
            supervisor:delete_child(?MODULE, PoolName);
        Error ->
            Error
    end.

has_pool(PoolName) ->
    case supervisor:get_childspec(?MODULE, PoolName) of
        {ok, _} ->
            true;
        {error, not_found} ->
            false
    end.

init(Args) ->
    Childrens = [
        worker(mysql_connection_manager, infinity, Args)
    ],

    {ok, {{one_for_one, 10, 10}, Childrens}}.

worker(Name, WaitForClose, Args) ->
    children(Name, WaitForClose, worker, Args).

children(Name, WaitForClose, Type, Args) ->
    {Name, {Name, start_link, Args}, permanent, WaitForClose, Type, [Name]}.