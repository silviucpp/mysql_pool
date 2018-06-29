-module(mysql_pool_sup).

-behaviour(supervisor).

-export([
    start_link/0,
    add_pool/2,
    remove_pool/1,
    init/1
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add_pool(PoolName, ChildSpecs) ->
    case supervisor:start_child(?MODULE, ChildSpecs) of
        {error, already_present} ->
            supervisor:restart_child(?MODULE, PoolName);
        Other ->
            Other
    end.

remove_pool(PoolName) ->
    case supervisor:terminate_child(?MODULE, PoolName) of
        ok ->
            supervisor:delete_child(?MODULE, PoolName);
        Error ->
            Error
    end.

init([]) ->
    Childrens = [
        worker(mysql_connection_manager, infinity)
    ],

    {ok, {{one_for_one, 10, 10}, Childrens}}.

worker(Name, WaitForClose) ->
    children(Name, WaitForClose, worker).

children(Name, WaitForClose, Type) ->
    {Name, {Name, start_link, []}, permanent, WaitForClose, Type, [Name]}.