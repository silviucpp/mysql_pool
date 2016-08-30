-module(mysql_pool_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Childrens = [
        supervisor(pooler_sup, infinity),
        worker(mysql_connection_manager, infinity)
    ],

    {ok, {{one_for_one, 10, 10}, Childrens}}.

worker(Name, WaitForClose) ->
    children(Name, WaitForClose, worker).

supervisor(Name, WaitForClose) ->
    children(Name, WaitForClose, supervisor).

children(Name, WaitForClose, Type) ->
    {Name, {Name, start_link, []}, permanent, WaitForClose, Type, [Name]}.