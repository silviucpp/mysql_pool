-module(mysql_utils).
-author("silviu").

-export([env/1, lookup/2, lookup/3, replace/3, delete/2]).

env(Key) ->
    application:get_env(mysql_pool, Key).

lookup(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Result} ->
            Result;
        false ->
            Default
    end.

lookup(Key, List) ->
    lookup(Key, List, null).

replace(Key, NewValue, List) ->
    case lookup(Key, List) of
        null ->
            [{Key, NewValue} | List];
        _ ->
            lists:keyreplace(Key, 1, List, {Key, NewValue})
    end.

delete(Key, List) ->
    lists:keydelete(Key, 1, List).