-module(mysql_utils).

-export([
    env/1,
    lookup/2,
    lookup/3,
    replace/3,
    delete/2,
    as_json/1,
    as_json/2
]).

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

as_json({ok, Columns, Rows}) ->
    as_json(Columns, Rows).

as_json(Columns, Rows) ->
    [begin [{K, json_val(V)} || {K, V} <- lists:zip(Columns, Row)] end || Row <- Rows].

json_val(Value) when is_tuple(Value) ->
    % convert datetime, date, time into string
    % decimals seconds are not supported yet. so for the moment are truncated.
    case Value of
        {{Year, Month ,Day}, {Hour, Min, Sec}} ->
            iolist_to_binary(io_lib:format("~4.4.0w-~2.2.0w-~2.2.0wT~2.2.0w:~2.2.0w:~2.2.0wZ", [Year, Month, Day, Hour, Min, trunc(Sec)]));
        {Year, Month, Day} ->
            iolist_to_binary( io_lib:format("~4.4.0w-~2.2.0w-~2.2.0w", [Year, Month, Day]));
        {Days, {Hour, Min, Sec}} ->
            iolist_to_binary( io_lib:format("~2.2.0w:~2.2.0w:~2.2.0w", [Days*24+Hour, Min, trunc(Sec)]))
    end;
json_val(Value) ->
    Value.