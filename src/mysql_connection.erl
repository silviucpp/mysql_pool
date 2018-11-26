-module(mysql_connection).

-include("mysql_pool.hrl").

-define(UNKNOWN_PREPARE_STM, 1243).

-export([
    start_link/1, stop/1,
    query/2, query/3, query/4,
    execute/3, execute/4,
    query_opt/3, query_opt/4, query_opt/5,
    execute_opt/4, execute_opt/5,
    prepare/2, prepare/3, unprepare/2,
    warning_count/1, affected_rows/1, autocommit/1, insert_id/1,
    encode/2, in_transaction/1,
    transaction/2, transaction/3, transaction/4
]).

start_link(Options) ->
    mysql:start_link(Options).

stop(Conn) ->
    exit(Conn, kill).

query(Conn, Query) ->
    mysql:query(Conn, Query).

query(Conn, Query, Params) ->
    mysql:query(Conn, Query, Params).

query(Conn, Query, Params, Timeout) ->
    mysql:query(Conn, Query, Params, Timeout).

query_opt(Conn, Query, OptionFlag) ->
    transform_result(Conn, mysql:query(Conn, Query), OptionFlag).

query_opt(Conn, Query, Params, OptionFlag) ->
    transform_result(Conn, mysql:query(Conn, Query, Params), OptionFlag).

query_opt(Conn, Query, Params, Timeout, OptionFlag) ->
    transform_result(Conn, mysql:query(Conn, Query, Params, Timeout), OptionFlag).

execute(Conn, StatementRef, Params) ->
    execute_stm(Conn, StatementRef, Params, null).

execute(Conn, StatementRef, Params, Timeout) ->
    execute_stm(Conn, StatementRef, Params, Timeout).

execute_opt(Conn, StatementRef, Params, OptionFlag) ->
    transform_result(Conn, execute_stm(Conn, StatementRef, Params, null), OptionFlag).

execute_opt(Conn, StatementRef, Params, Timeout, OptionFlag) ->
    transform_result(Conn, execute_stm(Conn, StatementRef, Params, Timeout), OptionFlag).

prepare(Conn, Query) ->
    mysql:prepare(Conn, Query).

prepare(Conn, Name, Query) ->
    mysql:prepare(Conn, Name, Query).

unprepare(Conn, StatementRef) ->
    mysql:unprepare(Conn, StatementRef).

warning_count(Conn) ->
    mysql:warning_count(Conn).

affected_rows(Conn) ->
    mysql:affected_rows(Conn).

autocommit(Conn) ->
    mysql:autocommit(Conn).

insert_id(Conn) ->
    mysql:insert_id(Conn).

in_transaction(Conn) ->
    mysql:in_transaction(Conn).

transaction(Conn, Fun) ->
    transaction(Conn, Fun, [], infinity).

transaction(Conn, Fun, Retries) ->
    transaction(Conn, Fun, [], Retries).

transaction(Conn, Fun, Args, Retries) ->
    case catch mysql:transaction(Conn, Fun, Args, Retries) of
        {atomic, _} = S ->
            S;
        {aborted, _} = A ->
            A;
        Error ->
            ?ERROR_MSG("mysql:transaction unexpected response for connection: ~p error: ~p", [Conn, Error]),
            stop(Conn),
            Error
    end.

encode(Conn, Term) ->
    mysql:encode(Conn, Term).

% internal methods

transform_result(_Pid, Rs, null) ->
    Rs;
transform_result(Pid, Rs, Opt) ->
    case Rs of
        ok ->
            case Opt of
                affected_rows ->
                    {ok, mysql:affected_rows(Pid)};
                insert_id ->
                    {ok, mysql:insert_id(Pid)};
                both ->
                    {ok, {mysql:affected_rows(Pid), mysql:insert_id(Pid)} }
            end;
        _ ->
            Rs
    end.

execute_stm(Pid, Statement, Params, Timeout) ->
    Rs = case Timeout of
        null ->
            mysql:execute(Pid, Statement, Params);
        _ ->
            mysql:execute(Pid, Statement, Params, Timeout)
    end,

    case Rs of
        {error, {?UNKNOWN_PREPARE_STM, _, _}} ->
            {ok, PoolName} = mysql_connection_manager:get_connection_pool(Pid),

            case reprepare_evicted_stm(PoolName, Pid, Statement) of
                ok ->
                    execute_stm(Pid, Statement, Params, Timeout);
                Error ->
                    ?ERROR_MSG("failed to reprepare evicted statement: ~p", [Error]),
                    Rs
            end;
        _ ->
            Rs
    end.

reprepare_evicted_stm(PoolName, Pid, Stm) ->
    ?WARNING_MSG("reprepare evicted statement: ~p", [Stm]),

    case mysql_connection_manager:pool_get_statement(PoolName, Stm) of
        null ->
            {error, prepare_key_evicted};
        {ok, Stm, Query} ->
            case mysql:prepare(Pid, Stm, Query) of
                {ok, Stm} ->
                    ok;
                Error ->
                    Error
            end
    end.