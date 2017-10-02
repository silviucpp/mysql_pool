
-type reason() :: any().
-type pool_id() :: atom().
-type stm_id() :: atom().
-type opt_flag() :: affected_rows|insert_id|both.

-define(PRINT_MSG(Format, Args),
    io:format(Format, Args)).

-define(DEBUG_MSG(Format, Args),
    lager:debug(Format, Args)).

-define(INFO_MSG(Format, Args),
    lager:info(Format, Args)).

-define(WARNING_MSG(Format, Args),
    lager:warning(Format, Args)).

-define(ERROR_MSG(Format, Args),
    lager:error(Format, Args)).

-define(CRITICAL_MSG(Format, Args),
    lager:critical(Format, Args)).
