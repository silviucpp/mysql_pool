
% logs

-include_lib("kernel/include/logger.hrl").

-define(LOG_PRINT(Format, Args),
    io:format(Format, Args)).

% data types

-type reason() :: any().
-type pool_id() :: atom().
-type stm_id() :: atom().
-type opt_flag() :: affected_rows|insert_id|both.
