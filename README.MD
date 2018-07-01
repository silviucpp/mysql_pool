# mysql_pool

mysql_pool provides connection pooling for [MySQL/OTP][1] using [Poolboy][2]. It contains convenience functions for executing SQL queries on a mysql connection in a pool.

Use always the methods from `mysql_connection` in case you need to play with a certain connection from the pool. This module has logic to reprepare and execute the query again in case the server evicted a prepare statement. The [MySQL/OTP][1] is not treating this case at this moment.

### Notes

`mysql_pool` it's using a fork of `mysql-otp` which it's slightly different having the following changes:

- [All floats are encoded as decimals withg 8 digit precision][6]

In case you want to use the master branch of `mysql-otp` or a specific version change this in `rebar.config` ot bind to the version you want.
The interfaces are 100 % compatible so should work without any problem.

### Getting starting:

Using `rebar`, put this in your `rebar.config`:

```erlang
{deps, [
    {mysql_pool, ".*", {git, "https://github.com/silviucpp/mysql_pool.git", {tag, "1.6"}}}
]}.
```

Examples:


```erlang
ok = mysql_pool:start(),
mysql_pool:prepare(pool_one, get_accounts, <<"SELECT id, fname, lname, email FROM accounts WHERE id = ?">>).
mysql_pool:execute(pool_one, get_accounts, [1]).

mysql_pool:transaction(pool_name, fun (Pid) ->
       ok = mysql_connection:query(Pid, "INSERT INTO foo(id, name) VALUES (?, ?)", [1, <<"banana">>]),
       ok = mysql_connection:query(Pid, "INSERT INTO foo(id, name) VALUES (?, ?)", [2, <<"kiwi">>])
end).
```

### Config

```erlang
    {mysql_pool, [
        {pools, [
            {pool_one, [
                {size, 10},
                {connection_options, [
                    {host, "localhost"},
                    {user, "root"},
                    {password, "root"},
                    {database, "db_name"}
                ]}
            ]}
        ]}
    ]}
```

Each pool has the following settings:

- `size`: The size of the pool, default value is `5` if omitted.
- `connection_options` - The options that are used to start each connection. The list of available settings are:
    - `{host, Host}` - Hostname of the MySQL server, default value is `localhost`.
    - `{port, Port}` - Port where MySQL server listen, default value is `3306` if omitted.
    - `{user, User}` - Username used to connect to the database
    - `{password, Password}` - Password used to connect to the database
    - `{database, Database}` - The name of the database AKA schema to use
    - `{connect_timeout, Timeout}` - The maximum time to spend for connecting to the server
    - `{log_warnings, boolean()}` - Whether to fetch warnings and log them using error_logger, default `true`.
    - `{keep_alive, boolean() | timeout()}` - Send ping when unused for a certain time. Possible values are `true`, `false` or `integer() > 0` for an explicit interval in milliseconds. The default is `false`. For `true` a default ping timeout is used.
    - `{prepare, NamedStatements}` - Named prepared statements to be created as soon as the connection is ready. Can be created also at runtime using `mysql_pool:prepare/3`
    - `{queries, Queries}` - Queries to be executed as soon as the connection is ready. Any results are discarded. Typically, this is used for setting time zone and other session variables.
    - `{query_timeout, Timeout}` - The default time to wait for a response when executing a query or a prepared statement. This can be given per query using `query/3,4` and `execute/4`. The default is `infinity`.
    - `{found_rows, boolean()}` - If set to true, the connection will be established with CLIENT_FOUND_ROWS capability. affected_rows/1 will now return the number of found rows, not the number of rows changed by the query
    - `{query_cache_time, Timeout}` - The minimum number of milliseconds to cache prepared statements used for parametrized queries with `query/3`.
    - `{tcp_options, Options}` - Additional options for `gen_tcp:connect/3`. You may want to set `{recbuf, Size}` and `{sndbuf, Size}` if you send or receive more than the default (typically 8K) per query.

### API

The `mysql-otp` documentation is available [here][3]. `mysql_connection` is only a wrapper around `mysql` interface from `mysql-otp`, It's accepting the same arguments and returns the values in the same way.

Also it provides few additional methods like: `query_opt/3, query_opt/4, query_opt/5` and `execute_opt/4, execute_opt/5` where the last argument (`OptionFlag`) is an atom argument that can take one for the following values:
- affected_rows - will transform all `ok` results into `{ok, NumberOfAffectedRows}`
- insert_id - will transform all `ok` results into `{ok, LastInsertId}`
- both - will transform all `ok` results into `{ok, {NumberOfAffectedRows, LastInsertId}}`

Beside this the `mysql_pool` comes with the following methods

- `add_pool(PoolName, Size, MaxOverflow, ConnectionOptions)` - Add a pool at runtime with a specific size and connection options
- `remove_pool(PoolName)` - Remove a pool at runtime
- `prepare(PoolName, Stm, Query)` - Add a prepare statement for a pool. The statement will be added to all existing connections and also on all new connections that might be created in the future
- `unprepare(PoolName, Stm)` - Remove a prepare statement from a pool. The statement will be unprepare on all existing connections as well.
- `query/2, query/3, query/4` - Run a query over a connection in a specific pool
- `execute/3, execute/4` - Run a prepare statement over a connection in a specific pool
- `query_opt/3, query_opt/4, query_opt/5` - Run a query over a connection in a specific pool and returns additional info's for `ok` results as it's described above
- `execute_opt/4, execute_opt/5` - Run a prepare statement over a connection in a specific pool and returns additional info's for `ok` results as it's described above
- `transaction/2, transaction/3, transaction/4` - Start a transaction over a connection in a specified pool
- `with(PoolName, Fun)` - Pick a connection from the pool `PoolName` and call the specified method using the connection as argument : `Fun(Pid)`.

### Utils

#### Converting Row Data To JSON

Using `mysql_utils:as_json` you are getting back a list of erlang terms in accordance with EEP-18. For actual utf8 binary JSON string you will need external library like [jsx][4] or [jiffy][5]

Example:

```erl
Result = mysql_pool:query(pool1, <<"select bar, baz from foo">>),
JSON = mysql_utils:as_json(Result).
#[[{<<"bar">>,<<"bar_value">>}, {<<"baz">>,<<"baz_value">>}], ...]
```

[1]:https://github.com/mysql-otp/mysql-otp
[2]:https://github.com/devinus/poolboy
[3]:https://mysql-otp.github.io/mysql-otp/index.html
[4]:https://github.com/talentdeficit/jsx
[5]:https://github.com/davisp/jiffy
[6]:https://github.com/mysql-otp/mysql-otp/issues/52