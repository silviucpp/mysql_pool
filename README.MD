# mysql_pool

mysql_pool provides connection pooling for [MySQL/OTP][1] using [Pooler][2]. It contains convenience functions for executing SQL queries on a mysql connection in a pool.  

Use always the methods from `mysql_connection` in case you need to play with a certain connection from the pool. This module has logic to reprepare and execute the query again in case the server evicted a prepare statement. The [MySQL/OTP][1] is not treating this case at this moment.   

### Getting starting:

Using `rebar`, put this in your `rebar.config`:

```erlang
{deps, [
    {mysql_pool, ".*", {git, "https://github.com/silviucpp/mysql_pool.git", {tag, "1.0.0"}}}
]}.
```

Examples:


```erlang
ok = mysql_pool:start(),
mysql_pool:prepare(pool_one, get_accounts, <<"SELECT id, fname, lname, email FROM accounts WHERE id = ?">>).
mysql_pool:execute(pool_one, get_accounts, [1]).
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
    - `{query_cache_time, Timeout}` - The minimum number of milliseconds to cache prepared statements used for parametrized queries with `query/3`.
    - `{tcp_options, Options}` - Additional options for `gen_tcp:connect/3`. You may want to set `{recbuf, Size}` and `{sndbuf, Size}` if you send or receive more than the default (typically 8K) per query.

[1]:https://github.com/mysql-otp/mysql-otp
[2]:https://github.com/seth/pooler
