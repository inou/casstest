-module(casstest_app).

-behaviour(application).

-include("casstest.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    start_cassandra_pools(),
    casstest_sup:start_link().

stop(_State) ->
    ok.

start_cassandra_pools() ->
    {ok, PoolHost} = casstest:get_env(cassandra_pool_host),
    {ok, PoolPort} = casstest:get_env(cassandra_pool_port),
    {ok, PoolSize} = casstest:get_env(cassandra_pool_size),
    {ok, PoolOverflow} = casstest:get_env(cassandra_pool_overflow),

    Pool = [{size, PoolSize}, {max_overflow, PoolOverflow},
            {worker_module, cassclient}],

    PoolOptions = [{host, PoolHost},
                   {port, PoolPort},
                   {use, ?CASSTEST_KEYSPACE},
                   {keepalive, true},
                   {cql_version, <<"3.1.1">>},
                   {auto_reconnect, true},
                   {prepare, casstest:statements()}],

    {ok, _} = erlcql_poolboy:start_link(?CASSTEST_POOL,
                                        Pool, PoolOptions),
    ok.
