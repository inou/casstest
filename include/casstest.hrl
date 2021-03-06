-define(NEW_USER, new_user).
-define(NEW_EVENT, new_event).
-define(UPDATE_SET, update_set).
-define(UPDATE_PK, update_pk).
-define(READ_SET, read_set).
-define(READ_PK, read_pk).

-define(CASSTEST_KEYSPACE, <<"casstest">>).
-define(CASSTEST_POOL, casstest_pool).

-type user_id() :: uuid:uuid().
-type event_id() :: uuid:uuid().
-type update_stmt() :: {atom(), [user_id() | event_id()]}.
