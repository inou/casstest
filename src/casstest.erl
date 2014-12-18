-module(casstest).

-include("casstest.hrl").

-export([create_user/0,
         create_event/0,
         update_events_set/2,
         update_events_pk/2,
         read_set/1,
         read_pk/1]).


-export([statements/0]).

-export([start/0, stop/0]).
-export([get_env/1, get_env/2]).


start() ->
    reltool_util:application_start(?MODULE).

stop() ->
    reltool_util:application_stop(?MODULE).

-spec get_env(atom()) -> any().
get_env(Name) ->
    application:get_env(casstest, Name).

-spec get_env(atom(), any()) -> any().
get_env(Name, Default) ->
    application:get_env(casstest, Name, Default).


-spec create_user() -> user_id().
create_user() ->
    UserId = get_timeuuid(),
    Name = get_text(),
    {ok, void} = cassclient:execute({?NEW_USER, [UserId, Name]}),
    UserId.

-spec create_event() -> event_id().
create_event() ->
    EventId = get_timeuuid(),
    Description = get_text(),
    {ok, void} = cassclient:execute({?NEW_EVENT, [EventId, Description]}),
    EventId.

-spec update_events_set(user_id(), event_id()) -> update_stmt().
update_events_set(UserId, EventId) ->
    {?UPDATE_SET, [[EventId], UserId]}.

-spec update_events_pk(user_id(), event_id()) -> update_stmt().
update_events_pk(UserId, EventId) ->
    {?UPDATE_PK, [UserId, EventId]}.

-spec read_set(uuid:uuid()) -> {ok, void}.
read_set(UserId) ->
    cassclient:execute({?READ_SET, [UserId]}).

-spec read_pk(uuid:uuid()) -> {ok, void}.
read_pk(UserId) ->
    cassclient:execute({?READ_PK, [UserId]}).

statements() ->
    [{?NEW_USER,
      <<"INSERT INTO casstest.users (id, name) VALUES (?,?);">>},
     {?NEW_EVENT,
      <<"INSERT INTO casstest.events(id, description) VALUES (?,?);">>},
     {?UPDATE_SET,
      <<"UPDATE casstest.events1 SET events = events +? WHERE user = ?;">>},
     {?UPDATE_PK,
      <<"INSERT INTO casstest.events2 (user, event) VALUES (?, ?);">>},
     {?READ_SET,
      <<"SELECT events FROM casstest.events1 WHERE user = ?;">>},
     {?READ_PK,
      <<"SELECT event FROM casstest.events2 WHERE user = ?;">>}].


get_timeuuid() ->
    U = uuid:new(self(), [{timestamp_type, os}]),
    U1 = uuid:get_v1(U),
    uuid:uuid_to_string(U1, binary_standard).

get_text() ->
    base64:encode(crypto:rand_bytes(15)).

