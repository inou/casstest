-module(casstest).

-include("casstest.hrl").

-export([create_user/0,
         create_event/0,
         update_events_set/2,
         update_events_pk/2,
         time_updates/0,
         time_reads/0]).


-export([statements/0]).

create_user() ->
    U1 = get_timeuuid(),
    Name = get_text(),
    {U1, {?NEW_USER, [U1, Name]}}.

create_event() ->
    U1 = get_timeuuid(),
    Description = get_text(),
    {U1, {?NEW_EVENT, [U1, Description]}}.

update_events_set(User, Event) ->
    {?UPDATE_SET, [Event, User]}.

update_events_pk(User, Event) ->
    {?UPDATE_PK, [User, Event]}.

time_updates() ->
    ok.

time_reads() ->
    ok.

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
