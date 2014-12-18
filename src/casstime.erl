-module(casstime).

-include("casstest.hrl").

-export([setup/2]).
-export([test_set/1, test_pk/1]).
-export([avg/1]).


setup(UN, EN) ->
    Users = create_users(UN),
    Events = create_events(EN),
    [update_events_set(UserId, Events) || UserId <- Users],
    [update_events_pk(UserId, Events) || UserId <- Users],
    Users.

test_set(Users) ->
    [time_read_set(UserId) || UserId <- Users].

test_pk(Users) ->
    [time_read_pk(UserId) || UserId <- Users].

avg([]) -> {error, empty_list};
avg(Times) ->
    Times1 = [T || {T, _} <- Times],
    lists:sum(Times1)/length(Times1).


-spec create_users(pos_integer()) -> list(user_id()).
create_users(N) ->
    [casstest:create_user() || _ <- lists:seq(1, N)].

-spec create_events(pos_integer()) -> list(event_id()).
create_events(N) ->
    [casstest:create_event() || _ <- lists:seq(1, N)].

-spec update_events_set(user_id(), list(event_id())) -> ok.
update_events_set(UserId, Events) ->
    Statements =
        [casstest:update_events_set(UserId, EventId) || EventId <- Events],
    cassclient:batch(Statements),
    ok.

-spec update_events_pk(user_id(), list(event_id())) -> ok.
update_events_pk(UserId, Events) ->
    Statements =
        [casstest:update_events_pk(UserId, EventId) || EventId <- Events],
    cassclient:batch(Statements),
    ok.

-spec time_read_set(user_id()) -> {integer(), term()}.
time_read_set(UserId) ->
    timer:tc(casstest, read_set, [UserId]).

-spec time_read_pk(user_id()) -> {integer(), term()}.
time_read_pk(UserId) ->
    timer:tc(casstest, read_pk, [UserId]).
