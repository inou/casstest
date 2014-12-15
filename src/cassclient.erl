-module(cassclient).

-behaviour(gen_server).

-export([start_link/0,start_link/1]).
-export([execute/1, batch/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
          client :: pid()
         }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    start_link([]).

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

execute(Query) ->
    gen_server:call(?MODULE, {execute, Query}).

batch(Queries) ->
    gen_server:call(?MODULE, {batch, Queries}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    {ok, Pid} = erlcql_client:start_link(Opts),
    {ok, #state{client=Pid}}.

handle_call({execute, {QueryName, Values}}, _From,
            #state{client=Client}=State) ->
    Reply = erlcql_client:execute(Client, QueryName, Values,
                                  [{consistency, quorum}]),
    {reply, Reply, State};

handle_call({batch, Queries}, _From, #state{client=Client}=State) ->
    Reply = erlcql_client:batch(Client, Queries, [{consistency, quorum}]),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
