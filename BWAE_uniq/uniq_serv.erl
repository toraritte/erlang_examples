-module(uniq_serv).
-behaviour(gen_server).

%%% API
-export([get_id/0, start_link/0, stop/0]).
%%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         % handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {count}).

%%% === API ====
%%% ============
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_id() ->
    {id, ID} = gen_server:call(?MODULE, id),
    ID.

stop() ->
    gen_server:call(?MODULE, stop).

%%% === gen_server callbacks ====
%%% =============================
init([]) ->
    {ok, #state{count= 1 }}.

handle_call(id, _From, S=#state{count = C}) ->
    {reply, {id, C}, S#state{count= C+1}};
handle_call(stop, _From, State) ->
    {stop, normal, stopping, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

% handle_info(_Msg, State) ->
%     {noreply, State}.

terminate(normal, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
