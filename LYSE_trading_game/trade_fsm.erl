%%% http://learnyousomeerlang.com/finite-state-machines#a-trading-system-specification
-module(trade_fsm).
-behaviour(gen_fsm).

%% public API
-export([start/1, start_link/1, trade/2, accept_trade/1,
         make_offer/2, retract_offer/2, ready/1, cancel/1]).
%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4,
         % custom state names
         idle/2, idle/3, idle_wait/2, idle_wait/3, negotiate/2,
         negotiate/3, wait/2, ready/2, ready/3]).

%%% ================================================
%%% PUBLIC API
%%% ================================================
start(Name) ->
    gen_fsm:start(?MODULE, [Name],[]).

start_link(Name) ->
    gen_fsm:start_link(?MODULE, [Name], []).

%% ask for beginning a trade. Return when/if the other accepts.
trade(OwnPid, OtherPid) ->
    %       sync_send_event(FsmRef, Event,                 Timeout)
    gen_fsm:sync_send_event(OwnPid, {negotiate, OtherPid}, 30000).
    % FsmRef =:= YourFSMpid because these functions for the client to
    % communicate with their associated FSM (OtherPid belongs to
    % the FSM whose client we'd like to reach.
    %
    %   YOU      <--->   yourFSM    <------->    otherFSM     <--->    OTHER
    %  shell              <toby>                  <creed>              shell
    %  (pidB)            (OwnPid)               (OtherPid)             (pidA)

%% accept trade offer
accept_trade(OwnPid) ->
    %       sync_send_event(FsmRef, Event)
    gen_fsm:sync_send_event(OwnPid, accept_negotiate).
    % As a reminder, this whole module is only a bit more
    % complicated version of:
    %
    %    send(ServerPid, Message) ->
    %        ServerPid ! Message,
    %        receive
    %            Reply -> Reply
    %        end.
    %
    %    start() -> spawn(?MODULE, loop, []).
    %
    %    loop() ->
    %        receive
    %            Message ->
    %                handle_message(Message),
    %                loop()
    %        end.
    %
    %    Where send/2 is in another module providing this
    %    functionality and loop/1 is represented by multiple
    %    recursive functions referring to each other in different
    %    receive branches.
    %
    %    stateA() ->
    %        receive
    %            aaa -> stuff(), state();
    %            bbb -> stateB();
    %            ccc -> stateC();
    %            stop -> stop
    %        end.
    %
    %    stateB() ->

%% send an item to be traded
make_offer(OwnPid, Item) ->
    gen_fsm:send_event(OwnPid,{make_offer, Item}).

%% cancel offer
retract_offer(OwnPid, Item) ->
    gen_fsm:send_event(OwnPid, {retract_offer, Item}).

%% Player announces that the trade is ready. When other
%% player does the same, the trade is done.
ready(OwnPid) ->
    gen_fsm:sync_send_event(OwnPid, ready, infinity).

%% cancel transaction
cancel(OwnPid) ->
    gen_fsm:sync_send_all_state_event(OwnPid, cancel).

%%% ================================================
%%% FSM to FSM FUNCTIONS
%%% ================================================
%% Functions in this section are all for interFSM communication,
%%
%%   YOU      <--->   yourFSM    <------->    otherFSM     <--->    OTHER
%% yourPID           YourFSMpid      ^      OtherFSMpid            otherPID
%%                                   |
%%                               you are here

%%% The ask_negotiate/2 asks the other FSM if they want to trade,
%%% and accept_negotiate/2 replies to it (asynchronously, of course).

%% Ask the other FSM's PID for a trade session
ask_negotiate(OtherPid,OwnPid) ->
    gen_fsm:send_event(OtherPid, {ask_negotiate, OwnPid}).

%% Forward the client message accepting the transaction
accept_negotiate(OtherPid, OwnPid) ->
    gen_fsm:send_event(OtherPid, {accept_negotiate, OwnPid}).

%% forward a client's offer
do_offer(OtherPid, Item) ->
    gen_fsm:send_event(OtherPid, {do_offer, Item}).

%% forward a client's offer cancellation
undo_offer(OtherPid, Item) ->
    gen_fsm:send_event(OtherPid, {undo_offer, Item}).

%% ask the other side whether they are ready
%% to finish the trade and exchange items
are_you_ready(OtherPid) ->
    gen_fsm:send_event(OtherPid, are_you_ready).

%% Tells the other fsm that user is currently waiting for the
%% READY state. State should trasition to READY.
am_ready(OtherPid) ->
    gen_fsm:send_event(OtherPid, am_ready).

%% The side is not ready to close the deal yet
%% ie. is not in WAIT state.
not_yet(OtherPid) ->
    gen_fsm:send_event(OtherPid, not_yet).

%% Acknowledge that the FSM(s) are in READY state
ack_ready(OtherPid) ->
    gen_fsm:send_event(OtherPid, ack_ready).

%% ask if ready to commit
ask_commit(OtherPid) ->
    gen_fsm:sync_send_event(OtherPid, ask_commit).

%% begin the synchronous commit
do_commit(OtherPid) ->
    gen_fsm:sync_send_event(OtherPid, do_commit).

%% courtesy function to let the other FSM know
%% that we cancelled the trade
notify_cancel(OtherPid) ->
    gen_fsm:send_all_state_event(OtherPid, cancel).


%%% ================================================
%%% GEN_FSM CALLBACKS
%%% ================================================
-record(state,{name="",    % the FSM gets named after the player
               otherFSM,
               ownitems=[],
               otheritems=[],
               monitor,    % monitor reference to know when otherFSM dies
               from}).     % player 'address' (YOU or OTHER) as {Pid, Tag}

init(Name) ->
   %{ok, StartStateName, StateData
    {ok, idle,           #state{name=Name}}.

%% ASYNC idle/2 when a player FSM receives a request to trade
idle({ask_negotiate, OtherPid},S=#state{}) ->
    Ref = erlang:monitor(process, OtherPid),
    notice(S,"~p asked for a trade negotiation", [OtherPid]),
    {next_state, idle_wait, S#state{otherFSM=OtherPid, monitor=Ref}};
idle(Event, StateData)->
    unexpected(Event, StateData),
    {next_state, idle, StateData}.

%% SYNC idle/3 when a player wants to start a trade (the players process sends
%% this synchronous message to his/her FSM and the above async idle/2 will forward
%% the request to the opponents FSM.
idle({negotiate, OtherPid}, From, S=#state{}) ->
    ask_negotiate(OtherPid,self()),
    % Notifying the player(s) (ie printing it in the shell)
    notice(S, "asking user ~p for a trade", [OtherPid]),
    Ref = erlang:monitor(process, OtherPid),
    {next_state, idle_wait, S#state{otherFSM=OtherPid, monitor=Ref, from=From}};
idle(Event, _From, StateData) ->
    unexpected(Event, idle),
    {next_state, idle, StateData}.

%% The FSMs will move to IDLE_WAIT state when
%% 1. player ---trade---> FSM
%% 2. FSM ---ask_nego---> FSM
%%
%% The FSM in IDLE_WAIT state will only get 'ask_negotiate' if
%% the 2 players started a negotiation towards (and independently of)
%% each other. A race condition because both FSMs transition to
%% IDLE_WAIT after shooting off ask_negotiate.
%%
%% yourFSM                               otherFSM
%% [IDLE]  ->->-> ask_nego  ask_nego <-<- [IDLE]
%%   |                    \/                |
%% [IDLE_WAIT] <-<-<-<-<--/\->->->->->->- [IDLE_WAIT]
%%
%% ASYNC state
idle_wait({ask_negotiate, OtherPid}, S=#state{otherFSM=OtherPid}) ->
    % In this scenario both players are waiting for feedback on their
    % request and they both get it (IDLE never sends replies in any
    % of its clauses).
    gen_fsm:reply(S#state.from, ok),
    notice(S, "startig negotiation", []),
    % Moving on to NEGOTIATE state because this special clause only
    % happens if both sides would like the same thing: trade.
    {next_state, negotiate, S};
idle_wait({accept_negotiate, OtherPid},S=#state{otherFSM=OtherPid}) ->
    gen_fsm:reply(S#state.from, ok),
    notice(S, "startig negotiation", []),
    {next_state, negotiate, S};
idle_wait(Event,StateData) ->
    unexpected(Event, idle_wait),
    {next_state, idle_wait, StateData}.

%% Accepting the SYNC calls from players that will be forwarded
%% to the otherFSM.
%% _From not needed because the result tuple is a {reply, ...}
idle_wait(accept_negotiate, _From, S=#state{otherFSM=OtherPid}) ->
    accept_negotiate(OtherPid, self()),
    notice(S, "accepting negotiation", []),
    {reply, ok, negotiate, S};
idle_wait(Event,_From, StateData) ->
    unexpected(Event, idle_wait),
    {next_state, idle_wait, StateData}.

%% Negotiation protocol (ASYNC) between
%%     FSM-to-FSM (do_*, undo_*)
%%  player-to-FSM (make_*, retract_*).
%%
%% The message tags are arbitrary we just needed it to
%% differentiate between the different interactions as
%% both FSM-to-FSM and player-to-FSM messages are async.
negotiate({make_offer, Item}, S=#state{ownitems=I}) ->
    do_offer(S#state.otherFSM, Item),
    notice(S, "offers ~p", [Item]),
    {next_state, negotiate, S#state{ownitems=add(Item,I)}};
negotiate({do_offer, Item}, S=#state{otheritems=O}) ->
    notice(S, "received ~p as an offer", [Item]),
    {next_state, negotiate, S#state{otheritems=add(Item,O)}};
negotiate({retract_offer, Item}, S=#state{ownitems=I}) ->
    undo_offer(S#state.otherFSM, Item),
    notice(S,"takes back ~p", [Item]),
    {next_state, negotiate, S#state{ownitems=remove(Item,I)}};
negotiate({undo_offer, Item}, S=#state{otheritems=O}) ->
    notice(S, "has given back ~p", [Item]),
    {next_state, negotiate, S#state{otheritems=remove(Item,O)}};
%% Finishing the negotiation phase (syncing states)
%
%   YOU      <--->   yourFSM    <-------------------->    otherFSM     <--->    OTHER
%  shell              <toby>                               <creed>              shell
%  (pidB)            (OwnPid)                            (OtherPid)             (pidA)
%                       |                                    |
%                  [-NEGOTIATE-]                        [-NEGOTIATE-]
% ready(<toby>) ----> are_you_ready(<creed>) --------------> |
%  (waiting)            |                                    |
%                   [-WAIT-] <----------not_yet------------- |
%  "offer changed" <--- |    <--------offer, retract ------- | <--------------- make_offer/retract_offer
%                  [-NEGOTIATE-]
%
% At this point we got back to negotiation (we probably want to check what <creed> is up to
% and if we are ok with the deal just hit are_you_ready/1 again. At one point both sides will
% agree:
%
% ready(<toby>) ----> are_you_ready(<creed>) -------------> |
%                       |                                   |
%      w             [-WAIT-] <---------not_yet-----------  |
%      a                |                                   |
%      i                | <---------------------------- are_you_ready(<toby>) <--- ready(<creed>)
%      t                |                                 [-WAIT-]
%      s                | ---am_ready------------------->   | --------                 w
%      .        ------- | <-------------------am_ready---   |         \                a
%      .       /        | <-------------------ack_ready--   |          \               i
%      .      /         |                                 [-READY-]     \              t
%            /          | ---am_ready------------------->   |            \             s
%     ok ---/           | ---ack_ready------------------>   |             \            .
%                    [-READY-]                              |              \           .
%                       |                                   |               \          .
%                       | ---ask_commit-------------------> |                \
%                       | <-----------------ready_commit--- |                 \------ ok
%                       | ---do_commit--------------------> |
%      (... saves ...)  |                                   |
%                       | <----------ok-------------------- |
%                       |                                   | (... saves...)
%                     STOP                                 STOP

%% ... continuing ASYNC negotiate/2 using the above flow
negotiate(are_you_ready, S=#state{}) ->
    notice(S, "is ready to seal the deal",[]),
    notice(S, "gets ~p, while other user gets ~p",
           [S#state.otheritems, S#state.ownitems]),
    not_yet(S#state.otherFSM),
    {next_state, negotiate, S};
negotiate(Event, StateData) ->
    unexpected(Event, negotiate),
    {next_state, negotiate, StateData}.

%% SYNC negotiate/3 from players
negotiate(ready, From, S=#state{}) ->
    are_you_ready(S#state.otherFSM),
    notice(S, "asking if ready, waiting...", []),
    {next_state, wait, S#state{from=From}};
negotiate(Event, _From, StateData) ->
    unexpected(Event, negotiate),
    {next_state, negotiate, StateData}.

%% ASYNC wait/2
wait({do_offer, Item}, S=#state{otheritems=O}) ->
    % A reply is needed because poor sap is waiting since they
    % declared ready/1 for a response whether the deal can be
    % closed or needs adjustments.
    gen_fsm:reply(S#state.from, offer_changed),
    notice(S, "received ~p as an offer", [Item]),
    {next_state, negotiate, S#state{otheritems=add(Item,O)}};
wait({undo_offer, Item}, S=#state{otheritems=O}) ->
    % Same here for gen_fsm:reply/2
    gen_fsm:reply(S#state.from, offer_changed),
    notice(S, "has given back ~p", [Item]),
    {next_state, negotiate, S#state{otheritems=remove(Item,O)}};
%% Focusing on closing the trade.
wait(are_you_ready, S=#state{otherFSM=OtherPid}) ->
    am_ready(OtherPid),
    notice(S, "asked if ready and I am. Waiting for same reply.", []),
    {next_state, wait, S};
wait(not_yet, S) ->
    notice(S, "Other not ready yet.", []),
    {next_state, wait, S};
% ready(<toby>) ----> are_you_ready(<creed>) -------------> |
%                       |                                   |
%      w             [-WAIT-] <---------not_yet-----------  |
%      a                |                                   |
%      i                | <---------------------------- are_you_ready(<toby>) <--- ready(<creed>)
%      t                |                                 [-WAIT-]
%      s                | ---am_ready------------------->   | --------                 w
%      .        ------- | <-------------------am_ready---   |         \                a
%      .       /        | <-------------------ack_ready--   |          \               i
%      .      /         |                                 [-READY-]     \              t
%            /          | ---am_ready------------------->   |            \             s
%     ok ---/           | ---ack_ready------------------>   |             \            .
%                    [-READY-]                              |              \           .
%                       |                                   |               \          .
%                       | ---ask_commit-------------------> |                \
%                       | <-----------------ready_commit--- |                 \------ ok
%                       | ---do_commit--------------------> |
%      (... saves ...)  |                                   |
%                       | <----------ok-------------------- |
%                       |                                   | (... saves...)
%                     STOP                                 STOP

%% When both FSMs are in the WAIT state and the other party is ready,
%% they exchange an extra "am_ready" message that seems superfluous
%% but it is important to safely get into READY state and negate the
%% effects of race conditions.
%% Such as:

%   YOU      <--->   yourFSM    <-------------------->    otherFSM     <--->    OTHER
%  shell              <toby>                               <creed>              shell
%  (pidB)            (OwnPid)                            (OtherPid)             (pidA)
%                       |                                    |
%                  [-NEGOTIATE-]                        [-NEGOTIATE-]
% ready(<toby>) ----> are_you_ready(<creed>) ------  ------- offer
%  (waiting)            |                          \//------ are_you_ready(<toby>)
%                       |                          /\     [-WAIT-]
%                   [-WAIT-] <---------------------/ ----->  |
%                  [-NEGOTIATE-] <----------------/       [-READY-]
%                       | ---------------not_yet-----------> |
%                       |                                    |
%                      ???                 ???              ???

wait(am_ready, S=#state{otherFSM=OtherPid}) ->
    am_ready(OtherPid),
    % Both FSMs use ack_ready/1 to enter READY state in a
    % synchronized way and once in there, both players' 
    % actions become useless. (The FSMs are free to talk
    % without distractions to close the trade. The players
    % expressed their wish to finish the trade willingly
    % anyway.
    ack_ready(OtherPid),
    % I keep forgetting to reply to previous synchronous
    % requests originating from other states...
    gen_fsm:reply(S#state.from, ok),
    notice(S, "Other side is ready. Moving to ready state.", []),
    {next_state, ready, S};
wait(Event, StateData) ->
    unexpected(Event, wait),
    {next_state, wait, StateData}.

%% Once in ready state, we can start our bastardized two-phase
%% commit to finish the trade.
%% https://en.wikipedia.org/wiki/Two-phase_commit_protocol

%   YOU      <--->   yourFSM    <-------------------->    otherFSM     <--->    OTHER
%  shell              <toby>                               <creed>              shell
%  (pidB)            (OwnPid)                            (OtherPid)             (pidA)
%                       |                                   |
% ready(<toby>) ----> are_you_ready(<creed>) -------------> |
%                       |                                   |
%      w             [-WAIT-] <---------not_yet-----------  |
%      a                |                                   |
%      i                | <---------------------------- are_you_ready(<toby>) <--- ready(<creed>)
%      t                |                                 [-WAIT-]
%      s                | ---am_ready------------------->   | --------                 w
%      .        ------- | <-------------------am_ready---   |         \                a
%      .       /        | <-------------------ack_ready--   |          \               i
%      .      /         |                                 [-READY-]     \              t
%            /          | ---am_ready------------------->   |            \             s
%     ok ---/           | ---ack_ready------------------>   |             \            .
%                    [-READY-]                              |              \           .
%                       |                                   |               \          .
%                       | ---ask_commit-------------------> |                \
%                       | <-----------------ready_commit--- |                 \------ ok
%                       | ---do_commit--------------------> |
%      (... saves ...)  |                                   |
%                       | <----------ok-------------------- |
%                       |                                   | (... saves...)
%                     STOP                                 STOP
%
% Two-phase commits require SYNCHRONOUS communication and in order to
% to avoid deadlocks (a reason why we used ASYNCHRONOUS methods in
% interFSM communicatis) one FSM has to take charge and orchestrate the
% commit procedure.
%
% SOLUTION: pids can be compared to each other and we can select the
%           highest one to control the process. See priority/2 among
%           the helper functions.
%
% REMINDER: All terms are comparable to each other in Erlang due to
%           the strict hierarchy.
%           "Nuked Atoll Refers to a Fun sPort to Piddle on Tulips More at Least a Bit."

%% ASYNC ready
%% Basically the controlling FSM sends the messages, the other
%% just stands by and replies when asked.
ready(ack_ready, S=#state{}) ->
    case priority(self(), S#state.otherFSM) of
        true ->
            try
                notice(S, "asking for commit", []),
                ready_commit = ask_commit(S#state.otherFSM),
                notice(S, "ordering commit", []),
                ok = do_commit(S#state.otherFSM),
                notice(S, "committing",[]),
                commit(S),
                {stop, normal, S}
            catch
                % either ask_commit or do_commit failed
                Class:Reason ->
                    notice(S, "commit failed", []),
                    {stop, {Class, Reason}, S}
            end;
        false ->
            {next_state, ready, S}
    end;
ready(Event, StateData) ->
    unexpected(Event, ready),
    {next_state, ready, StateData}.

%% SYNC ready
ready(ask_commit, _From, S) ->
    notice(S, "replying to ask commit", []),
    {reply, ready_commit, ready, S};
ready(do_commit, _From, S) ->
    notice(S, "committing...", []),
    commit(S),
   %{stop, Reason, Reply, NewStateData}
    {stop, normal, ok,    S};
ready(Event, _From, StateData) ->
    unexpected(Event, ready),
    {next_state, ready, StateData}.

%% Any player can cancel the transaction at any time,
%% using cancel/1 that calls gen_fsm:sync_send_all_state_event/2
%% This tells yourFSM to drop it but we need to notify
%% the other player as well (using notify_cancel/2).

%% In order for these functions to work we need to implement
%% the handle_*event callbacks.
%%
%% cancel/1      -> gen_fsm:sync_send_all_state_event/2 -> handle_sync_event/4
%% notify_cancel -> gen_fsm:send_all_state_event/2      -> handle_event/3

handle_event(cancel, _State, S) ->
    notice(S, "other cancelled", []),
    {stop, other_cancelled, S};
handle_event(Event, StateName, StateData) ->
    unexpected(Event, StateName),
    {next_state, StateName, StateData}.

handle_sync_event(cancel, _From, _State, S) ->
    notify_cancel(S#state.otherFSM),
    notice(S, "cancelling trade, sending cancel event", []),
    {stop, cancelling, ok, S};
%% DO NOT reply to unexpected calls, let the call-maker crash!
handle_sync_event(Event, _From, StateName, StateData) ->
    unexpected(Event, StateName),
    {next_state, StateName, StateData}.

%% We need to take care of the situation when the other player's
%% FSM crashes. Fortunatily we set up a monitor in IDLE state
%% that we can match.
%% We can use the handle_info/3 callback because it handles all
%% messages received from
%%   * a NOT async/sync event
%%   * the runtime system
handle_info({'DOWN', Ref, process, Pid, Reason},
            StateName,
            S=#state{otherFSM=Pid, monitor=Ref}) ->
    notice(S,"other side dead, got this is state:[~p]", [StateName]),
    {stop, {other_down, Reason}, S};
handle_info(Info, StateName, StateData) ->
    unexpected(Info, StateName),
    {next_state, StateName, StateData}.

code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.
 
%% Transaction completed.
terminate(normal, ready, S=#state{}) ->
    notice(S, "FSM leaving.", []);
terminate(_Reason, _StateName, _StateData) ->
    ok.
%%% ================================================
%%% HELPER FUNCTIONS
%%% ================================================

%% send notice to players (or in our case, to the shell)
%% NOTE
%    we used io:format/2 for most of our messages to let the FSMs
%    communicate with their own clients. In a real world application, we
%    might want something more flexible than that. One way to do it is to
%    let the client send in a Pid, which will receive the notices sent to
%    it. That process could be linked to a GUI or any other system to make the
%    player aware of the events. The io:format/2 solution was chosen for its
%    simplicity: we want to focus on the FSM and the asynchronous protocols,
%    not the rest.
notice(#state{name=N}, Str, Args) ->
    io:format("~s: " ++ Str ++ "~n", [N|Args]).
    % i think extra brackets are missing ([[N|Args]])

%% to log unexpected messages
unexpected(Msg, StateName) ->
    io:format("~p received unknown event ~p while in state ~p~n",
              [self(), Msg, StateName]).

%% adds item to an itemlist
add(Item, Items) -> [Item|Items].

%% removes items from an itemlist
remove(Item,Items) -> Items -- [Item].

%% compare FSM pids to select control process
%% Returns boolean to make READY state easier. It could return
%% a pid but then we would have to compare pids again in
%% relevant READY clause to see which is which.
priority(OwnPid, OtherPid) when OwnPid > OtherPid -> true;
priority(OwnPid, OtherPid) when OwnPid < OtherPid -> false.

%% commit/1
% It's generally not possible to do a true safe commit with only two
% participants—a third party is usually required to judge if both players
% did everything right. If you were to write a true commit function, it
% should contact that third party on behalf of both players, and then do
% the safe write to a database for them or rollback the whole exchange. We
% won't go into such details and the current commit/1 function will be
% enough for the needs of this book.
commit(#state{name=N, ownitems=I, otheritems=O}) ->
    io:format("Transaction completed for ~s.~n"
              "Items sent: ~p~n, Items received: ~p~n"
              "~nThis operation should have atomic save in a database~n",
              [N, I, O]).

%% === STATE FLOW =======
%% ======================

 % === The Skeleton

 %   ____________________
 %  /                    V        <----\
 % IDLE -> IDLE_WAIT -> NEGOTIATE       WAIT -> READY -> STOP
 %                       ^    /  \---->
 %                       \___/

