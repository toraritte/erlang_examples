Addition comments to the Learn You Some Erlang trading system implementation so that I can wrap my head around it a couple days later as well.

The chapter url:
http://learnyousomeerlang.com/finite-state-machines

```erlang
%   ____________________
%  /                    V        <----\
% IDLE -> IDLE_WAIT -> NEGOTIATE       WAIT -> READY -> STOP
%                       ^    /  \---->
%                       \___/

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
```
