-module(lyse_naive_tcp).

-export([start_server/0]).

start_server() ->
    Parent = self(),
    Pid = spawn_link(fun() ->
        % start a tcp server by opening a listening port
        {ok, ListenSocket} = gen_tcp:listen(0, [{active, false}, binary]),
        spawn(fun() -> acceptor(ListenSocket) end),
        % sending the port to the parent process, so that we
        % what port number we can use
        Parent ! ListenSocket,
        % We need to keep this process alive, because the listen socket
        % is bound to the process that opened it
        timer:sleep(infinity)
    end),
    {ok, PortNumber} = receive
                           Port when is_port(Port)->
                               inet:port(Port)
                       end,
    {ok, Pid, PortNumber}.

acceptor(ListenSocket) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    % At this point the process is waiting for connections
    % (same as when the process hangs in 'receive' while
    %  waiting for messages)
    % Once a connection request comes in, we start another
    % process to wait for connections and we handle the
    % the current client.
    spawn(fun() -> acceptor(ListenSocket) end),
    handle(AcceptSocket).

handle(AcceptSocket) ->
    % receive 1 message and then go into passive mode
    inet:setopts(AcceptSocket, [{active, once}]),
    receive
        % the pattern means that we first want to match on
        % a binary string containing the characters q, u, i,
        % and t, plus some binary data we don't care about (_).
        {tcp, AcceptSocket, <<"quit", _/binary>>} ->
            gen_tcp:close(AcceptSocket);
        % parroting back whatever we get
        % REMEMBER: we can use the accept socket to send back
        % messages too
        {tcp, AcceptSocket, Msg} ->
            gen_tcp:send(AcceptSocket, Msg),
            handle(AcceptSocket)
    end.

