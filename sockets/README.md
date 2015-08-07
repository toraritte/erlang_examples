LYSE [naive tcp server][1]
=====================
```erlang
% Shell  --->  Process 1  ----------->  Process 2
% =====        =========                =========
%            listen socket <.....> waiting for requests
%                ^    ^                    ^
%                :    :                    | ----------------> Process 3
%                :    :                    |                  =========
%                :    :................................> waiting for requests
%                :                         |                      ^
%                :                  handle messages               |
%                :                         |                      |
%                :                     (telnet)                   | ------------> Process 4
%                :                                                |               =========
%                :.........................................................> waiting for requests
%                                                                 |
%                                                             erl shell
1> c(lyse_naive_tcp).
{ok,lyse_naive_tcp}
2>
2> lyse_naive_tcp:start_server().
{ok,<0.47.0>,39150}
3> 
3> {ok, Socket} = gen_tcp:connect({127,0,0,1}, 39150, [{active, true}, binary]).
{ok,#Port<0.2443>}
4> 
4> gen_tcp:send(Socket, "balabab").
ok
5> flush().
Shell got {tcp,#Port<0.2443>,<<"balabab">>}
ok
```
```bash
telnet localhost 39150
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
hello 
hello
lofa
lofa
quit it
Connection closed by foreign host.
```
> The code is simple, but wasn't thought with parallelism in mind. If all
> the requests come one by one, then the naive server works fine. What
> happens if we have a queue of 15 people wanting to connect to the server
> at once, though?
> 
> Then only one query at a time can be replied to, and this has to do
> with each process first waiting for the connection, setting it up,
> then spawning a new acceptor. The 15th request in the queue will have
> had to wait for 14 other connections to have been set up to even get
> the chance of asking for a right to discuss with our server. If you're
> working with production servers, it might be closer to, I don't know,
> five hundred to a thousand queries per second. That's impractical.

![server sequential workflow][2]

[1]: http://learnyousomeerlang.com/buckets-of-sockets
[2]: http://learnyousomeerlang.com/static/img/sequential-accept.png
