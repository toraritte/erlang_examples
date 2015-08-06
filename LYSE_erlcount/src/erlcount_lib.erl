-module(erlcount_lib).
-export([find_erl/1, regex_count/2]).

%%% see file:read_file_info/{1,2} or
%%%     file:write_file_info/{1,2}
%%% in the file module
-include_lib("kernel/include/file.hrl").

find_erl(Directory) ->
    find_erl(Directory, queue:new()).

%%% PRIVATE
%% dispatch based on file type
find_erl(Name, Queue) ->
    {ok, F = #file_info{}} = file:read_file_info(Name),
    case F#file_info.type of
        directory ->
            handle_directory(Name, Queue);
        regular ->
            handle_regular_file(Name, Queue);
        _Other ->
            dequeue_and_run(Queue)
    end.

%% handles directories and enqueues files
handle_directory(Dir, Queue) ->
    case file:list_dir(Dir) of
        {ok, []} ->
            dequeue_and_run(Queue);
        {ok, Files} ->
            dequeue_and_run(enqueue_files_in_dir(Dir, Files, Queue))
    end.

%% Pushing all the files in a given directory to the
%% queue.
%% NOTE: of course, any of these files can be a directory
%%       themselves, but this is a recursive function and
%%       when the particular dir entries are dequeued then
%%       the whole cycle will start again
enqueue_files_in_dir(Path, Files, Queue) ->
    F = fun(File, Q) -> queue:in(filename:join(Path,File), Q) end,
    lists:foldl(F,Queue,Files).

%% pops an item from the queue and runs it
dequeue_and_run(Queue) ->
    case queue:out(Queue) of
        {{value, Item}, Q} ->
            find_erl(Item, Q);
        {empty, _} ->
            done
    end.

%% checks whether the file extension is .erl
handle_regular_file(File, Queue) ->
    case filename:extension(File) of
        ".erl" ->
            {continue, File, fun() -> dequeue_and_run(Queue) end};
        _Other ->
            dequeue_and_run(Queue)
    end.

regex_count(Re, Str) ->
    case re:run(Str, Re, [global]) of
        nomatch -> 0;
        {match, List} -> length(List)
    end.

