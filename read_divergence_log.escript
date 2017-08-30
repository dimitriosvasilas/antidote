#!/usr/bin/env escript

main(_) ->
  LogPath = "/Users/dimitriosvasilas/antidote/_build/default/rel/antidote1/data/1/divergence",
  case disk_log:open([{name, LogPath}]) of
    {error, Reason} ->
      io:format("~p", [Reason]);
    {ok, Log} ->
      read(Log, start),
      disk_log:close(Log);
    {repaired, Log, _, _} ->
      read(Log, start),
      disk_log:close(Log)
    end.

read(Log, Continuation) ->
    io:format("read__~p~n", [Continuation]),
    case disk_log:chunk(Log, Continuation) of
        eof ->
            ok;
        {error, Reason} ->
            io:format("~p~n", [Reason]);
        {NewContinuation, NewTerms} ->
            io:format("~p~n", [NewTerms]),
            read(Log, NewContinuation)
    end.
