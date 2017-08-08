#!/usr/bin/env escript

-define(ADDRESS, "localhost").

-define(PORT, 8187).

load(Dep) ->
    Path = filename:dirname(escript:script_name()) ++ "/../_build/test/lib/" ++ Dep ++ "/ebin",
    case code:add_pathz(Path) of
        true ->
            true;
        Err ->
            erlang:error({could_not_load, Path, Err})
    end.

main(_) ->
    % load required code
    [load(Dep) || Dep <- ["riak_pb", "antidote_pb", "protobuffs"]],

    % Try to read something:
    ok = test_transaction(20).

test_transaction(Tries) ->
    {ok, Pid} = try_connect(10),
    CounterKey = <<"counter_test_key">>,
    SetKey = <<"set_test_key">>,
    CounterObj = {CounterKey, antidote_crdt_counter, <<"release_test_key_bucket">>},
    SetObj = {SetKey, antidote_crdt_orset, <<"release_test_key_bucket">>},
    Key = <<"pb_client_SUITE_crdt_map_aw_test">>,
    Bound_object = {Key, antidote_crdt_gmap, <<"bucket">>},


    io:format("Starting Test transaction~n"),
    {ok, Tx} = antidotec_pb:start_transaction(Pid, ignore, {}),

    %io:format("Updating counter~n"),
    %ok = antidotec_pb:update_objects(Pid, [{CounterObj, increment, 1}], Tx),
    %io:format("Updating set~n"),
    %ok = antidotec_pb:update_objects(Pid, [{SetObj, add, <<"a">>}], Tx),
    io:format("Updating map~n"),
    %ok = antidotec_pb:update_objects(Pid, [{SetObj, update, {{<<"a">>, antidote_crdt_integer}, {set, 42}}}], Tx),
    ok = antidotec_pb:update_objects(Pid, [
      {Bound_object, update, {{<<"a">>, antidote_crdt_lwwreg}, {assign, <<"42">>}}}], Tx),

    %io:format("Reading counter~n"),
    %{ok, [CVal]} = antidotec_pb:read_objects(Pid, [CounterObj], Tx),
    %Value = antidotec_counter:value(CVal),
    %io:format("Counter value ~p~n", [Value]),
    %io:format("Reading set~n"),
    %{ok, [SVal]} = antidotec_pb:read_objects(Pid, [SetObj], Tx),
    %SValue = antidotec_set:value(SVal),
    %io:format("Set value~p~n", [SValue]),
    %io:format("Reading map~n"),
    %{ok, [Value]} = antidotec_pb:read_values(Pid, [Bound_object], Tx),
    %io:format("Map value~p~n", [Value]),

    io:format("Commiting transaction~n"),
    {ok, _} = antidotec_pb:commit_transaction(Pid, Tx),
    %true = Value >= 0,
    _Disconnected = antidotec_pb_socket:stop(Pid),
    io:format("Release is working!~n"),
    ok.

try_connect(Tries) ->
     case antidotec_pb_socket:start(?ADDRESS, ?PORT) of
        {ok, Pid} ->
            {ok, Pid};
        Other when Tries > 0 ->
            io:format("Could not connect to Antidote: ~p~n", [Other]),
            timer:sleep(1000),
            io:format("Retrying to connect ...~n"),
            try_connect(Tries - 1);
        Other ->
            Other
     end.
