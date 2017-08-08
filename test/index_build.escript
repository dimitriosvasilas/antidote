#!/usr/bin/env escript

-define(ADDRESS, "localhost").

-define(PORT, 8087).

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
    SetKey1 = <<"k1_v1">>,
    SetKey2 = <<"k2_v2">>,
    SetObj1 = {SetKey1, antidote_crdt_orset, <<"index_test_bucket">>},
    SetObj2 = {SetKey2, antidote_crdt_orset, <<"index_test_bucket">>},

    io:format("Starting Test transaction~n"),
    {ok, Tx} = antidotec_pb:start_transaction(Pid, ignore, {}),

    io:format("Updating set~n"),
    ok = antidotec_pb:update_objects(Pid, [
    {SetObj1, add, <<"obj1">>},
    {SetObj2, add, <<"obj2">>},
    {SetObj1, add, <<"obj3">>},
    {SetObj2, add, <<"obj4">>}
    ], Tx),

    io:format("Reading set~n"),
    {ok, [Val]} = antidotec_pb:read_objects(Pid, [SetObj1], Tx),
    Value = antidotec_set:value(Val),
    io:format("Set value~p~n", [Value]),

    io:format("Commiting transaction~n"),
    {ok, _} = antidotec_pb:commit_transaction(Pid, Tx),
    true = Value >= 0,
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
