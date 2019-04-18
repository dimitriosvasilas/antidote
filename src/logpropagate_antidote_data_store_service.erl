-module(logpropagate_antidote_data_store_service).

-behaviour(logpropagate_antidote_data_store_bhvr).

-export([watch_async/2]).

-spec watch_async(Message::log_propagate_pb:sub_request(), GrpcStream :: grpcbox_stream:t()) -> ok.
watch_async(_Message, GrpcStream) ->
    grpcbox_stream:send(#{key => <<"Test">>}, GrpcStream),
    ok.

