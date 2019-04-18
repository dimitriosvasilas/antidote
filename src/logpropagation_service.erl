-module(logpropagation_service).

-behaviour(logpropagation_service_bhvr).

-export([watch_async/2]).

-spec watch_async(Message::log_propagation_pb:sub_request(), GrpcStream :: grpcbox_stream:t()) -> ok.
watch_async(_Message, GrpcStream) ->
    gen_server:cast(log_propagator, {watchAsync, GrpcStream}),
    receive
	    _ -> io:format("receive.. d ~n")
    end,
    ok.
