%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service logpropagation.Service.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on 2019-05-07T10:57:48+00:00 and should not be modified manually

-module(logpropagation_service_bhvr).

%% @doc 
-callback watch_async(log_propagation_pb:sub_request(), grpcbox_stream:t()) ->
    ok | grpcbox_stream:grpc_error_response().

