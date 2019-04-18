%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service logpropagate.AntidoteDataStore.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on 2019-04-17T12:58:33+00:00 and should not be modified manually

-module(logpropagate_antidote_data_store_bhvr).

%% @doc 
-callback watch_async(log_propagate_pb:sub_request(), grpcbox_stream:t()) ->
    ok | grpcbox_stream:grpc_error_response().

