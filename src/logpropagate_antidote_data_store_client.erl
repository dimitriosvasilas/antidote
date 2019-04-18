%%%-------------------------------------------------------------------
%% @doc Client module for grpc service logpropagate.AntidoteDataStore.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on 2019-04-17T12:58:33+00:00 and should not be modified manually

-module(logpropagate_antidote_data_store_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpcbox.hrl").

-define(is_ctx(Ctx), is_tuple(Ctx) andalso element(1, Ctx) =:= ctx).

-define(SERVICE, 'logpropagate.AntidoteDataStore').
-define(PROTO_MODULE, 'log_propagate_pb').
-define(MARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Input, Output, MessageType), #grpcbox_def{service=?SERVICE,
                                                      message_type=MessageType,
                                                      marshal_fun=?MARSHAL_FUN(Input),
                                                      unmarshal_fun=?UNMARSHAL_FUN(Output)}).

%% @doc 
-spec watch_async(log_propagate_pb:sub_request()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response().
watch_async(Input) ->
    watch_async(ctx:new(), Input, #{}).

-spec watch_async(ctx:t() | log_propagate_pb:sub_request(), log_propagate_pb:sub_request() | grpcbox_client:options()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response().
watch_async(Ctx, Input) when ?is_ctx(Ctx) ->
    watch_async(Ctx, Input, #{});
watch_async(Input, Options) ->
    watch_async(ctx:new(), Input, Options).

-spec watch_async(ctx:t(), log_propagate_pb:sub_request(), grpcbox_client:options()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response().
watch_async(Ctx, Input, Options) ->
    grpcbox_client:stream(Ctx, <<"/logpropagate.AntidoteDataStore/WatchAsync">>, Input, ?DEF(sub_request, operation, <<"logpropagate.SubRequest">>), Options).

