-module(log_propagator).

-behaviour(gen_server).

-include("antidote.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(DC_META_UTIL, mock_partition).
-define(LOG_UTIL, mock_partition).

-else.
-define(DC_META_UTIL, dc_meta_data_utilities).
-define(LOG_UTIL, log_utilities).
-endif.

%% API
-export([start_link/0, perform_update/4, committed/2]).

%% Server methods
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% State
-record(state, { streams :: [grpcbox_stream:t()],
	         txs :: dict:dict()}).


%%%% API --------------------------------------------------------------------+

perform_update({Key, Bucket}, Type, Transaction, Update) ->
    gen_server:cast(?MODULE, {perform_update, Key, Bucket, Type, Transaction, Update}).

committed(Transaction, CommitTime) ->
    gen_server:cast(?MODULE, {committed, Transaction, CommitTime}).

%%%% Server methods ---------------------------------------------------------+

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{streams = [], txs = dict:new()}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({watchAsync, Stream}, State = #state{streams = Streams}) ->
    {noreply, State#state{streams = [Stream | Streams]}};

handle_cast({perform_update, Key, Bucket, Type, Transaction, Update}, State = #state{txs = Txs}) ->
    Partition = log_utilities:get_key_partition({Key, Bucket}),
    SnapshotOld = 
        case clocksi_vnode:read_data_item(Partition, Transaction, {Key, Bucket}, Type, []) of
            {ok, S}->
	        S;
	    {error, Reason}->
		io:format("error in read_data_item ~p~n", [Reason]),
	        error
	end,
    {ok, Op}  = Type:downstream(Update, SnapshotOld),
    SnapshotNew = clocksi_materializer:materialize_eager(Type, SnapshotOld, [Op]),
    Txs1 = dict:append(Transaction#transaction.txn_id, {Key, Bucket, Type, Update, SnapshotOld, SnapshotNew}, Txs),
    {noreply, State#state{txs = Txs1}};

handle_cast({committed, Transaction, CommitTime}, State = #state{streams = Streams, txs = Txs}) ->
    case dict:take(Transaction#transaction.txn_id, Txs) of
        {Updates, Txs1} ->
            {DcID, _} = ?DC_META_UTIL:get_my_dc_id(),
            ProtoMsgs = [update_to_proto_msg(U, CommitTime, DcID) || U <- Updates],
            [grpcbox_stream:send(Msg, S) || Msg <- ProtoMsgs, S <- Streams],
            {noreply, State#state{txs = Txs1}};
	error ->
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%% Internal Functions -----------------------------------------------------+

update_to_proto_msg({Key, Bucket, Type, {update, Update}, SnapshotOld, SnapshotNew}, CommitTime, DcID) ->
    {PartitionID, _} = ?LOG_UTIL:get_key_partition({Key, Bucket}),
    #{dc_ID => atom_to_list(DcID),
      partition_ID => integer_to_list(PartitionID),
      key => Key,
      bucket => Bucket,
      crdt_type => atom_to_list(Type),
      commit_time => CommitTime,
      payload => payload(Update, SnapshotOld, SnapshotNew, delta)}.


payload(Update, _SnapshotOld, _SnapshotNew, op) ->
    #{val => {op, operation(Update)}};

payload(_Update, SnapshotOld, SnapshotNew, delta) ->
    #{val => {delta, state_delta(dict:to_list(SnapshotOld), dict:to_list(SnapshotNew))}}.

operation(Update) when is_list(Update) ->
    #{op => lists:map(fun({{K, T}, {Op, V}}) -> crdt_map_entry(K, T, Op, V) end, Update)};

operation({{K, T}, {Op, V}}) ->
    #{op => [crdt_map_entry(K, T, Op, V)]}.

state_delta(SO, SN) ->
    #{old => crdt_map_state(SO), new => crdt_map_state(SN)}.

crdt_map_state(S) ->
    #{state => lists:map(fun({{K, T}, V}) -> state(K, T, V) end, S)}.

state(Key, Type, Val) ->
    #{object => object(Key, Type), value => value(Type:value(Val))}.

crdt_map_entry(Key, Type, OpType, Val) ->
    #{object => object(Key, Type), update => update(OpType, Val)}.

object(Key, Type) ->
    #{key => Key, type => atom_to_list(Type)}.

update(OpType, Val) ->
    #{opType => atom_to_list(OpType), value => value(Val)}.

value(Val) when is_integer(Val) ->
    #{val => {int, Val}};

value(Val) when is_bitstring(Val) ->
    #{val => {str, Val}};

value(_Val) ->
    #{val => {str, "not_supported"}}.
