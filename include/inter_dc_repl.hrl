-include("antidote_message_types.hrl").

-record(recvr_state,
        {lastRecvd :: orddict:orddict(), %TODO: this may not be required
         lastCommitted :: orddict:orddict(),
         %%Track timestamps from other DC which have been committed by this DC
         recQ :: orddict:orddict(), %% Holds recieving updates from each DC separately in causal order.
         statestore,
         partition}).

-type socket_address() :: {inet:ip_address(), inet:port_number()}.
-type zmq_socket() :: any().
-type pdcid() :: {dcid(), partition_id()}.
-type log_opid() :: pos_integer().

%% Ping strcuture for partial replication
-record(partial_ping, {
	  partition_dcid_op_list :: [{partition_id(), [{dcid(), #dc_last_ops{}}]}],
	  time :: clock_time()
	 }).

-record(interdc_txn, {
	  dcid :: dcid(),
	  partition :: partition_id(),
	  prev_log_opid_dc :: [{dcid(),#op_number{}}] | #partial_ping{} | none, %% for partial rep, the opid by per DC that replicates is
	  prev_log_opid :: #op_number{} | none, %% the value is *none* if the transaction is read directly from the log
	  snapshot :: snapshot_time() | none,
	  timestamp :: clock_time(),
	  last_update_opid :: #op_number{}, %% last opid of the txn that was an update operations (i.e. not a commit/abort) THIS ISN'T USED???
	  bucket :: bucket(),
	  log_records :: [#log_record{}] %% if the OP list is empty, the message is a HEARTBEAT
}).

-record(descriptor, {
 bucket_sub_list :: [bucket()],
 dcid :: dcid(),
 partition_list :: [partition_id()],
 partition_num :: non_neg_integer(),
 publishers :: [socket_address()],
 logreaders :: [socket_address()]
}).

%% This keeps information about an inter-dc request that
%% is waiting for a reply
-record(request_cache_entry, {
	  request_type :: inter_dc_message_type(),
	  func :: function() | undefined,
	  req_id_binary :: binary(),
	  req_pid :: pid() | {fsm, pid()},
	  pdcid :: pdcid(),
	  timer :: reference(),
	  extra_state :: term(),
	  binary_req :: binary()
	 }).

%% This keeps information about an inter-dc request
%% on the site that is performing the query
-record(inter_dc_query_state, {
	  request_type :: inter_dc_message_type(),
	  zmq_id :: term(),
	  request_id_num_binary :: binary(),
	  local_pid :: pid()
	 }).

-record(external_read_request_state, {
	  dc_list :: [pdcid()],
	  key :: key(),
	  type :: type(),
	  transaction :: tx(),
	  coordinator :: pid() | {fsm, pid()}
	 }).

%% State for sub buff
-record(inter_dc_sub_buf, {
  local_partition :: partition_id(),
  external_partition_count :: non_neg_integer(),
  state_name :: normal | buffering,
  pdcid :: pdcid(),
  last_observed_opid :: non_neg_integer() | init,
  last_observed_commit_ids :: {non_neg_integer(), non_neg_integer()} | init,
  queue :: queue()
}).
