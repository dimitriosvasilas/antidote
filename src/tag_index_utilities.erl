-module(tag_index_utilities).

-include("antidote.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(LOG_UTIL, mock_partition_fsm).
-define(LOGGING_VNODE, mock_partition_fsm).
-else.
-define(LOG_UTIL, log_utilities).
-define(LOGGING_VNODE, logging_vnode).
-endif.

-export([
  format_update_tag_params/1,
  perform_tag_update/3
  ]).

format_update_tag_params(Updates) ->
  [{{Key, _, Bucket}, _, TagList} | _ ] = Updates,
  TagObjKey = list_to_atom(atom_to_list(Key) ++ atom_to_list('/tags')),
  lists:map(fun({TagK, TagV}) ->
                {{TagObjKey, antidote_crdt_gmap, Bucket},
                update,
                {{TagK,antidote_crdt_lwwreg},{assign, TagV}}} end,
                TagList).

perform_tag_update(Args, SD0, Transaction) ->
  lager:info("tag_index_utilities:perform_tag_update__Args_~p~n", [Args]),
  {{Key, _, Bucket}, OpType, Params} = hd(Args),
  Preflist = ?LOG_UTIL:get_preflist_from_key({Key, Bucket}),
  TxId = Transaction#transaction.txn_id,
  LogRecord = #log_operation{tx_id = TxId, op_type = tag_update,
  log_payload = #update_log_payload{key = Key, type = antidote_crdt_gmap, op = {OpType, Params}}},
  LogId = ?LOG_UTIL:get_logid_from_key({Key, Bucket}),
  [Node] = Preflist,
  ok = ?LOGGING_VNODE:asyn_append(Node, LogId, LogRecord, {fsm, undefined, self()}),
  NewClientOps = [{{Key, Bucket}, antidote_crdt_gmap, {OpType, Params}} | SD0#tx_coord_state.client_ops],
  WriteSet = case lists:keyfind(Node, 1, SD0#tx_coord_state.updated_partitions) of
              false ->
                [];
              {Node, WS} ->
                WS
              end,
  NewUpdatedPartitions =
    case WriteSet of
      [] ->
        [{Node, [{{Key, Bucket}, antidote_crdt_gmap, add}]} | SD0#tx_coord_state.updated_partitions];
      _ ->
        lists:keyreplace(Node, 1, SD0#tx_coord_state.updated_partitions,
                          {Node, [{{Key, Bucket}, antidote_crdt_gmap, add} | WriteSet]})
    end,
  {NewClientOps, NewUpdatedPartitions}.
