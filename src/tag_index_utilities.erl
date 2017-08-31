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
  perform_tag_update/3,
  read_tag_index/2,
  update_tags/2
  ]).

format_update_tag_params(Updates) ->
  lists:map(fun({{Key, _, Bucket}, _, _, {_, {{TagK, _},{_, TagV}}}}) ->
                TagBucket = binary:list_to_bin(atom_to_list('_') ++ binary:bin_to_list(Bucket)),
                {{Key, antidote_crdt_gmap, TagBucket},
                update,
                {{TagK,antidote_crdt_lwwreg},{assign, TagV}}} end,
                Updates).

perform_tag_update(Args, SD0, Transaction) ->
  {Key, Bucket, TagParams} = Args,
  TagBucket = binary:list_to_bin(atom_to_list('_') ++ binary:bin_to_list(Bucket)),
  TagKey = binary:list_to_bin(atom_to_list('_') ++ binary:bin_to_list(Key)),
  Preflist = ?LOG_UTIL:get_preflist_from_key({TagKey, TagBucket}),
  TxId = Transaction#transaction.txn_id,
  LogRecord = #log_operation{tx_id = TxId, op_type = tag_update,
  log_payload = #update_log_payload{key = TagKey, type = antidote_crdt_tag, op = TagParams}},
  LogId = ?LOG_UTIL:get_logid_from_key({TagKey, TagBucket}),
  [Node] = Preflist,
  ok = ?LOGGING_VNODE:asyn_append(Node, LogId, LogRecord, {fsm, undefined, self()}),
  NewClientOps = [{{TagKey, TagBucket}, antidote_crdt_tag, TagParams} | SD0#tx_coord_state.client_ops],
  WriteSet = case lists:keyfind(Node, 1, SD0#tx_coord_state.updated_partitions) of
              false ->
                [];
              {Node, WS} ->
                WS
              end,
  NewUpdatedPartitions =
    case WriteSet of
      [] ->
        [{Node, [{{TagKey, TagBucket}, antidote_crdt_tag, TagParams}]} | SD0#tx_coord_state.updated_partitions];
      _ ->
        lists:keyreplace(Node, 1, SD0#tx_coord_state.updated_partitions,
                          {Node, [{{TagKey, TagBucket}, antidote_crdt_tag, TagParams} | WriteSet]})
    end,
  {NewClientOps, NewUpdatedPartitions}.

read_tag_index(TagKey, TagValue) ->
  IndexEntryKey = binary:list_to_bin(binary:bin_to_list(TagKey) ++ atom_to_list('_') ++ binary:bin_to_list(TagValue)),
  antidote:read_objects(ignore, [], [{IndexEntryKey, antidote_crdt_orset, index_bucket}]).

update_tags(TxId, Updates) ->
  case format_tag_ops(Updates) of
    {error, Reason} ->
        {error, Reason};
    TagOperations ->
      case hd(TagOperations) of
          {_, _, noop} ->
              ok;
          _ ->
              {_, _, CoordFsmPid} = TxId,
              case gen_fsm:sync_send_event(CoordFsmPid, {update_tags, hd(TagOperations)}, ?OP_TIMEOUT) of
                ok ->
                    UpdateList = format_update_tag_params(Updates),
                    case antidote:update_objects(UpdateList, TxId) of
                        ok ->
                            ok;
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
              end
      end
  end.

  format_tag_ops(Updates) ->
      try
        lists:map(fun(Update) ->
            {Key, Bucket, TagOp} = case Update of
              {{K, _, B}, _, _, TO} ->
                  {K, B, TO};
              {{K, _, B}, {_, _}} ->
                  {K, B, noop};
              {{K, _, B}, _, _} ->
                  {K, B, noop}
            end,
            {Key, Bucket, TagOp}
          end, Updates)
      catch
          _:Reason ->
              {error, Reason}
      end.
