%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 SyncFree Consortium.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(index_manager).

-behaviour(gen_server).

-export([start_link/0,
        update_tag_index/1
        ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{}}.

update_tag_index(Txn) ->
  {_, _, _, _, _, _, _, _, Log_records} = Txn,
  lists:map(fun(LogRecord) ->
    {_, _, _, _, LogOperation} = LogRecord,
    {_, _, _, LogPayload} = LogOperation,
    case LogOperation of
      {_, _, tag_update, _} ->
          {_, _, _, LogPayload} = LogOperation,
          gen_server:cast(?MODULE, {update_tag_index, LogPayload});
          _ ->
            ok
          end
        end, Log_records).

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast({update_tag_index, LogPayload}, _State) ->
  {_, ObjKey, _, _, {_, {{TagKey, _},{_, TagValue}}}} = LogPayload,
  IndexEntryKey = binary:list_to_bin(binary:bin_to_list(TagKey) ++ atom_to_list('_') ++ binary:bin_to_list(TagValue)),
  {ok, _CT} = antidote:update_objects(ignore, [], [{{IndexEntryKey, antidote_crdt_orset, index_bucket}, add, ObjKey}]),
  {noreply, _State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
