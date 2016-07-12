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
-module(antidote_node_event_handler).
-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).
-record(state, {}).

init([]) ->
    {ok, #state{}}.

handle_event({service_update, _Services}, State) ->
    {_,_,DownNodes,_,_} = riak_core_status:ring_status(),
    %lists:foreach(fun(Node) -> io:format("~p Failed\n", [Node]) end, DownNodes),
    lists:foreach(fun(Node) -> riak_core:remove_from_cluster(Node) end, DownNodes), 
    %lists:foreach(fun(Node) -> 
    %                      case riak_core_claimant:remove_member(Node) of
    %                           ok ->
    %                                riak_core_claimant:commit();
    %    		       {error, is_claimant} ->
    %			            riak_core:down(Node);
    %                           Other ->
    %                               Other
    %                      end
    %              end, DownNodes),
    {ok, State}.

handle_call(_Event, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

