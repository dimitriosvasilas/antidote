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

-module(divergence_metrics_collector).

-behaviour(gen_server).

-export([start_link/0,
        log/2
        ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{}}.

log(LastUpdated, ClockSiOps) ->
  gen_server:cast(?MODULE, {log, LastUpdated, ClockSiOps}).

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast({log, LastUpdated, ClockSiOps}, _State) ->
    LogPath = filename:join(
              app_helper:get_env(riak_core, platform_data_dir), "divergence"),
    Log = case disk_log:open([{name, LogPath}]) of
        {error, R} ->
            {error, R};
        {_, L} ->
            L;
        {repaired, L, _, _} ->
            L
    end,
    case disk_log:log(Log, {LastUpdated, {ClockSiOps}}) of
        ok ->
            ok;
        {error, Reason} ->
            lager:info("write error__~p~n", [Reason])
    end,
    ok = disk_log:sync(Log),
    ok = disk_log:close(Log),
    {noreply, _State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
