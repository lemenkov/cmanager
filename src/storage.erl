%% Copyright (c) 2017 Peter Lemenkov.
%%
%% The MIT License
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%%

-module(storage).
-author('lemenkov@gmail.com').

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
	process_flag(trap_exit, true),
	machines = ets:new(machines, [public, named_table]),
	users = ets:new(users, [public, named_table]),
	transactions = ets:new(transactions, [public, named_table]),
        error_logger:warning_msg("storage: started at ~p.~n", [node()]),
	{ok, [machines, users, emails, transactions]}.

handle_call(Call, _From, State) ->
	error_logger:error_msg("storage: strange call: ~p~n", [Call]),
	{stop, {error, {unknown_call, Call}}, State}.

handle_cast(Cast, State) ->
	error_logger:error_msg("storage: strange cast: ~p~n", [Cast]),
	{stop, {error, {unknown_cast, Cast}}, State}.

handle_info(Info, State) ->
	error_logger:error_msg("storage: strange info: ~p~n", [Info]),
	{stop, {error, {unknown_info, Info}}, State}.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

terminate(Reason, _State) ->
	{memory, Bytes} = erlang:process_info(self(), memory),
	error_logger:warning_msg("storage backend: terminated due to reason [~p] (allocated ~b bytes)", [Reason, Bytes]).
