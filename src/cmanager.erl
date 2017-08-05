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

-module(cmanager).

-behaviour(gen_server).
-compile({parse_transform, do}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    rand:seed(exsplus),
    {ok, Args}.

handle_call({req, Method, Path, PerhapsJson}, From, State) ->
    spawn(fun() -> process_raw_request(Method, Path, PerhapsJson, From) end),
    {noreply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

% Allow empty payload (treat it as an empty JSON struct)
process_raw_request(Method, Path, <<>>, From) ->
    process_json_request(Method, Path, [], From);
process_raw_request(Method, Path, PerhapsJson, From) ->
    case (catch mochijson2:decode(PerhapsJson)) of
	    {struct, Json} ->
		    process_json_request(Method, Path, Json, From);
	    Any ->
		    % FIXME should we just die here? W/o reply.
		    error_logger:warning_msg("Not a valid JSON ~p.~nError (400): ~p~n", [PerhapsJson, Any]),
		    gen_server:reply(From, {400, [{<<"error_text">>,<<"not a valid JSON">>}, {<<"error_code">>, 400}]})
    end.

process_json_request('PUT', ["coffee", "buy", [ $: | UserId], [ $: | MachineId] ], Json, From) ->
        error_logger:warning_msg("PUT coffee buy: ~p ~p.~n", [UserId, MachineId]),
	Timestamp = proplists:get_value(<<"timestamp">>, Json),
	%<<Caffeine>> = list_to_binary(ets:match(machines, {'_', '$1', list_to_binary(MachineId)})),
	[[Caffeine]] = ets:match(machines, {'_', '$1', list_to_binary(MachineId)}),
        error_logger:warning_msg("PUT coffee buy: ~p ~p ~p ~p.~n", [UserId, MachineId, Timestamp, Caffeine]),
	ets:insert_new(transactions, {os:timestamp(), Timestamp, list_to_binary(UserId), list_to_binary(MachineId), Caffeine}),
	gen_server:reply(From, {200, []});

process_json_request('GET', ["coffee", "buy", [ $: | UserId], [ $: | MachineId] ], Json, From) ->
        error_logger:warning_msg("GET coffee buy: ~p ~p.~n", [UserId, MachineId]),
	process_json_request('PUT', ["coffee", "buy", [ $: | UserId], [ $: | MachineId] ], Json ++ [{<<"timestamp">>, iso8601:format(calendar:universal_time())}], From);

process_json_request('GET', ["stats", "coffee"], Json, From) ->
        error_logger:warning_msg("GET stats coffee.~n"),
	Result = return_transactions('_', '_'),
	gen_server:reply(From, {200, Result});

process_json_request('GET', ["stats", "coffee", "machine", [ $: | MachineId] ], Json, From) ->
        error_logger:warning_msg("GET stats coffee machine: ~p.~n", [MachineId]),
	Result = return_transactions('_', list_to_binary(MachineId)),
	gen_server:reply(From, {200, Result});

process_json_request('GET', ["stats", "coffee", "user", [ $: | UserId] ], Json, From) ->
        error_logger:warning_msg("GET stats coffee user: ~p.~n", [UserId]),
	Result = return_transactions(list_to_binary(UserId), '_'),
	gen_server:reply(From, {200, Result});

process_json_request('GET', ["stats", "level", "user", [ $: | UserId] ], Json, From) ->
        error_logger:warning_msg("GET stats level user: ~p.~n", [UserId]),
	gen_server:reply(From, {501, [{<<"error_text">>,<<"not implemented here.">>}, {<<"error_code">>, 501}]});

%%
%% Undocumented API
%%

process_json_request('GET', ["id", "user", Login], _, From) ->
        error_logger:warning_msg("[UNDOCUMENTED] GET id user: ~p.~n", [Login]),
	[[UserId]] = ets:match(users, {list_to_binary(Login), '_', '_', '$1'}),
	gen_server:reply(From,
			 {200, [
				{<<"login">>, Login},
				{<<"id">>, UserId}
			       ]
			 }
			);
process_json_request('GET', ["id", "machine", Name], _, From) ->
        error_logger:warning_msg("[UNDOCUMENTED] GET id machine: ~p.~n", [Name]),
	[[MachineId]] = ets:match(machines, {list_to_binary(Name), '_', '$1'}),
	gen_server:reply(From,
			 {200, [
				{<<"name">>, Name},
				{<<"id">>, MachineId}
			       ]
			 }
	);

process_json_request('PUT', ["user", "request"], Json, From) ->
	% FIXME validate with Jesse
	Login = proplists:get_value(<<"login">>, Json),
	Password = proplists:get_value(<<"password">>, Json),
	Email = proplists:get_value(<<"email">>, Json),

	% Check for already existing login
	PossibleEmail = ets:match(users, {Login, '_', '$1', '_'}),
	% Check for already existing email
	PossibleLogin = ets:match(users, {'$1', '_', Email, '_'}),

	%error_logger:error_msg("DATA: ~p ~p ~p ~p~n", [Login, Email, PossibleLogin, PossibleEmail]),

	case {PossibleLogin, PossibleEmail} of
		{[], []} -> % Register a new user
			UserId = make_hash("user-"),
			ets:insert_new(users, {Login, Password, Email, UserId}),
			gen_server:reply(From,
					 {200, [
						{<<"login">>, Login},
						{<<"password">>, Password},
						{<<"email">>, Email},
						{<<"id">>, UserId}
					       ]
					 }
			);
		{[[Login]], [[Email]]} -> % Duplicated registration attempt (FIXME password change)
			[[UserId]] = ets:match(users, {Login, '_', '_', '$1'}),
			gen_server:reply(From,
					 {200, [
						{<<"login">>, Login},
						{<<"password">>, Password},
						{<<"email">>, Email},
						{<<"id">>, UserId}
					       ]
					 }
			);
		{[[_]], _} ->  % Email already taken
			gen_server:reply(From,
					 % FIXME should it be 303 See Other?
					 {409, [
						{<<"error_code">>, 409},
						{<<"error_text">>, <<"username already taken">>},
						{<<"email">>, Email}
					       ]
					 }
			);
		{_, [[_]]} -> % Login already taken
			gen_server:reply(From,
					 % FIXME should it be 303 See Other?
					 % FIXME should we say that email is known? Privacy concern.
					 {409, [
						{<<"error_code">>, 409},
						{<<"error_text">>, <<"email already taken">>},
						{<<"login">>, Login}
					       ]
					 }
			)
	end;

process_json_request('POST', ["machine"], Json, From) ->
	% FIXME validate with Jesse
	Name = proplists:get_value(<<"name">>, Json),
	Caffeine = proplists:get_value(<<"caffeine">>, Json),
	MachineId = make_hash("machine-"),
	case ets:insert_new(machines, {Name, Caffeine, MachineId}) of
		true ->
			gen_server:reply(From,
					 % FIXME should it be 201?
					 {200, [
						{<<"name">>, Name},
						{<<"caffeine">>, Caffeine},
						{<<"id">>, MachineId}
					       ]
					 }
			);
		false ->
			% FIXME machine exists. Update info or return existing data?
			[[OldCaffeine, OldMachineId]] = ets:match(machines, {Name, '$1', '$2'}),
			gen_server:reply(From,
					 % FIXME should it be 3xx REDIRECTED?
					 {200, [
						{<<"name">>, Name},
						{<<"caffeine">>, OldCaffeine},
						{<<"id">>, OldMachineId}
					       ]
					 }
			)
	end;
	
process_json_request(_Method, _Path, _Json, From) ->
	gen_server:reply(From, {488, [{<<"error_text">>,<<"Don't know what to do.">>}, {<<"error_code">>, 488}]}).

return_transactions(User, Machine) ->
	Result = ets:select(transactions, [{ {'_', '_', User, Machine, '_'}, [], ['$_'] } ]),
        error_logger:warning_msg("Stats: ~p.~n", [Result]),
	[
	 begin
		 [[L, P, E]] = ets:match(users, {'$1', '$2', '$3', UId}),
		 error_logger:warning_msg("Stats1: ~p.~n", [{[L, P, E], UId}]),
		 [[M]] = ets:match(machines, {'$1', '_', MId}),
		 error_logger:warning_msg("Stats2: ~p.~n", [{[M], MId}]),
		 [
		  {<<"timestamp">>, TS},
		  {<<"machine">>, [
				   {<<"name">>, M}, { <<"id">>, MId}
				  ]},
		  {<<"user">>, [
				{<<"login">>, L}, { <<"password">>, P}, {<<"email">>, E}
			       ]}
		 ]
	 end ||  {_, TS, UId, MId, _CL} <- Result].

make_hash(Prefix) ->
	[Hash] = io_lib:format("~p", [erlang:phash2({node(), os:timestamp()})]),
	list_to_binary(Prefix ++ Hash).
