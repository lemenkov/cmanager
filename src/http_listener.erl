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

-module(http_listener).
-author('lemenkov@gmail.com').

-export([dispatch/1]).

dispatch(Req) ->
	Method = Req:get(method),
	case (Method == 'GET') or (Method == 'PUT') or (Method ==  'POST') or (Method == 'DELETE') of
		true ->
			Path = string:tokens(Req:get(path), "/"),
			PerhapsJson = Req:recv_body(),
			error_logger:info_msg("~p: ~p: ~p~n", [Method, Path, PerhapsJson]),
			{ReplyCode, ReplyJson} = gen_server:call(cmanager, {req, Method, Path, PerhapsJson}),
			Req:respond({ReplyCode, [{"Content-Type", "application/json"}], mochijson2:encode(ReplyJson)});
		false ->
			error_logger:warning_msg("UNKNOWN method: ~p~n", [Method]),
			Headers = [{"Allow", "GET,POST,PUT,DELETE"}],
			Req:respond({405, Headers, "405 Method Not Allowed\r\n"})
	end.
