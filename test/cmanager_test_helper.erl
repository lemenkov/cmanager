-module(cmanager_test_helper).
-compile(export_all).

json_request(Request, Url, Json) ->
	Headers = [],
	ContentType = "application/json",
	Body = iolist_to_binary(mochijson2:encode(Json)),
        httpc:request(Request, {Url, Headers, ContentType, Body}, [], []).

user_create(Username, Password, Email) ->
	Json = [ {<<"login">>, Username},{<<"password">>, Password},{<<"email">>,Email}],
	Url = "http://127.0.0.1:8080/user/request",
	json_request(put, Url, Json).

machine_create(Name, Level) ->
	Json = [ {<<"name">>, Name},{<<"caffeine">>, Level}],
	Url = "http://127.0.0.1:8080/machine",
	json_request(post, Url, Json).

get_id(Type, Name) ->
	{ok, {{"HTTP/1.1", 200, "OK"}, _, JsonStr}} = httpc:request(get, {"http://127.0.0.1:8080/id/" ++ binary_to_list(Type) ++ "/" ++ binary_to_list(Name), []}, [], []),
        {struct, JsonProplist} = mochijson2:decode(JsonStr),
	proplists:get_value(<<"id">>, JsonProplist).
