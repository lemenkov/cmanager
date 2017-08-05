-module(cmanager_test).

-include_lib("eunit/include/eunit.hrl").

user_test_() ->
	{setup,
	 fun() -> cmanager_app:start(), inets:start() end,
	 fun(_) -> cmanager_app:stop(), inets:stop(), timer:sleep(1000) end,
	 [
	  {"Test user creation",
	   fun() ->
               {ok, {{"HTTP/1.1", ReplyCode, _ReplyReason}, _ReplyHeaders, _ReplyBody}} = cmanager_test_helper:user_create(<<"peter_lemenkov">>, <<"testpass">>, <<"lemenkov@gmail.com">>),
               ?assertEqual(200, ReplyCode)
	   end
	  },
	  {"Test user creation (duplicated request)",
	   fun() ->
               {ok, {{"HTTP/1.1", ReplyCode, _}, _, _}} = cmanager_test_helper:user_create(<<"peter_lemenkov">>, <<"testpass">>, <<"lemenkov@gmail.com">>),
               ?assertEqual(200, ReplyCode)
	   end
	  },
	  {"Test user creation (existing login)",
	   fun() ->
               {ok, {{"HTTP/1.1", ReplyCode, _}, _, _}} = cmanager_test_helper:user_create(<<"peter_lemenkov">>, <<"testpass">>, <<"lemenkov+newmail@gmail.com">>),
               ?assertEqual(409, ReplyCode)
	   end
	  },
	  {"Test user creation (existing email)",
	   fun() ->
               {ok, {{"HTTP/1.1", ReplyCode, _}, _, _}} = cmanager_test_helper:user_create(<<"peter_lemenkov2">>, <<"testpass">>, <<"lemenkov@gmail.com">>),
               ?assertEqual(409, ReplyCode)
	   end
	  },
	  {"Test multiple users creation",
	   fun() ->
               % The first one is a duplicated request
               {ok, {{"HTTP/1.1", 200, "OK"}, _, _}} = cmanager_test_helper:user_create(<<"peter_lemenkov">>, <<"testpass">>, <<"lemenkov@gmail.com">>),
               {ok, {{"HTTP/1.1", ReplyCode, _}, _, _}} = cmanager_test_helper:user_create(<<"peter_lemenkov2">>, <<"testpass">>, <<"lemenkov+newmail@gmail.com">>),
               ?assertEqual(200, ReplyCode)
	   end
	  }
	 ]
	}.

machine_test_() ->
	{setup,
	 fun() -> cmanager_app:start(), inets:start() end,
	 fun(_) -> cmanager_app:stop(), inets:stop(), timer:sleep(1000) end,
	 [
	  {"Test machine registration",
	   fun() ->
               {ok, {{"HTTP/1.1", ReplyCode, _ReplyReason}, _ReplyHeaders, _ReplyBody}} = cmanager_test_helper:machine_create(<<"machine1">>, 80),
               ?assertEqual(200, ReplyCode)
	   end
	  },
	  {"Test machine registration (duplicated request)",
	   fun() ->
               {ok, {{"HTTP/1.1", ReplyCode, _ReplyReason}, _ReplyHeaders, _ReplyBody}} = cmanager_test_helper:machine_create(<<"machine1">>, 80),
               ?assertEqual(200, ReplyCode)
	   end
	  },
	  {"Test multiple machines registration",
	   fun() ->
               % The first one is a duplicated request
               {ok, {{"HTTP/1.1", ReplyCode1, _}, _, _}} = cmanager_test_helper:machine_create(<<"machine1">>, 80),
               {ok, {{"HTTP/1.1", ReplyCode2, _}, _, _}} = cmanager_test_helper:machine_create(<<"machine2">>, 180),
               ?assertEqual({200, 200}, {ReplyCode1, ReplyCode2})
	   end
	  }
	 ]
	}.

undocumented_api_test_() ->
	{setup,
	 fun() -> cmanager_app:start(), inets:start() end,
	 fun(_) -> cmanager_app:stop(), inets:stop(), timer:sleep(1000) end,
	 [
	  {"Test user ID retrieval",
	   fun() ->
               {ok, {{"HTTP/1.1", 200, "OK"}, _, JsonStr}} = cmanager_test_helper:user_create(<<"peter_lemenkov">>, <<"testpass">>, <<"lemenkov@gmail.com">>),
	       timer:sleep(100),
	       {ok, {{"HTTP/1.1", 200, "OK"}, _, JsonStr2}} = httpc:request(get, {"http://127.0.0.1:8080/id/user/" ++ binary_to_list(<<"peter_lemenkov">>), []}, [], []),
	       F = fun(J) ->
                   {struct, JP} = mochijson2:decode(J),
		   proplists:get_value(<<"id">>, JP)
               end,
               ?assertEqual(F(JsonStr), F(JsonStr2))
	   end
	  },
	  {"Test machine ID retrieval",
	   fun() ->
               {ok, {{"HTTP/1.1", 200, "OK"}, _, JsonStr}} = cmanager_test_helper:machine_create(<<"machine1">>, 80),
	       timer:sleep(100),
	       {ok, {{"HTTP/1.1", 200, "OK"}, _, JsonStr2}} = httpc:request(get, {"http://127.0.0.1:8080/id/machine/" ++ binary_to_list(<<"machine1">>), []}, [], []),
	       F = fun(J) ->
                   {struct, JP} = mochijson2:decode(J),
		   proplists:get_value(<<"id">>, JP)
               end,
               ?assertEqual(F(JsonStr), F(JsonStr2))
	   end
	  }
	 ]
	}.


coffee_buy_test_() ->
	{setup,
	 fun() ->
             cmanager_app:start(),
	     inets:start(),

	     % Register three users
	     cmanager_test_helper:user_create(<<"peter_lemenkov">>, <<"testpass">>, <<"lemenkov@gmail.com">>),
	     cmanager_test_helper:user_create(<<"another_user2">>, <<"testpass2">>, <<"user2@example.com">>),
	     cmanager_test_helper:user_create(<<"another_user3">>, <<"testpass3">>, <<"user3@example.com">>),

	     % Create three machines
	     cmanager_test_helper:machine_create(<<"machine1">>, 80),
	     cmanager_test_helper:machine_create(<<"machine2">>, 180),
	     cmanager_test_helper:machine_create(<<"machine3">>, 280),
	     
	     UserId1 = cmanager_test_helper:get_id(<<"user">>, <<"peter_lemenkov">>),
	     UserId2 = cmanager_test_helper:get_id(<<"user">>, <<"another_user2">>),
	     
	     MachineId1 = cmanager_test_helper:get_id(<<"machine">>, <<"machine1">>),
	     MachineId2 = cmanager_test_helper:get_id(<<"machine">>, <<"machine2">>),
	     
	     {ok, {{"HTTP/1.1", 200, "OK"}, _, _}} = httpc:request(get, {"http://127.0.0.1:8080/coffee/buy/:" ++ binary_to_list(UserId1) ++ "/:" ++ binary_to_list(MachineId1), []}, [], []),
	     {ok, {{"HTTP/1.1", 200, "OK"}, _, _}} = httpc:request(get, {"http://127.0.0.1:8080/coffee/buy/:" ++ binary_to_list(UserId2) ++ "/:" ++ binary_to_list(MachineId2), []}, [], []),
	     {ok, {{"HTTP/1.1", 200, "OK"}, _, _}} = httpc:request(get, {"http://127.0.0.1:8080/coffee/buy/:" ++ binary_to_list(UserId1) ++ "/:" ++ binary_to_list(MachineId2), []}, [], []),
	     {ok, {{"HTTP/1.1", 200, "OK"}, _, _}} = httpc:request(get, {"http://127.0.0.1:8080/coffee/buy/:" ++ binary_to_list(UserId2) ++ "/:" ++ binary_to_list(MachineId1), []}, [], []),
	     {ok, {{"HTTP/1.1", 200, "OK"}, _, _}} = httpc:request(get, {"http://127.0.0.1:8080/coffee/buy/:" ++ binary_to_list(UserId2) ++ "/:" ++ binary_to_list(MachineId1), []}, [], []),
	     
	     ok
	 end,
	 fun(_) -> cmanager_app:stop(), inets:stop(), timer:sleep(1000) end,
	 [
	  {"Test coffee ordering for the 1st user",
	   fun() ->
	       Id = cmanager_test_helper:get_id(<<"user">>, <<"peter_lemenkov">>),
	       {ok, {{"HTTP/1.1", 200, "OK"}, _, JsonStr}} = httpc:request(get, {"http://127.0.0.1:8080/stats/coffee/user/:" ++ binary_to_list(Id), []}, [], []),
               ListOfJsons = mochijson2:decode(JsonStr),
               ?assertEqual(2, length(ListOfJsons))
	   end
	  },
	  {"Test coffee ordering for the 2nd user",
	   fun() ->
	       Id = cmanager_test_helper:get_id(<<"user">>, <<"another_user2">>),
	       {ok, {{"HTTP/1.1", 200, "OK"}, _, JsonStr}} = httpc:request(get, {"http://127.0.0.1:8080/stats/coffee/user/:" ++ binary_to_list(Id), []}, [], []),
               ListOfJsons = mochijson2:decode(JsonStr),
               ?assertEqual(3, length(ListOfJsons))
	   end
	  },
	  {"Test coffee ordering for the 3rd user",
	   fun() ->
	       Id = cmanager_test_helper:get_id(<<"user">>, <<"another_user3">>),
	       {ok, {{"HTTP/1.1", 200, "OK"}, _, JsonStr}} = httpc:request(get, {"http://127.0.0.1:8080/stats/coffee/user/:" ++ binary_to_list(Id), []}, [], []),
               ListOfJsons = mochijson2:decode(JsonStr),
               ?assertEqual(0, length(ListOfJsons))
	   end
	  },
	  {"Test coffee ordering for the 1st machine",
	   fun() ->
	       Id = cmanager_test_helper:get_id(<<"machine">>, <<"machine1">>),
	       {ok, {{"HTTP/1.1", 200, "OK"}, _, JsonStr}} = httpc:request(get, {"http://127.0.0.1:8080/stats/coffee/machine/:" ++ binary_to_list(Id), []}, [], []),
               ListOfJsons = mochijson2:decode(JsonStr),
               ?assertEqual(3, length(ListOfJsons))
	   end
	  },
	  {"Test coffee ordering for the 2nd machine",
	   fun() ->
	       Id = cmanager_test_helper:get_id(<<"machine">>, <<"machine2">>),
	       {ok, {{"HTTP/1.1", 200, "OK"}, _, JsonStr}} = httpc:request(get, {"http://127.0.0.1:8080/stats/coffee/machine/:" ++ binary_to_list(Id), []}, [], []),
               ListOfJsons = mochijson2:decode(JsonStr),
               ?assertEqual(2, length(ListOfJsons))
	   end
	  },
	  {"Test coffee ordering for the 3rd machine",
	   fun() ->
	       Id = cmanager_test_helper:get_id(<<"machine">>, <<"machine3">>),
	       {ok, {{"HTTP/1.1", 200, "OK"}, _, JsonStr}} = httpc:request(get, {"http://127.0.0.1:8080/stats/coffee/machine/:" ++ binary_to_list(Id), []}, [], []),
               ListOfJsons = mochijson2:decode(JsonStr),
               ?assertEqual(0, length(ListOfJsons))
	   end
	  },
	  {"Test coffee ordering for the 1st user (with forged timestamp)",
	   fun() ->
	       UserId = cmanager_test_helper:get_id(<<"user">>, <<"another_user3">>),
	       MachineId = cmanager_test_helper:get_id(<<"machine">>, <<"machine3">>),
	       Url = "http://127.0.0.1:8080/coffee/buy/:" ++ binary_to_list(UserId) ++ "/:" ++ binary_to_list(MachineId),
	       Time = iso8601:format(calendar:universal_time()),
	       Json = [{<<"timestamp">>,Time}],
	       Headers = [],
	       ContentType = "application/json",
	       Body = iolist_to_binary(mochijson2:encode(Json)),
	       {ok, {{"HTTP/1.1", 200, "OK"}, _, _}} = httpc:request(put, {Url, Headers, ContentType, Body}, [], []),
	       {ok, {{"HTTP/1.1", 200, "OK"}, _, JsonStr}} = httpc:request(get, {"http://127.0.0.1:8080/stats/coffee/user/:" ++ binary_to_list(UserId), []}, [], []),
               [{struct, JsonProplist}] = mochijson2:decode(JsonStr),
               ?assertEqual(Time, proplists:get_value(<<"timestamp">>, JsonProplist))
	   end
	  }
	 ]
	}.


