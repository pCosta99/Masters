-module(authentication).
-export([authenticate/2]).

% ---------------------------------------------------------- AUTHENTICATION ---------------------------------------------------------- %
authenticate(Sock, DistrictServersPids) ->
	io:format("Let's authenticate!~n"),
	receive
		{tcp, Sock, Data} -> 
			Msg = protocol:decode_msg(Data, 'AuthMessage'),
			User = maps:get(user, Msg),
			Username = maps:get(username, User),
			Password = maps:get(password, User),
			DistrictId = maps:get(districtId, User),
			case maps:get(type, Msg) of
				'LOGIN' -> login(Sock, Username, Password, DistrictServersPids);
				'SIGNUP' -> 
					signup(Sock, Username, Password, DistrictId),
					authenticate(Sock, DistrictServersPids)
			end;
		{tcp_close, _} -> 
			io:format("TCP closed~n"),
			false;
		{tcp_error, _, _} ->
			io:format("RIP TCP~n"),
			gen_tcp:close(Sock)
	end.

login(Sock, Username, Password, DistrictServersPids) ->
	io:format("Logging in! ~n"),
	case login_mgr:login(Username, Password) of
		ok -> 
			{ok, Subs} = topic_mgr:encode_subs(Username),
			DistrictId = login_mgr:get_district(Username),
			io:format("Login sucessful, replying with district id ~p.~n", [DistrictId]),
			send_login_success_reply(Sock, login_mgr:isSick(Username), DistrictId, Subs),
			DistrictPid = maps:get(DistrictId, DistrictServersPids), % needs to be changed when we want to test with diferents servers
			district_rcv:newLogin(Username, Sock),
			menu:menu(Sock, Username, DistrictPid, DistrictServersPids);
		user_already_logged -> 
			send_error_reply(Sock, 'USER_ALREADY_LOGGED', 'LoginReply'),
			authenticate(Sock, DistrictServersPids);
		invalid_password -> 
			send_error_reply(Sock, 'INVALID_PASSWORD', 'LoginReply'),
			authenticate(Sock, DistrictServersPids);
		invalid_username -> 
			send_error_reply(Sock, 'INVALID_USERNAME', 'LoginReply'),
			authenticate(Sock, DistrictServersPids)
	end.

signup(Sock, Username, Password, DistrictId) ->
	io:format("Signing up! ~n"),
	case login_mgr:createAccount(Username, Password, DistrictId) of
		ok ->
			topic_mgr:create_user(Username), 
			send_signup_success_reply(Sock);
		user_exists -> send_error_reply(Sock, 'USER_EXISTS', 'SignupReply')
	end.

% ---------------------------------------------------------- REPLIES ---------------------------------------------------------- %

send_error_reply(Sock, Error, ReplyType) -> 
	Msg = protocol:encode_msg(#{success => false, error => Error}, ReplyType),
	io:format("Error auth reply sent! ~n"),
	gen_tcp:send(Sock, Msg).

send_signup_success_reply(Sock) ->
	Msg = protocol:encode_msg(#{success => true}, 'SignupReply'),
	io:format("Sucess auth reply sent! ~n"),
	gen_tcp:send(Sock, Msg).

send_login_success_reply(Sock, IsSick, DistrictId, Subbed) ->
	Msg = protocol:encode_msg(#{success => true, sick => IsSick, district => DistrictId, subbed => Subbed}, 'LoginReply'),
	io:format("Sucess auth reply sent! ~n"),
	gen_tcp:send(Sock, Msg).