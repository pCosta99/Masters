-module(menu).
-export([menu/4]).

menu(Sock, Username, DistrictPid, DistrictServersPids) ->
	io:format("~p is now in the main menu!~n", [Username]),
	receive
		{tcp, Sock, Data} -> 
			Op = protocol:decode_msg(Data, 'Operation'),
			case maps:get(type, Op) of
				'LOGOUT' -> 
					login_mgr:logout(Username),
					authentication:authenticate(Sock, DistrictServersPids);
				'FLIP_SICK' ->
					login_mgr:flipSick(Username, DistrictPid),
					menu(Sock, Username, DistrictPid, DistrictServersPids);
				'NOTIFICATION' -> 
					NotOp = maps:get(nop, Op),
					case maps:get(type, NotOp) of
						'SUB' -> 
							topic_mgr:insert_topic(Username, maps:get(topic, NotOp), maps:get(district, NotOp), maps:get(location, NotOp)),
							menu(Sock, Username, DistrictPid, DistrictServersPids);
						'UNSUB' ->
							topic_mgr:remove_topic(Username, maps:get(topic, NotOp), maps:get(district, NotOp), maps:get(location, NotOp)),
							menu(Sock, Username, DistrictPid, DistrictServersPids)
					end;
				'LOCATION' -> 
					LocOp = maps:get(lop, Op),
					case maps:get(type, LocOp) of
						'UPDATE' -> 
							io:format("~p location is ~p.~n", [Username, maps:get(location, LocOp)]),
							DistrictPid ! {location_update, Username, Data, self()}, % Ask the district pid to update my location
							menu(Sock, Username, DistrictPid, DistrictServersPids);
						'GET' -> % obtain the number of people in location
							DistrictPid ! {get_number, Data},
							menu(Sock, Username, DistrictPid, DistrictServersPids)
					end
			end;
		{tcp_close, _} -> 
			io:format("Logging ~p out! ~n", [Username]),
			login_mgr:logout(Username),
			exit(normal);
		{tcp_error, _, _} ->
			io:format("Logging ~p out! ~n", [Username]),
			login_mgr:logout(Username),
			exit(normal)
	end.