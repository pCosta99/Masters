-module(district_rcv).
-export([start/2, newLogin/2, newLogout/1]).

% Register the receiver and the function that will store the user sockets
start(PullSocket, PubSocket) ->
    register(usersSocks, spawn(fun() -> usersSocks(#{}) end)),
    register(receiver, spawn(fun() -> receiver(PullSocket, PubSocket) end)),
    io:format("> Frontend Server is ready to receive messages from District Servers!").

% Start receiving messages from the district servers
receiver(PullSocket, PubSocket) -> 
    {ok, Data} = chumak:recv(PullSocket),
    OperationReply = protocol:decode_msg(Data, 'OperationReply'),
    case maps:get(type, OperationReply) of
		'GET_LOCATION_REPLY' -> 
            Username = maps:get(username, OperationReply),
            io:format("> Received a GET LOCATION Reply for the User: ~p.~n", [Username]),
		    case getUserSock(Username) of 
                error -> ok;  % User not found
                Sock ->  gen_tcp:send(Sock, Data)
            end; 
		'INFECTED_CONTACTS' ->
			io:format("> Received infected contacts, going to notify the authenticated ones.~n"),
            Contacts = maps:get(contacts, OperationReply),
            Usernames = maps:get(usernames, Contacts),
            UsernamesString = lists:map(fun (V) -> binary_to_list(V) end, Usernames),
            SendTo = getCommon(UsernamesString, login_mgr:online()),
            SendToEach = fun (User) -> chumak:send(PubSocket, "infected-c " ++ [User]) end,
            lists:foreach(SendToEach, SendTo);
		'ERROR' ->
            io:format("> Error received!~n");
        _ ->
            io:format("> Unknown message type received from district server~n")
    end,

    receiver(PullSocket, PubSocket).

usersSocks (Users) ->
    receive
        {login, Username, Socket} ->
            usersSocks(maps:put(Username, Socket, Users));

        {logout, Username} ->
            usersSocks(maps:remove(Username, Users));

        {get, Username, From} ->
            case maps:get(Username, Users) of 
                error -> 
                    From ! { getUserSock, error };
                UserSock -> 
                    From ! { getUserSock, UserSock }
            end,
            usersSocks(Users)
    end.

% return Socket or error
getUserSock(Username) ->
    usersSocks ! { get, Username, self() },

    receive
        { getUserSock, error } -> error;
        { getUserSock, UserSock } -> UserSock
    end.

newLogin(Username, Sock)->
    usersSocks ! {login, Username, Sock},
    ok.

newLogout(Username) ->
    usersSocks ! {logout, Username},
    ok.

getCommon(List1,List2) -> 
    F = fun (L) -> lists:filter(fun (V) -> lists:member(V, List2) end, L) end,
    F(List1). 