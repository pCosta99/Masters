-module(client).
-import(rmanager, [init_rooms/0, retrieve/2]).
-export([newUser/2]).

get_line_tcp(Sock, Prompt) ->
    inet:setopts(Sock, [{active, false}]),
    gen_tcp:send(Sock, Prompt),
    case gen_tcp:recv(Sock, 0, 100000) of
        {ok, Packet} ->
            % Strip \r and \n from the message
            string:trim(binary_to_list(Packet));
        {error, _} ->
            io:format("Got an error bro ~n")
            %{error, Reason}
    end.

% Process of logging in
login_procedure(Sock) ->
    case list_to_integer(get_line_tcp(Sock,"1) Login \n2) Register\n")) of
        1 ->
            User = get_line_tcp(Sock,"Username: "),
            Pass = get_line_tcp(Sock,"Password: "),
            inet:setopts(Sock, [{active, once}]), % goes back to normal way of reading I think
            case login_mgr:login(User, Pass) of
                ok ->
                    gen_tcp:send(Sock, "\nSucess! You are now in the default room!\n");
                user_already_logged ->
                    gen_tcp:send(Sock, "\nThis user is already logged!\n Currently there is no support for multiple customers.\n");
                invalid_credentials ->
                    gen_tcp:send(Sock, "\nInvalid credentials were provided.\nForwarding into login interface again.\n\n"),
                    login_procedure(Sock)
            end;
        2 ->
            User = get_line_tcp(Sock,"Username: "),
            Pass = get_line_tcp(Sock,"Password: "),
            inet:setopts(Sock, [{active, once}]), % goes back to normal way of reading I think
            case login_mgr:createAccount(User, Pass) of
                user_exists ->
                    gen_tcp:send(Sock, "\nUsername already exists!\nForwarding into login interface again.\n\n"),
                    login_procedure(Sock);
                ok ->
                    gen_tcp:send(Sock, "\nSucess! You are now in the default room!\n")
            end
    end,
    User.

% Called once to "init" the user
newUser(Sock, RoomName) ->
    User = login_procedure(Sock),
    io:format("Finished login procedure for ~p!~n", [User]),
    user(Sock, RoomName, User).

% Helper function
format(Data) ->
    % Formatting the string we just obtained
    Fun = fun(X) -> lists:sublist(X, 3, length(X)) end,
    Message = Fun(string:find(binary_to_list(Data), ": ")),
    Message.

% Parses and responds to command messages (join, logout, etc..)
parse_command(Text, Sock, RoomName, Self, RoomMap, User) ->
    case string:split(Text," ") of
        ["join", Value] ->
            NewRoom = string:trim(Value),
            {_, {NewMap, Room}} = rmanager:retrieve(RoomMap, NewRoom),
            Room ! {enter, Self},
            ets:insert(table, {rooms, NewMap}),
            user(Sock, NewRoom, User);
        ["leave", Value] ->
            NewRoom = string:trim(Value),
            case rmanager:retrieveNoCreate(RoomMap, NewRoom) of
                {ok, Room} ->
                    Room ! {leave, Self},
                    if
                        NewRoom == RoomName ->
                           user(Sock, "default", User);
                        true ->
                            user(Sock, RoomName, User)
                    end;
                not_ok ->
                    io:format("The room requested to leave doesn't exist.~n"),
                    user(Sock, RoomName, User)
            end
    end.

% Respond to user requests
user(Sock, RoomName, User) ->
    [{_, RoomMap}] = ets:lookup(table, rooms),
    Self = self(),
    receive
        {line, {Self, Data}} ->
            inet:setopts(Sock, [{active, once}]),
            case string:prefix(format(Data),"\\") of
                nomatch ->
                    gen_tcp:send(Sock, Data),
                    user(Sock, RoomName, User);
                Value -> parse_command(Value, Sock, RoomName, Self, RoomMap, User)
            end;
        {line, {_, Data}} ->
            % Avoid broadcasting command messages
            case string:prefix(format(Data), "\\") of
                nomatch ->
                    gen_tcp:send(Sock, Data),
                    user(Sock, RoomName, User);
                _ ->
                    user(Sock, RoomName, User)
            end;
        {tcp, _, Data} ->
            case rmanager:retrieve(RoomMap, RoomName) of
                {_, {_, Room}} ->
                    Room ! {line, {Self, Data}, User},
                    user(Sock, RoomName, User)
            end;
        {tcp_closed, _} ->
            maps:get(RoomName, RoomMap) ! {leave, self()};
        {tcp_error, _, _} ->
            maps:get(RoomName, RoomMap) ! {leave, self()}
    end.

