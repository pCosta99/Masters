%A room manager for our chat

-module(rmanager).
-export([init_rooms/0, create_room/2, retrieve/2, room/2, retrieveNoCreate/2]).

init_rooms() -> #{"default" => spawn(fun()-> room([], "default") end)}.

create_room(RoomMap, NewRoomName) ->
    maps:put(NewRoomName, spawn(fun() -> room([], NewRoomName) end), RoomMap).

retrieve(RoomMap, RoomName) ->
    case maps:find(RoomName, RoomMap) of
        {ok, Room} ->
            %io:format("retrieving room named ~p~n", [RoomName]),
            {ok, {RoomMap, Room}};
         _ ->
            io:format("creating room named ~p~n", [RoomName]),
            NewMap = create_room(RoomMap, RoomName),
            Room = maps:get(RoomName, NewMap),
            {new, {NewMap, Room}}
    end.

% Particular case of the one above.
retrieveNoCreate(RoomMap, RoomName) ->
    case maps:find(RoomName, RoomMap) of
        {ok, Room} ->
            %io:format("retrieving room named ~p~n", [RoomName]),
            {ok, Room};
         _ ->
            not_ok
    end.

%Format the data so it tells us the room it came from
format_data(RoomName, UserName, Data) ->
      list_to_binary(UserName ++ " (" ++ RoomName ++ ") : " ++ binary_to_list(Data)).

% Rooms receive the requests in the server
room(Pids, Name) ->
  receive
    {enter, Pid} ->
      io:format("user entered in room ~p~n", [Name]),
      room(lists:usort([Pid | Pids]), Name);
    {line, Data, User} ->
      io:format("received ~p in ~p from ~p~n", [Data, Name, User]),
      Msg = {line, setelement(2, Data, format_data(Name, User, element(2,Data)))},
      [Pid ! Msg || Pid <- Pids],
      room(Pids, Name);
    {leave, Pid} ->
      io:format("user left~n", []),
      room(Pids -- [Pid], Name)
  end.

