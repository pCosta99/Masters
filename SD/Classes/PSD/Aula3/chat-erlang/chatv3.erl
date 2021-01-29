-module(chatv3).
-import(rmanager, [init_rooms/0, retrieve/2]).
-import(client, [newUser/2]).
-export([start_server/1]).

start_server(Port) ->
    login_mgr:finish(),  %Finish the maybe existing login manager.
    ets:new(table, [named_table, set, public]),
    {ok, LSock} = gen_tcp:listen(Port, [binary, {active, once}, {packet, line},
                                      {reuseaddr, true}]),
    RoomMap = rmanager:init_rooms(),
    login_mgr:start(), %Launch the login manager
    ets:insert(table, {rooms, RoomMap}),
    spawn(fun() -> acceptor(LSock) end),
    ok.

% Accepts connections from clients
acceptor(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock) end),
    [{_, RoomMap}] = ets:lookup(table, rooms),
    maps:get("default", RoomMap) ! {enter, self()},
    client:newUser(Sock, "default").
