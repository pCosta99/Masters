-module(server).
-export([start_server/3]).

start_server(Port, DistrictInfoMap, DistrictServersPids) -> 
	login_mgr:finish(),  %Finish the maybe existing login manager.
    topic_mgr:finish(),
    {ok, LSock} = gen_tcp:listen(Port, [binary, {active, true}, {packet, 0},
                                      {reuseaddr, true}]),
    login_mgr:start(), %Launch the login manager
    topic_mgr:start(),
    spawn(fun() -> acceptor(LSock, DistrictInfoMap, DistrictServersPids) end), % pass the map with district info to  all new clients
	io:format("> Frontend Server is listening for connections at 12345! ~n"),
    ok.

% Accepts connections from clients
acceptor(LSock, DistrictInfoMap, DistrictServersPids) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock, DistrictInfoMap, DistrictServersPids) end),
    send_entry_msg(Sock, DistrictInfoMap),
    authentication:authenticate(Sock, DistrictServersPids).

% Send the entry message containing the district map info
send_entry_msg(Sock, DistrictInfoMap) ->
	Msg = protocol:encode_msg(#{districts => DistrictInfoMap}, 'EntryMessage'),
	io:format("Entry message sent! ~n"),
	gen_tcp:send(Sock, Msg).