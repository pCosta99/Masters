%%%-------------------------------------------------------------------
%% @doc frontend_server app public API
%% @end
%%%-------------------------------------------------------------------

-module(frontend_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    { {PortsMap, DistrictInfoMap}, {LocalPullPort, LocalPubPort} } = read_json(), % Read the district servers json
    DistrictServersPids = connectToDistrictServers(PortsMap, #{}, maps:iterator(PortsMap)), % connect to all district servers
    io:format("> The local pull port where we are going to bind is ~p~n", [LocalPullPort]),
    LocalPullSocket = createZeroMqLocalPullSocket(LocalPullPort),
    LocalPubSocket = createZeroMqLocalPubSocket(LocalPubPort),
    district_rcv:start(LocalPullSocket, LocalPubSocket),
    server:start_server(12345, DistrictInfoMap, DistrictServersPids),
    frontend_server_sup:start_link().

stop(_State) ->
    ok.

%%%-------------------------------------------------------------------
%%                 FRONTEND SERVER CONFIG JSON PARSE
%%%-------------------------------------------------------------------

% Read the json containing the frontend config
read_json() -> 
	{ok, Data} = file:read_file("include/frontend_config.json"),

    % Districts info parse
	ListDistricts = maps:get(<<"districts">>, jsone:decode(Data)),
    FuncInfo = fun(N) -> json_district_info_entry_convert(N) end,
    FuncPorts = fun(N) -> json_district_ports_entry_convert(N) end,
    DistrictInfoList = lists:map(FuncInfo, ListDistricts),
	DistrictPortsList = lists:map(FuncPorts, ListDistricts),
	DistrictInfoMap = maps:from_list(DistrictInfoList),
	DistrictPortsMap = maps:from_list(DistrictPortsList),

    % Frontend config parse
    FrontendConfig = maps:get(<<"frontend-server">>, jsone:decode(Data)),
    LocalPullPort = maps:get(<<"pull">>, FrontendConfig),
    LocalPubPort = maps:get(<<"publisher">>, FrontendConfig),
	
	{ {DistrictPortsMap, DistrictInfoMap} , {LocalPullPort, LocalPubPort} }.

json_district_info_entry_convert(Entry) -> 
	Id = maps:get(<<"id">>, Entry),
	InfoList = maps:get(<<"info">>, Entry),
	Name = binary_to_list(maps:get(<<"district">>, InfoList)),
	Size = maps:get(<<"size">>, InfoList),
	Info = { Id, #{name => Name, size => Size} },
	Info.

json_district_ports_entry_convert(Entry) -> 
    Id = maps:get(<<"id">>, Entry),
	ServerList = maps:get(<<"server">>, Entry),
	PullPort = maps:get(<<"pull">>, ServerList),
	Ports = { Id, #{pull => PullPort} },
    Ports.

%%%-------------------------------------------------------------------
%%              DISTRICT SERVERS CONNECTION AND SOCKET CREATION
%%%-------------------------------------------------------------------

% connect to district servers
connectToDistrictServers(PortsMap, DistrictServersPidsMap, Iterator) -> 
    case maps:next(Iterator) of
        none -> 
            DistrictServersPidsMap; % Return the map containing all pids

        {Id, SocketPorts, NextIterator} ->
            DistrictServerPullPort = maps:get(pull, SocketPorts),
            io:format("DistrictServer with id: ~p, with Pull Port: ~p~n",[Id, DistrictServerPullPort]),
            Push = createZeroMqPushSocket(DistrictServerPullPort),
            DistrictServerPid = district_conn:start(Id, Push), % Start the district server connection to send messages
            NewMap = maps:put(Id, DistrictServerPid, DistrictServersPidsMap), % Update the map with district server pid
            connectToDistrictServers(PortsMap, NewMap, NextIterator)
        end.

% create the push socket and connect the push to district server
createZeroMqPushSocket(DistrictServerPullPort) ->
    % create push socket
    {ok, Push} = chumak:socket(push),
    % connect to pull
    case chumak:connect(Push, tcp, "localhost", DistrictServerPullPort) of
        {ok, _} ->
            io:format("> Push Connected successful with PID: ~p\n", [Push]);
        {error, Reason_Push} ->
            io:format("> Push Connection failed: ~p\n", [Reason_Push]);
        X_Push ->
            io:format("> Push Unhandled reply ~p \n", [X_Push])
        end,
    Push.

% create local pull socket and bind it
createZeroMqLocalPullSocket(LocalPullPort) ->
    % create pull socket    
    {ok, Pull} = chumak:socket(pull),
    % bind this socket
    case chumak:bind(Pull, tcp, "localhost", LocalPullPort) of
        {ok, _} ->
            io:format("> Pull Binded successful with PID: ~p\n", [Pull]);
        {error, Reason_Pull} ->
            io:format("> Pull Bind failed: ~p\n", [Reason_Pull]);
        X_Pull ->
            io:format("> Pull Unhandled reply ~p \n", [X_Pull])
        end,
    Pull.

% create local pull socket and bind it
createZeroMqLocalPubSocket(LocalPubPort) ->
    % create pull socket    
    {ok, Pub} = chumak:socket(pub),
    % bind this socket
    case chumak:bind(Pub, tcp, "localhost", LocalPubPort) of
        {ok, _} ->
            io:format("> Pull Binded successful with PID: ~p\n", [Pub]);
        {error, Reason_Pub} ->
            io:format("> Pull Bind failed: ~p\n", [Reason_Pub]);
        X_Pub ->
            io:format("> Pull Unhandled reply ~p \n", [X_Pub])
        end,
    Pub.