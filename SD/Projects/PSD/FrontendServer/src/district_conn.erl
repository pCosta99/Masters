-module(district_conn).
-export([start/2]).

% Start the loop to receive messages
start(Id, Push) ->
  io:format("> District connection with id ~p is now running!~n", [Id]),

  % Run the loop to start receiving messages from the menu
  Pid = spawn (fun() -> loop (Push) end),
  Pid.

loop (Push) -> 
    receive
        % Location update message from menu
        {location_update, _, LocationMessage, _} ->
            io:format("> Location update from menu~n"),
            chumak:send(Push, LocationMessage),
            loop(Push);
        {get_number, LocationMessage} ->
            io:format("> Location count request~n"),
            chumak:send(Push, LocationMessage), % send request and continue, the reply will eventually be received in the pull socket
            loop(Push);
        {infected, User} ->
            io:format("> User infected report, sending info to district server.~n"),
            Msg = protocol:encode_msg(#{type => 'SICK', username => User}, 'Operation'),
            chumak:send(Push, Msg),
            loop(Push);
        _-> 
            loop(Push)
    end
.