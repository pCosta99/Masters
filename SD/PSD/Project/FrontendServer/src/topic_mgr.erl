-module(topic_mgr).
-export([start/0, finish/0, create_user/1, insert_topic/4, remove_topic/4, print_subs/0, encode_subs/1, priv_encode_subs/2, cleanup/1]).

start() -> register(?MODULE,  spawn(fun() -> loop(#{}) end)).

finish() ->
    case whereis(?MODULE) of
        undefined -> ok;
        Pid -> exit(Pid, kill)
    end.

rpc(Request) ->
    ?MODULE ! {Request, self()},
    receive
        {Result, ?MODULE} -> Result
    end.

create_user(User) -> rpc({create_user, User}).

insert_topic(User, Topic, District, Loc) -> rpc({insert_topic, User, Topic, District, Loc}).

remove_topic(User, Topic, District, Loc) -> rpc({remove_topic, User, Topic, District, Loc}).

print_subs() -> rpc({print_subs}).

encode_subs(User) -> rpc({encode_subs, User}).

% Subscriptions keeps info about user's subscriptions for each district (up to a maximum of 3)
loop(Subscriptions) ->
    receive
        {{create_user, User}, From} -> 
            From ! { ok, ?MODULE },
            loop(maps:put(User, #{}, Subscriptions));
        {{insert_topic, User, Topic, District, Loc}, From} -> 
            {ok, DistrictMap} = maps:find(User, Subscriptions),
            case maps:find(District, DistrictMap) of
                % If the district and the location is already part we can just add the topic to the list
                {ok, Locs} -> 
                    case maps:find(Loc,Locs) of
                        {ok, List} ->                
                            NewLocsMap = maps:update(Loc, lists:append(List, [Topic]), Locs),
                            NewDistrictMap = maps:update(District, NewLocsMap, DistrictMap),
                            From ! { ok, ?MODULE },
                            loop(maps:update(User, NewDistrictMap, Subscriptions));
                        _ -> 
                            NewLocsMap = maps:put(Loc, [Topic], #{}),
                            NewDistrictMap = maps:put(District, NewLocsMap, DistrictMap),
                            From ! { ok, ?MODULE },
                            loop(maps:update(User, NewDistrictMap, Subscriptions))
                    end;
                % Otherwise, we need to check if we have hitted the limit of 3 districts before creating a new entry
                _ -> 
                    Size = maps:size(DistrictMap),
                    if Size < 3 ->
                        NewLocsMap = maps:put(Loc, [Topic], #{}),
                        NewDistrictMap = maps:put(District, NewLocsMap, DistrictMap),
                        From ! { ok, ?MODULE },
                        loop(maps:update(User, NewDistrictMap, Subscriptions));
                    true ->
                        From ! { max_districts, ?MODULE },
                        loop(Subscriptions)
                    end
            end;
        {{remove_topic, User, Topic, District, Loc}, From} ->
            {ok, DistrictMap} = maps:find(User, Subscriptions),
            case maps:find(District, DistrictMap) of
                {ok, Locs} ->
                    case maps:find(Loc, Locs) of
                        {ok, List} -> 
                            case lists:member(Topic, List) of
                                true ->
                                    NewLocsMap = maps:update(Loc, lists:delete(Topic, List), Locs),
                                    NewDistrictMap = maps:update(District, NewLocsMap, DistrictMap),
                                    From ! { ok, ?MODULE },
                                    NewSubs = maps:update(User, NewDistrictMap, Subscriptions), 
                                    loop(cleanup(NewSubs));
                                false -> 
                                    From ! { subscription_doesnt_exist, ?MODULE },
                                    loop(Subscriptions)
                            end;
                        _ -> 
                            From ! { location_not_subscribed, ?MODULE },
                            loop(Subscriptions)
                    end;
                _ ->
                    From ! { district_not_subscribed, ?MODULE },
                    loop(Subscriptions)
            end;
        {{print_subs}, From} ->
            io:format("~p~n", [Subscriptions]),
            From ! { ok , ?MODULE },
            loop(Subscriptions);
        {{encode_subs, User}, From} -> 
            Subs = priv_encode_subs(Subscriptions, User),
            From ! { {ok, Subs}, ?MODULE },
            loop(Subscriptions)
    end.

priv_encode_subs(Subscriptions, User) ->
    {ok, UserMap} = maps:find(User, Subscriptions),
    EncodeIn = fun(_,Topics) -> #{topics => Topics} end,
    EncodeOut = fun(_,LocTopics) -> #{locmap => maps:map(EncodeIn,LocTopics)} end,
    Subs = maps:map(EncodeOut,UserMap),
    #{subs => Subs}.

% Delete any entry that has no topics.
cleanup(Subscriptions) ->
    NotNull = fun (L) -> L /= [] end,
    HasTopic = fun (_,LocTopic) -> lists:any(NotNull, maps:values(LocTopic)) end,
    Apply = fun (_,Subs) -> maps:filter(HasTopic, Subs) end,
    maps:map(Apply, Subscriptions).