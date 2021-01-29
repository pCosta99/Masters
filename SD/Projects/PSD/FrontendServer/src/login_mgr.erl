-module(login_mgr).
-export([createAccount/3, closeAccount/2, login/2, logout/1, online/0, start/0, all_users/0, finish/0, isSick/1, flipSick/2, get_district/1]).

%interface functions

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

isSick(User) -> rpc({is_sick, User}).

flipSick(User, DistrictPid) -> rpc({flip_sick, User, DistrictPid}).

createAccount(User, Pass, DistrictID) -> rpc({create_account, User, Pass, DistrictID}).

closeAccount(User, Pass) -> rpc({close_account, User, Pass}).

login(User, Pass) -> rpc({login, User, Pass}).

logout(User) -> rpc({logout, User}).

online() -> rpc({online}).

all_users() -> rpc({all_users}).

get_district(User) -> rpc({get_district, User}).

%server loop
% Accounts keeps info about user's personal data and their sick state
loop(Accounts) ->
    receive
        {{is_sick, User}, From} ->
            {_, {_, _, IsSick, _}} = maps:find(User, Accounts),
            From ! { IsSick, ?MODULE },
            loop(Accounts);
        {{flip_sick, User, DistrictPid}, From} ->
            From ! { ok, ?MODULE },
            {_, {Pass, IsLogged, IsSick, DistrictID}} = maps:find(User, Accounts),
            case IsSick of
                true -> loop(maps:put(User, {Pass, IsLogged, false, DistrictID}, Accounts));
                false -> 
                    DistrictPid ! {infected, User}, % notify district server about being infected 
                    loop(maps:put(User, {Pass, IsLogged, true, DistrictID}, Accounts))
            end;
        {{create_account, User, Pass, DistrictID}, From} ->
            case maps:find(User, Accounts) of
                error ->
                    From ! { ok, ?MODULE },
                    loop(maps:put(User, {Pass, false, false, DistrictID}, Accounts));
                _ ->
                    From ! { user_exists, ?MODULE },
                    loop(Accounts)
            end;
        {{close_account, User, Pass}, From} ->
            case maps:find(User, Accounts) of
                {ok, {Pass, _, _, _, _}} ->
                    From ! { ok, ?MODULE },
                    loop(maps:remove(User,Accounts));
                _ ->
                    From ! {invalid, ?MODULE},
                    loop(Accounts)
            end;
        {{login, User, Pass}, From} ->
            case maps:find(User, Accounts) of        
                {ok, {OtherPass, IsLogged, IsSick, DistrictID}} ->
                    case OtherPass == Pass of
                        true -> 
                            case IsLogged of
                                true -> 
                                    %From ! { user_already_logged, ?MODULE },
                                    From ! { ok, ?MODULE }, % This allows multiple logins, useful during testing phase
                                    loop(Accounts); 
                                false -> 
                                    From ! { ok, ?MODULE },
                                    loop(maps:update(User, {Pass, true, IsSick, DistrictID}, Accounts))
                            end;
                        false -> 
                            From ! {invalid_password, ?MODULE },
                            loop(Accounts)
                    end;
                _ ->
                    From ! { invalid_username, ?MODULE },
                    loop(Accounts)
            end;
        {{logout, User}, From} ->
            case maps:find(User, Accounts) of
                {ok, {_, false, _, _}} ->
                    From ! { user_is_offline, ?MODULE },
                    loop(Accounts);
                {ok, {Pass, true, IsSick, DistrictID}} ->
                    From ! { ok, ?MODULE },
                    district_rcv:newLogout(User),
                    loop(maps:update(User, {Pass, false, IsSick, DistrictID}, Accounts));
                _ ->
                    From ! { invalid_credentials , ?MODULE },
                    loop(Accounts)
            end;
        {{all_users}, From} ->
            From ! { ok , ?MODULE },
            io:format("~p~n", [Accounts]),
            loop(Accounts);
        {{online}, From} ->
            Pred = fun(_,V) -> element(2,V) end,
            From ! { lists:map(fun (V) -> binary_to_list(V) end,maps:keys(maps:filter(Pred,Accounts))) , ?MODULE },
            io:format("~p~n", [lists:map(fun (V) -> binary_to_list(V) end,maps:keys(maps:filter(Pred,Accounts)))]),
            loop(Accounts);
        {{get_district, User}, From} -> 
            {_, {_, _, _, DistrictID}} = maps:find(User, Accounts),
            From ! { DistrictID, ?MODULE },
            loop(Accounts)
    end.