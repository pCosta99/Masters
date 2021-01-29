-module(login_mgr).
-export([createAccount/2, closeAccount/2, login/2, logout/1, online/0, start/0, all_users/0, finish/0]).

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

createAccount(User, Pass) -> rpc({create_account, User, Pass}).

closeAccount(User, Pass) -> rpc({close_account, User, Pass}).

login(User, Pass) -> rpc({login, User, Pass}).

logout(User) -> rpc({logout, User}).

online() -> rpc({online}).

all_users() -> rpc({all_users}).

%server loop

loop(Accounts) ->
    receive
        {{create_account, User, Pass}, From} ->
            case maps:find(User, Accounts) of
                error ->
                    From ! { ok, ?MODULE },
                    loop(maps:put(User, {Pass, false}, Accounts));
                _ ->
                    From ! { user_exists, ?MODULE },
                    loop(Accounts)
            end;
        {{close_account, User, Pass}, From} ->
            case maps:find(User, Accounts) of
                {ok, {Pass, _}} ->
                    From ! { ok, ?MODULE },
                    loop(maps:remove(User,Accounts));
                _ ->
                    From ! {invalid, ?MODULE},
                    loop(Accounts)
            end;
        {{login, User, Pass}, From} ->
            case maps:find(User, Accounts) of
                {ok, {Pass, false}} ->
                    From ! { ok, ?MODULE },
                    loop(maps:update(User, {Pass, true}, Accounts));
                {ok, {Pass, true}} ->
                    From ! { user_already_logged, ?MODULE },
                    loop(Accounts);
                _ ->
                    From ! { invalid_credentials , ?MODULE },
                    loop(Accounts)
            end;
        {{logout, User}, From} ->
            case maps:find(User, Accounts) of
                {ok, {_, false}} ->
                    From ! { user_is_offline, ?MODULE },
                    loop(Accounts);
                {ok, {Pass, true}} ->
                    From ! { ok, ?MODULE },
                    loop(maps:update(User, {Pass, false}, Accounts));
                _ ->
                    From ! { invalid_credentials , ?MODULE },
                    loop(Accounts)
            end;
        {{all_users}, From} ->
            From ! { ok , ?MODULE },
            io:format("~p~n", [Accounts]),
            loop(Accounts);
        {{online}, From} ->
            From ! { ok , ?MODULE },
            Pred = fun(_,V) -> element(2,V) end,
            io:format("~p~n", [maps:keys(maps:filter(Pred,Accounts))]),
            loop(Accounts)
    end.
