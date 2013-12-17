-module(control_service).
-behaviour(gen_server).
-compile(export_all).
%-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%-include_lib("/opt/otp/lib/erlang/lib/stdlib-1.17.4/include/qlc.hrl").
-include_lib("/usr/local/Cellar/erlang/R15B03-1//lib/erlang/lib/stdlib-1.18.3/include/qlc.hrl").
-record(authn_password, {authid, password}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
    % This traps exit messages, causing terminate/2 to be called when the application is stopped
    process_flag(trap_exit, true),
    io:format("~p starting~n", [?MODULE]),
    setup_authn_password_db(),
    {ok, 0}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call({list_attachable, Args}, _From, State) ->
    {reply, list_attachable(Args), State};
handle_call({attach, Args}, _From, State) ->
    {reply, attach(Args), State};
handle_call({detach, Args}, _From, State) ->
    {reply, detach(Args), State};
handle_call({spectate, Args}, _From, State) ->
    {reply, spectate(Args), State};
handle_call({authn, Credentials}, _From, State) ->
    {reply, authn(Credentials), State};
handle_call({authz, Args}, _From, State) ->
    {reply, authz(Args), State};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(request_ai_controller, From, State) ->
    {FromPid, _} = From,
    NewAI = spawn(fun() -> generic_ai:begin_generic_ai(FromPid) end),
    {reply, NewAI, State}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast(_, State) ->
    {noreply, State}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_info(_Info, State) ->
    {noreply, State}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
terminate(_Reason, _State) ->
    ok.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
list_attachable(_Credentials) ->
    not_yet_implemented.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
attach(_) ->
    not_yet_implemented.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
detach(_) ->
    not_yet_implemented.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
spectate(_) ->
    not_yet_implemented.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
setup_authn_password_db() ->
    mnesia:start(),
    % Test if the authn_password table exists
    TestQ = qlc:q([X || X <- mnesia:table(authn_password)]),
    F = fun() -> qlc:e(TestQ) end,
    {Status, _} = mnesia:transaction(F),
    case Status of
    aborted ->
        mnesia:create_table(authn_password, [{attributes, record_info(fields, authn_password)},
                         {disc_copies, [node()]}]),
        CreateF = fun() ->
        mnesia:write({authn_password, 'steve', 'killafoo'})
        end,
        mnesia:transaction(CreateF);
    atomic ->
        {atomic, ok}
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
authn(Credentials) ->
    {Method, AuthID, Secret} = Credentials,
    case Method of
    password ->
        authn_password_db(AuthID, Secret);
    _ ->
        {error, unknown_authn_method}
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
authn_password_db(AuthID, Secret) ->
    Q = qlc:q([ok || X <- mnesia:table(authn_password),
                      X#authn_password.authid == AuthID,
                      X#authn_password.password == Secret]),
    F = fun() -> qlc:e(Q) end,
    {atomic, ResultList} = mnesia:transaction(F),
    io:format("control_service:authn_password_db(): ResultList is ~p~n", [ResultList]),
    % FIXME results in an empty list every time?
    case ResultList of
    [] ->
        {error, "Authentication failure!"};
    _ ->
        {ok, ok}
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
authz(_) ->
    not_yet_implemented.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
