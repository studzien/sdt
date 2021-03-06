-module(sdt_client).

-behaviour(gen_server).

%% API
-export([start_link/5]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {conn, interval,props,backoff}).%backoff in ms 
-define(INITIAL_BACKOFF, 1000).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Server, Host, Username, Password, Interval) ->
    gen_server:start_link(?MODULE, [Server, Host, Username, Password, Interval], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Server, Host, Username, Password, Interval]) ->
    process_flag(trap_exit, true),
    random:seed(os:timestamp()),
    Props = [{server, Server},
 	     {host, Host},
             {username, Username},
             {password, Password},
             {resource, <<"sdt">>}],
    case escalus_connection:start(Props) of
        {ok, Conn, _Props} ->
            erlang:send_after(random:uniform(Interval), self(), send_message),
            send_initial_presence(Conn),
            sdt_manager:register(Username),
            {ok, #state{conn=Conn, interval=Interval,props=Props, backoff=?INITIAL_BACKOFF}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(reconnect, #state{backoff=Backoff,props=Props, interval=Interval} =State) ->
    try
        case escalus_connection:start(Props) of 
            {ok,Conn,_Props} ->
                send_initial_presence(Conn),
                erlang:send_after(Interval, self(), send_message),
                {noreply, State#state{conn=Conn,backoff=?INITIAL_BACKOFF}};
            {error,_ } ->
                handle_reconnect(Backoff,State)
        end
    catch _:_ ->
              handle_reconnect(Backoff,State)
    end; 
handle_info(send_message, #state{conn=Conn, interval=Interval}=State) ->
    handle_send_message(Conn, Interval),
    {noreply, State};
handle_info({stanza, Conn, Stanza}, #state{conn=Conn}=State) ->
    handle_stanza(Stanza),
    {noreply, State}; 
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    sdt_manager:unregister(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_reconnect(Backoff,State)->
    NBackoff = Backoff*1.5, 
    erlang:send_after(round(NBackoff), self(), reconnect),
    {noreply, State#state{backoff=NBackoff}}.

send_initial_presence(Conn) ->
    Presence = escalus_stanza:presence(<<"available">>),
    escalus_connection:send(Conn, Presence).

handle_send_message(Conn, Interval) ->
    Jid = sdt_manager:get_jid(),
    Time = timestamp_to_binary(os:timestamp()),
    Stanza = escalus_stanza:chat_to(Jid, Time),
    case escalus_connection:send(Conn, Stanza) of 
        ok-> 
            erlang:send_after(Interval, self(), send_message);
        {error,_}->
            self() ! reconnect
    end.

handle_stanza(Stanza) ->
    Now = os:timestamp(),
    Body = exml_query:path(Stanza, [{element, <<"body">>}, cdata]),
    try
        Then = binary_to_timestamp(Body),
        Diff = timer:now_diff(Now, Then),
        sdt_manager:report(Diff)
    catch _:_ ->
        ok
    end.

timestamp_to_binary({Me,S,Mi}) ->
    Time = Me*1000000000000+S*1000000+Mi,
    TimeL = integer_to_list(Time),
    list_to_binary(TimeL).

binary_to_timestamp(Binary) ->
    I1 = list_to_integer(binary_to_list(Binary)),
    Me = I1 div 1000000000000,
    I2 = I1 - Me*1000000000000,
    S = I2 div 1000000000000,
    Mi = I2 rem 1000000000000,
    {Me,S,Mi}.
