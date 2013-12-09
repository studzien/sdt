-module(sdt_manager).

-behaviour(gen_server).

%% API
-export([start_link/1,
         register/1,
         unregister/0,
         get_jid/0,
         report/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {clients = dict:new(),
                interval = 15000,
                times = []}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Interval) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Interval], []).

register(Username) ->
    gen_server:call(?MODULE, {register, self(), Username}).

unregister() ->
    gen_server:cast(?MODULE, {unregister, self()}).

get_jid() ->
    gen_server:call(?MODULE, {get_jid, self()}).

report(Time) ->
    gen_server:cast(?MODULE, {report, Time}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Interval]) ->
    random:seed(os:timestamp()),
    erlang:send_after(Interval, self(), flush),
    {ok, #state{interval=Interval}}.

handle_call({register, Pid, Username}, _From, #state{clients=Clients}=State) ->
    Jid = <<Username/binary, "@localhost">>,
    NewClients = dict:store(Pid, Jid, Clients),
    {reply, ok, State#state{clients=NewClients}};
handle_call({get_jid, Pid}, _From, #state{clients=Clients}=State) ->
    Clients1 = dict:erase(Pid, Clients),
    ClientsList = dict:to_list(Clients1),
    N = random:uniform(dict:size(Clients1)),
    {_, Jid} = lists:nth(N, ClientsList),
    {reply, Jid, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({unregister, Pid}, #state{clients=Clients}=State) ->
    NewClients = dict:erase(Pid, Clients),
    {noreply, State#state{clients=NewClients}};
handle_cast({report, Time}, #state{times=Times}=State) ->
    NewTimes = [{os:timestamp(), Time}|Times],
    {noreply, State#state{times=NewTimes}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(flush, #state{interval=Interval, times=Times}=State) ->
    erlang:send_after(Interval, self(), flush),
    try
        flush_graphite(Times),
        {noreply, State#state{times=[]}}
    catch _:_ ->
        {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
flush_graphite(Times) ->
    Host = string:strip(os:cmd("hostname"), both, $\n),
    NS = "sdt." ++ Host ++ ".delivery_time",
    Output = [graphite_metric(NS, Value, Timestamp) || {Timestamp, Value} <- Times],
    {ok, Socket} = gen_tcp:connect("10.100.0.70", 2003, []),
    gen_tcp:send(Socket, Output),
    gen_tcp:close(Socket).

graphite_metric(Namespace, Value, {Mega,Seconds,_}) ->
    Timestamp = 1000000*Mega+Seconds,
    io_lib:format("~s ~p ~p~n", [Namespace, Value, Timestamp]).
