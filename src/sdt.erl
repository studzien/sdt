-module(sdt).

-export([start_clients/3]).

start_clients(Server, Host, Clients) ->
    [start_client(Server, Host, Client) || Client <- Clients].

start_client(Server, Host, {Username, Password}) ->
    supervisor:start_child(sdt_client_sup, [Server, Host, Username, Password, 10000]).
