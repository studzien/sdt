-module(sdt).

-export([start_clients/2]).

start_clients(Server, Clients) ->
    [start_client(Server, Client) || Client <- Clients].

start_client(Server, {Username, Password}) ->
    supervisor:start_child(sdt_client_sup, [Server, Username, Password, 10000]).
