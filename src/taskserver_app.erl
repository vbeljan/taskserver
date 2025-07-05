%%%-------------------------------------------------------------------
%% @doc taskserver public API
%% @end
%%%-------------------------------------------------------------------

-module(taskserver_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([{'_', [{'_', taskserver_handler, #{} }]}]),

    {ok, _} = cowboy:start_clear(http, [{port, 8080}],
                                 #{
                                   env => #{dispatch => Dispatch}
                                  }),
    taskserver_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
