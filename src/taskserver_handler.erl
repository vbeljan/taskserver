-module(taskserver_handler).

-export([init/2]).

init(Req0, State) ->
    {ok, Body, _} = cowboy_req:read_body(Req0),
    ParsedBody = jsx:decode(Body),
    Result = taskserver:order(ParsedBody),
    EncodedResult = jsx:encode(Result),
    reply(200, EncodedResult, Req0, State).

reply(Status, Body, Req0, State) ->
    Req = cowboy_req:reply(Status, #{
        <<"content-type">> => <<"application/json">>
    }, Body, Req0),
    {ok, Req, State}.
