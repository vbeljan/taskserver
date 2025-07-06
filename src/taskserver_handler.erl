-module(taskserver_handler).

-export([init/2]).

init(Req0, State) ->
    Path = cowboy_req:path(Req0),
    {ok, Body, _} = cowboy_req:read_body(Req0),
    ParsedBody = jsx:decode(Body),
    Result = taskserver:order(ParsedBody),
    case Path of
        <<"/">> ->
            EncodedResult = jsx:encode(Result),
            reply(200, EncodedResult, Req0, State);
        <<"/script">> ->
            Script = taskserver:transform_to_script(Result),
            EncodedResult = jsx:encode(#{<<"script">> => Script}),
            reply(200, EncodedResult, Req0, State)
    end.

reply(Status, Body, Req0, State) ->
    Req = cowboy_req:reply(Status, #{
        <<"content-type">> => <<"application/json">>
    }, Body, Req0),
    {ok, Req, State}.
