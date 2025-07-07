-module(taskserver_handler).

-export([init/2]).

init(Req0, State) ->
    Path = cowboy_req:path(Req0),
    {ok, Body, _} = cowboy_req:read_body(Req0),
    ParsedBody = jsx:decode(Body),
    Result = try taskserver:order(ParsedBody)
             catch
                 _:{error, circular_dependency}:_ ->
                     #{<<"error">> => <<"Circular dependency detected">>}
             end,
    case Path of
        <<"/">> ->
            EncodedResult = jsx:encode(Result),
            reply(get_status(Result), EncodedResult, Req0, State);
        <<"/script">> ->
            Script = taskserver:transform_to_script(Result),
            EncodedResult = jsx:encode(#{<<"script">> => Script}),
            reply(get_status(Result), EncodedResult, Req0, State)
    end.

reply(Status, Body, Req0, State) ->
    Req = cowboy_req:reply(Status, #{
        <<"content-type">> => <<"application/json">>
    }, Body, Req0),
    {ok, Req, State}.

get_status(#{<<"error">> := _Error}) ->
    500;
get_status(_) ->
    200.
