-module(taskserver).

-define(SCRIPT_HEADER, <<"#!/usr/bin/env bash">>).

-export([order/1,
         transform_to_script/1]).

-spec transform_to_script(map()) -> binary().
transform_to_script(#{<<"tasks">> := Tasks}) ->
    lists:foldl(fun(#{<<"command">> := Command}, Acc) ->
                        <<Acc/binary, ";",  Command/binary>>
                end, ?SCRIPT_HEADER, Tasks).


-spec order(map()) -> map().
order(#{<<"tasks">> := Tasks}) ->
    {OrderedTasks, _} = order_tasks(Tasks, [], sets:new(), Tasks),
    #{<<"tasks">> => remove_dep_data(OrderedTasks)}.

order_tasks([], Acc, Processed, _) ->
    {Acc, Processed};
order_tasks([Task | TasksTail], Acc, Processed, FullTasksList) ->
    {AccWithDeps, ProcessedWithDeps} = order_deps(Task, Acc, Processed, FullTasksList),

    case sets:is_element(Task, ProcessedWithDeps) of
        true -> order_tasks(TasksTail, AccWithDeps, ProcessedWithDeps, FullTasksList);
        false -> order_tasks(TasksTail, [Task | AccWithDeps],
                             sets:add_element(Task, ProcessedWithDeps), TasksTail)
    end.

order_deps(#{<<"requires">> := Deps}, Acc, Processed, FullTasksList) ->
    FullDepsList = lists:foldl(fun(N, LocalAcc) ->
                                       Task = get_task(N, FullTasksList),
                                       case {Task, sets:is_element(Task, Processed)} of
                                           {undefined, _} -> LocalAcc;
                                           {_, true} -> LocalAcc;
                                           {_, false} -> [Task | LocalAcc]
                                       end
                               end,
                               [], Deps),
    order_tasks(FullDepsList, Acc, Processed, FullTasksList);
order_deps(_, Acc, Processed, _) ->
    {Acc, Processed}.

get_task(Name, TaskList) ->
    MatchingUnvisitedTasks =
        lists:filter(fun(#{<<"name">> := N}) ->
                             N =:= Name
                     end,
                     TaskList),
    case MatchingUnvisitedTasks of
        [] -> undefined;
        [Task] -> Task
    end.

remove_dep_data(TaskList) ->
    lists:foldl(fun(Task, Acc) ->
                        [maps:remove(<<"requires">>, Task) | Acc]
                end, [], TaskList).
