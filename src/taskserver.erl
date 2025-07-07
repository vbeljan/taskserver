-module(taskserver).

-define(SCRIPT_HEADER, <<"#!/usr/bin/env bash">>).

-export([order/1,
         transform_to_script/1]).

-spec transform_to_script(map() | {error, _}) -> binary().
transform_to_script({error, _} = Error) ->
    Error;
transform_to_script(#{<<"tasks">> := Tasks}) ->
    lists:foldl(fun(#{<<"command">> := Command}, Acc) ->
                        <<Acc/binary, ";",  Command/binary>>
                end, ?SCRIPT_HEADER, Tasks).


-spec order(map()) -> map().
order(#{<<"tasks">> := Tasks}) ->
    {OrderedTasks, _} = order_tasks(Tasks, [], sets:new(), sets:new(), Tasks),
    #{<<"tasks">> => remove_dep_data(OrderedTasks)}.

order_tasks([], Acc, Processed, _, _) ->
    {Acc, Processed};
order_tasks([Task | TasksTail], Acc, Processed, DepthStack, FullTasksList) ->
    circularity_check(sets:is_element(Task, DepthStack)),
    {AccWithDeps, ProcessedWithDeps} = order_deps(Task, Acc, Processed,
                                                  sets:add_element(Task, DepthStack),
                                                  FullTasksList),

    NewAccWithDeps =
    case (sets:is_element(Task, ProcessedWithDeps)) of
        true -> AccWithDeps;
        false -> [Task | AccWithDeps]
    end,
    order_tasks(TasksTail, NewAccWithDeps, sets:add_element(Task, ProcessedWithDeps),
                sets:new(), FullTasksList).

order_deps(#{<<"requires">> := Deps}, Acc, Processed, DepthStack, FullTasksList) ->
    FullDepsList = lists:foldl(fun(N, LocalAcc) ->
                                       Task = get_task(N, FullTasksList),
                                       case {Task, sets:is_element(Task, Processed)} of
                                           {undefined, _} -> LocalAcc;
                                           {_, true} -> LocalAcc;
                                           {_, false} -> [Task | LocalAcc]
                                       end
                               end,
                               [], Deps),
    order_tasks(FullDepsList, Acc, Processed, DepthStack, FullTasksList);
order_deps(_, Acc, Processed, _, _) ->
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

circularity_check(true) ->
    throw({error, circular_dependency});
circularity_check(false) ->
    ok.
