-module(taskserver).

-export([order/1]).

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
