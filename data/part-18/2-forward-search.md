---
path: '/part-18/2-forward-search'
title: 'Прямой поиск плана'
hidden: false
---

<text-box variant='learningObjectives' name="Цель изучения раздела">

После изучения этого раздела вы сможете

- Реализовать поиск в ширину по состояниям
- Хранить посещённые состояния
- Собирать план из действий
- Избегать циклов в графе состояний

</text-box>

Простейший планировщик перебирает состояния, начиная с начального, и применяет все возможные действия. Чтобы не зациклиться, посещённые состояния запоминают.

## Планировщик

```logtalk
:- object(planner).
    :- use_module(library(lists),
        [member/2, append/3, reverse/2, subtract/3]).

    :- public(plan/3).

    plan(Start, Goal, Plan) :-
        search([node(Start, [])], Goal, [Start], Plan).

    search([node(State, RevPlan)| _], Goal, _, Plan) :-
        satisfies(Goal, State), !,
        reverse(RevPlan, Plan).
    search([node(State, RevPlan)| Rest], Goal, Visited, Plan) :-
        action(Action, Pre, Add, Del),
        satisfies(Pre, State),
        apply(Add, Del, State, Next),
        \+ member(Next, Visited),
        append(Rest, [node(Next, [Action|RevPlan])], Queue),
        search(Queue, Goal, [Next|Visited], Plan).

    satisfies([], _).
    satisfies([Condition|Conditions], State) :-
        member(Condition, State),
        satisfies(Conditions, State).

    apply(Add, Del, State, Next) :-
        subtract(State, Del, State1),
        append(Add, State1, Next).

    action(move(a, b), [at(a)], [at(b)], [at(a)]).
    action(move(b, c), [at(b)], [at(c)], [at(b)]).

:- end_object.
```

```logtalk
?- planner::plan([at(a)], [at(c)], Plan).
Plan = [move(a, b), move(b, c)].
```

## Очередь поиска

`search/4` получает список узлов `node(State, RevPlan)`. Это очередь поиска в ширину:

- новый узел добавляется в конец (`append(Rest, [Node], Queue)`);
- первым обрабатывается самый старый узел.

Такой поиск находит план с минимальным числом действий, если все действия имеют одинаковую стоимость.

## Посещённые состояния

Список `Visited` хранит состояния, которые уже были в очереди. Проверка

```logtalk
\+ member(Next, Visited)
```

не даёт повторно добавить то же состояние. Без неё поиск может зациклиться.

Для больших задач список посещённых состояний становится узким местом. Тогда используют хеш-таблицу или множество.

## Восстановление плана

Узел хранит план в обратном порядке: `[Action|RevPlan]`. Когда цель достигнута, план переворачивается:

```logtalk
reverse(RevPlan, Plan)
```

Это дешевле, чем добавлять действие в конец списка на каждом шаге.

## Что важно запомнить

- Поиск в ширину обрабатывает состояния слоями.
- Посещённые состояния предотвращают циклы.
- План удобно накапливать в обратном порядке и переворачивать в конце.
- При одинаковой стоимости действий BFS даёт кратчайший план.
- Для больших пространств нужны более эффективные структуры данных.
