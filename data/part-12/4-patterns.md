---
path: '/part-12/4-patterns'
title: 'Практические схемы'
hidden: false
---

<text-box variant='learningObjectives' name="Цель изучения раздела">

После изучения этого раздела вы сможете

- Применять схему «разделяй и властвуй» с `threaded/1`
- Организовывать производителя и потребителя
- Ограничивать время выполнения цели
- Выбирать между синхронным и асинхронным запуском

</text-box>

Рассмотрим несколько типовых схем, которые встречаются при работе с потоками.

## Разделяй и властвуй

Если задача делится на независимые части, каждую часть можно считать в отдельном потоке:

```logtalk
:- object(parallel_sum).
    :- threaded.
    :- use_module(library(lists), [append/3, sum_list/2]).

    :- public(sum/2).
    sum(List, Sum) :-
        length(List, N),
        ( N < 2 ->
            sum_list(List, Sum)
        ;   split(List, Left, Right),
            threaded((
                sum(Left, SumLeft),
                sum(Right, SumRight)
            )),
            Sum is SumLeft + SumRight
        ).

    split(List, Left, Right) :-
        length(List, N),
        Half is N // 2,
        length(Left, Half),
        append(Left, Right, List).

:- end_object.
```

В реальном коде стоит остановить рекурсию на достаточно крупных частях: создание потока для одного элемента не окупается.

## Производитель и потребитель

Один поток создаёт элементы, другой обрабатывает:

```logtalk
:- object(pipeline).
    :- threaded.

    :- public(run/1).
    run(Items) :-
        threaded((
            produce(Items),
            consume
        )).

    produce([]).
    produce([Item|Items]) :-
        threaded_notify(item(Item)),
        produce(Items).

    consume :-
        ( threaded_peek(item(_)) ->
            threaded_wait(item(Item)),
            process(Item),
            consume
        ;   true
        ).

    process(Item) :-
        write(processed(Item)), nl.

:- end_object.
```

Для настоящей очереди с ограничением размера используйте отдельный объект-буфер и `synchronized/1`.

## Ограничение времени

Библиотека `timeout` позволяет ограничить время выполнения цели:

```logtalk
:- object(timeout_demo).
    :- public(run/1).

    run(Result) :-
        timeout::call_with_timeout(
            (thread_sleep(0.2), true),
            0.05,
            Result
        ).

:- end_object.
```

```logtalk
?- logtalk_load(timeout(loader)),
   timeout_demo::run(Result).
Result = timeout.
```

Третий аргумент `call_with_timeout/3` принимает значения `true`, `fail`, `timeout` или `error(Error)`. Это удобно для сетевых запросов и внешних инструментов, которые могут зависнуть.

## Синхронно или асинхронно

| Схема | Когда использовать |
|---|---|
| `threaded/1` | нужно дождаться всех целей и получить результаты |
| `threaded_call/1` + `threaded_exit/1` | результат нужен позже, можно работать параллельно |
| `threaded_once/1` | достаточно первого решения |
| `threaded_ignore/1` | результат не нужен, важна только побочная работа |
| `synchronized/1` | несколько потоков обращаются к общим данным |

## Ошибки в потоках

Исключение в потоке не исчезает. При `threaded/1` оно передаётся вызывающему коду. При `threaded_call/1` его нужно забрать через `threaded_exit/1`. При `threaded_ignore/1` ошибку нужно обработать внутри потока, иначе она может остаться незамеченной.

## Что важно запомнить

- Параллельность окупается на достаточно крупных независимых задачах.
- Для обмена элементами используйте уведомления или защищённый буфер.
- `timeout` ограничивает время выполнения цели.
- Выбирайте схему запуска по тому, нужен ли результат и когда.
- Ошибки потоков нужно явно обрабатывать.
