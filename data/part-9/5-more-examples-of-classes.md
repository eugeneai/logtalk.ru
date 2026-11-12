---
path: '/part-9/5-more-examples-of-classes'
title: 'Примеры перехвата сообщений'
hidden: false
---

<text-box variant='learningObjectives' name="Цель изучения раздела">

После изучения этого раздела вы сможете

- Строить журналирующий монитор
- Считать сообщения, отправленные объекту
- Ограничивать доступ к сообщениям
- Включать и выключать наблюдение во время работы

</text-box>

Рассмотрим несколько практических применений событий и перехвата сообщений.

## Журналирование

Монитор-трассировщик записывает события в динамический предикат:

```logtalk
:- object(tracer,
    implements(monitoring)).

    :- public(events/1).
    :- public(clear/0).

    :- private(event_/3).
    :- dynamic(event_/3).

    before(Object, Message, _Sender) :-
        assertz(event_(before, Object, Message)).

    after(Object, Message, _Sender) :-
        assertz(event_(after, Object, Message)).

    events(Events) :-
        findall(event(Type, Object, Message),
            event_(Type, Object, Message),
            Events).

    clear :-
        retractall(event_(_, _, _)).

:- end_object.
```

Использование:

```logtalk
?- set_logtalk_flag(events, allow),
   define_events(_, _, _, _, tracer),
   greeter::hello(world),
   tracer::events(Events).
Events = [
    event(before, greeter, hello(world)),
    event(after, greeter, hello(world))
].
```

## Счётчик сообщений

Монитор может считать только сообщения, отправленные конкретному объекту:

```logtalk
:- object(counter_monitor,
    implements(monitoring)).

    :- public(count/1).
    :- public(reset/0).

    :- private(count_/1).
    :- dynamic(count_/1).

    before(Object, _Message, _Sender) :-
        Object == greeter, !,
        (   count_(Count)
        ->  true
        ;   Count = 0
        ),
        Next is Count + 1,
        retractall(count_(_)),
        assertz(count_(Next)).
    before(_, _, _).

    after(_, _, _).

    count(Count) :-
        (   count_(Count)
        ->  true
        ;   Count = 0
        ).

    reset :-
        retractall(count_(_)).

:- end_object.
```

Такой монитор полезен для метрик: сколько раз вызывался сервис, сколько запросов обработал объект и т.п.

## Контроль доступа

Прокси может блокировать нежелательные сообщения:

```logtalk
:- object(read_only_proxy(_Source_),
    implements(forwarding)).

    forward(Message) :-
        allowed(Message),
        [_Source_::Message].

    allowed(read(_)).
    allowed(get(_)).

:- end_object.
```

Сообщения `write/1` или `delete/1` не пересылаются. Клиент получает неудачу, а источник защищён от изменения.

## Управление подпиской

Подписку удобно инкапсулировать:

```logtalk
:- object(event_control).

    :- public(watch/1).
    :- public(unwatch/1).

    watch(Monitor) :-
        define_events(_, _, _, _, Monitor).

    unwatch(Monitor) :-
        abolish_events(_, _, _, _, Monitor).

:- end_object.
```

Использование:

```logtalk
?- event_control::watch(tracer).
?- greeter::hello(world).
?- event_control::unwatch(tracer).
```

После `unwatch/1` новые события не записываются.

## Монитор и флаг events

Если события включены глобально, их проверка выполняется для всех сообщений. Для производительности можно отключать события у отдельных объектов:

```logtalk
:- object(hot_path).

    :- set_logtalk_flag(events, deny).

    ...

:- end_object.
```

Мониторы при этом продолжат работать для остальных объектов.

## Что важно запомнить

- Мониторы удобны для журналирования и метрик.
- Прокси подходит для контроля доступа и адаптации сообщений.
- Подписку лучше инкапсулировать в отдельном объекте.
- События можно включать и отключать на уровне объектов.
