---
path: '/part-9/1-the-category'
title: 'Категория монитора сообщений'
hidden: false
---

<text-box variant='learningObjectives' name="Цель изучения раздела">

После изучения этого раздела вы сможете

- Объяснять, что такое событие и монитор
- Подключать монитор к событиям сообщений
- Использовать протокол `monitoring`
- Выносить обработчики событий в категорию

</text-box>

До сих пор программа Logtalk выполнялась только из сообщений: объект получает сообщение и выполняет метод. Event-driven programming добавляет вторую возможность: программа может **наблюдать** за отправкой сообщений и реагировать на них.

## Событие

Событие — это факт отправки сообщения. Logtalk различает два события:

- `before` — сообщение отправлено, метод ещё не выполнен;
- `after` — метод успешно завершился.

Событие описывается парой «объект — сообщение» и отправителем. Например, отправка `greeter::hello(world)` порождает события для объекта `greeter` и сообщения `hello(world)`.

События генерируются только для сообщений, отправленных через `::/2`. Сообщения самому себе `::/1` и вызовы предка `^^/1` событий не порождают.

## Монитор

**Монитор** — это объект, который автоматически получает уведомления о событиях. Чтобы стать монитором, объект реализует встроенный протокол `monitoring` и определяет методы `before/3` и/или `after/3`:

```logtalk
:- object(tracer,
    implements(monitoring)).

    before(Object, Message, Sender) :-
        write('call: '), writeq(Object),
        write(' <-- '), writeq(Message),
        write(' from '), writeq(Sender), nl.

    after(Object, Message, Sender) :-
        write('exit: '), writeq(Object),
        write(' <-- '), writeq(Message),
        write(' from '), writeq(Sender), nl.

:- end_object.
```

Аргументы обработчиков:

- `Object` — объект, которому отправлено сообщение;
- `Message` — само сообщение;
- `Sender` — объект-отправитель.

## Подписка на события

Монитор подключается к событиям предикатом `define_events/5`:

```logtalk
?- set_logtalk_flag(events, allow).
true.

?- define_events(_, _, _, _, tracer).
true.
```

Пустые переменные означают «все события». После этого любое сообщение будет трассироваться:

```logtalk
?- greeter::hello(world).
call: greeter <-- hello(world) from user
Hello, world!
exit: greeter <-- hello(world) from user
true.
```

Отписаться можно так:

```logtalk
?- abolish_events(_, _, _, _, tracer).
true.
```

## Категория как переиспользуемый монитор

Обработчики событий можно вынести в категорию и импортировать в разные объекты:

```logtalk
:- category(message_monitor,
    implements(monitoring)).

    before(Object, Message, _Sender) :-
        format('before ~w: ~q~n', [Object, Message]).

    after(Object, Message, _Sender) :-
        format('after ~w: ~q~n', [Object, Message]).

:- end_category.

:- object(tracer,
    imports(message_monitor)).
:- end_object.
```

Категория описывает поведение монитора один раз, а объекты-мониторы получают его без дублирования кода. Это тот же приём композиции, который мы использовали для адаптеров в части 8.

## Флаг events

Чтобы события генерировались, у объекта-отправителя должен быть включён флаг `events(allow)`:

```logtalk
:- set_logtalk_flag(events, allow).
```

Если события не нужны, флаг `events(deny)` ускоряет отправку сообщений. Включайте события только там, где они действительно нужны.

## Что важно запомнить

- Событие — это отправка сообщения; бывают `before` и `after`.
- Монитор реализует `monitoring` и определяет `before/3`/`after/3`.
- `define_events/5` подписывает монитор, `abolish_events/5` отписывает.
- Обработчики можно вынести в категорию и переиспользовать.
