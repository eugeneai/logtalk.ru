---
path: '/part-13/1-catch-throw'
title: 'catch/3 и throw/1'
hidden: false
---

<text-box variant='learningObjectives' name="Цель изучения раздела">

После изучения этого раздела вы сможете

- Перехватывать исключения через `catch/3`
- Создавать исключения через `throw/1`
- Отличать ожидаемую ошибку от программной
- Повторно выбрасывать исключение после частичной обработки

</text-box>

В Prolog и Logtalk ошибка — это не просто `fail`. Исключение прерывает обычный поиск решений и передаёт управление обработчику.

## Синтаксис

```logtalk
catch(Goal, Pattern, Handler)
```

- `Goal` — защищаемая цель;
- `Pattern` — образец исключения;
- `Handler` — цель, которая выполняется, если исключение унифицировалось с `Pattern`.

## Пример

```logtalk
:- object(safe_math).
    :- public(divide/3).

    divide(A, B, Quotient) :-
        catch(
            ( integer(B), B =\= 0 ->
                Quotient is A / B
            ;   throw(error(division_by_zero(B), divide/3))
            ),
            Error,
            handle(Error, Quotient)
        ).

    handle(error(division_by_zero(_), _), 0).

:- end_object.
```

```logtalk
?- safe_math::divide(10, 2, Q).
Q = 5.

?- safe_math::divide(10, 0, Q).
Q = 0.
```

В обработчике можно вернуть значение по умолчанию, записать журнал или преобразовать ошибку в другой терм.

## `throw/1`

`throw/1` принимает произвольный непустой терм. Хорошая практика — использовать стандартный вид `error(Formal, Context)`:

```logtalk
throw(error(domain_error(port, Port), configure/1))
```

`Formal` описывает, что именно не так, а `Context` — где произошла ошибка. Такой терм понимают стандартные инструменты и `catch/3`.

## Перехват конкретных ошибок

Не перехватывайте всё подряд без необходимости. Лучше указать ожидаемый класс ошибки:

```logtalk
open(File, read, Stream) :-
    catch(
        open(File, read, Stream0),
        error(existence_error(source_sink, File), _),
        ( write(missing_file(File)), nl, fail )
    ).
```

Если исключение не унифицировалось с `Pattern`, `catch/3` не перехватывает его и оно уходит выше.

## Повторное выбрасывание

Если обработчик не может исправить ситуацию, исключение нужно выбросить снова:

```logtalk
handle_logged(Error) :-
    write(log(Error)), nl,
    throw(Error).
```

Иначе ошибка потеряется, и вызывающий код получит неверный результат.

## Что важно запомнить

- `catch(Goal, Pattern, Handler)` перехватывает исключение из `Goal`.
- `throw/1` создаёт исключение.
- Стандартный вид ошибки — `error(Formal, Context)`.
- Перехватывайте только те ошибки, которые действительно умеете обрабатывать.
- Если обработка неполная, используйте `throw(Error)` повторно.
