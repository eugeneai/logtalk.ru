---
path: '/part-10/1-macros'
title: 'Макротрансформации'
hidden: false
---

<text-box variant='learningObjectives' name="Цель изучения раздела">

После изучения этого раздела вы сможете

- Объяснять, зачем нужны раскрытие термов и целей
- Определять `term_expansion/2` и `goal_expansion/2`
- Подключать hook-объект к исходному файлу
- Проверять раскрытие через `expand_term/2` и `expand_goal/2`

</text-box>

Logtalk поддерживает **раскрытие термов и целей** (term and goal expansion) — преобразование исходного кода до его компиляции. Это основа для макросов, предметно-ориентированных языков и расширений синтаксиса.

## Раскрытие термов

Hook-объект реализует встроенный протокол `expanding` и определяет `term_expansion/2`:

```logtalk
:- object(my_macros,
    implements(expanding)).

    term_expansion(ping, pong).

    term_expansion(colors,
        [white, yellow, blue, green, red, black]).

:- end_object.
```

Проверить раскрытие можно вручную:

```logtalk
?- my_macros::expand_term(ping, Term).
Term = pong.

?- my_macros::expand_term(colors, Colors).
Colors = [white, yellow, blue, green, red, black].
```

Если ни одно правило не подходит, возвращается исходный терм.

## Раскрытие целей

`goal_expansion/2` преобразует цели внутри предложений:

```logtalk
:- object(math_macros,
    implements(expanding)).

    goal_expansion(X is Expr, true) :-
        catch(X is Expr, _, fail).

:- end_object.
```

Такое правило пытается вычислить арифметическое выражение на этапе компиляции. Если выражение корректно, цель заменяется на `true`; иначе остаётся исходная цель.

```logtalk
?- math_macros::expand_goal(X is 3 + 2 * 5, Goal).
X = 13,
Goal = true.
```

## Подключение hook-объекта

Hook-объект должен быть загружен до компиляции файла. Подключить его можно флагом `hook`:

```logtalk
?- logtalk_load(my_macros).
?- logtalk_load(source_file, [hook(my_macros)]).
```

Или директивой в самом файле:

```logtalk
:- set_logtalk_flag(hook, my_macros).
```

Директива действует на файл, в котором она находится. Это позволяет разным файлам использовать разные наборы макросов.

## Порядок раскрытия

Компилятор сначала применяет `term_expansion/2`, затем `goal_expansion/2`. Раскрытие целей повторяется до достижения неподвижной точки, но защищено от бесконечных циклов.

## Когда использовать

- для предметно-ориентированных языков (DSL);
- для сокращённой записи часто используемых конструкций;
- для оптимизации на этапе компиляции;
- для совместимости со старым синтаксисом.

Не стоит использовать макросы там, где достаточно обычного предиката: макросы усложняют чтение и отладку.

## Что важно запомнить

- `term_expansion/2` раскрывает термы, `goal_expansion/2` — цели.
- Hook-объект реализует `expanding`.
- Подключение выполняется флагом `hook` или директивой `set_logtalk_flag/2`.
- Раскрытие можно проверить через `expand_term/2` и `expand_goal/2`.
