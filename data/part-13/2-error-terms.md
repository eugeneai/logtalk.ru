---
path: '/part-13/2-error-terms'
title: 'Стандартные ошибки'
hidden: false
---

<text-box variant='learningObjectives' name="Цель изучения раздела">

После изучения этого раздела вы сможете

- Использовать стандартные классы ошибок
- Правильно выбирать `type_error`, `domain_error` и `existence_error`
- Передавать контекст ошибки
- Проверять аргументы до выполнения опасной операции

</text-box>

Стандартная ошибка в Prolog и Logtalk имеет вид:

```logtalk
error(Formal, Context)
```

`Formal` — описание нарушения, `Context` — предикат или объект, где оно произошло. Стандартные классы ошибок помогают единообразно обрабатывать сбои.

## Основные классы

| Ошибка | Когда возникает |
|---|---|
| `instantiation_error` | аргумент должен быть связан, но является переменной |
| `type_error(Type, Term)` | терм имеет неверный тип |
| `domain_error(Domain, Term)` | значение вне допустимой области |
| `existence_error(ObjectType, Object)` | объект не существует |
| `permission_error(Action, Type, Object)` | операция запрещена |
| `representation_error(Flag)` | значение не представимо |
| `evaluation_error(Kind)` | ошибка вычисления |

Примеры:

```logtalk
throw(error(instantiation_error, check/1)).
throw(error(type_error(integer, X), check/1)).
throw(error(domain_error(port, 70000), configure/1)).
throw(error(existence_error(source_sink, File), open_file/1)).
```

## Проверка аргументов

Проверяйте аргументы в начале предиката, до побочных эффектов:

```logtalk
:- object(port_config).
    :- public(set_port/1).

    set_port(Port) :-
        ( var(Port) ->
            throw(error(instantiation_error, set_port/1))
        ; \+ integer(Port) ->
            throw(error(type_error(integer, Port), set_port/1))
        ; Port < 1 ; Port > 65535 ->
            throw(error(domain_error(port, Port), set_port/1))
        ; true
        ),
        assertz(port_(Port)).

    :- private(port_/1).
    :- dynamic(port_/1).

:- end_object.
```

Такой предикат не изменит базу данных, если аргумент некорректен.

## Контекст

Второй аргумент `error/2` полезен для диагностики. Можно передавать:

- индикатор предиката: `set_port/1`;
- имя объекта: `port_config`;
- составной контекст: `context(port_config, set_port/1)`.

Главное — чтобы контекст помогал найти место ошибки.

## Ошибки и `fail`

Не подменяйте ошибку неудачей, если причина сбоя известна. `fail` означает «решения нет», а исключение — «вызов некорректен» или «среда сломалась». Например, отсутствие файла — это `existence_error`, а не просто `fail`.

## Что важно запомнить

- Стандартная ошибка — `error(Formal, Context)`.
- `instantiation_error` — не связан обязательный аргумент.
- `type_error` — неверный тип, `domain_error` — неверное значение.
- `existence_error` — отсутствует объект или ресурс.
- Проверяйте аргументы до побочных эффектов.
- Не заменяйте содержательную ошибку обычным `fail`.
