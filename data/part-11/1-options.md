---
path: '/part-11/1-options'
title: 'Опции и настройки'
hidden: false
---

<text-box variant='learningObjectives' name="Цель изучения раздела">

После изучения этого раздела вы сможете

- Описывать параметры программы в виде списка опций
- Задавать значения по умолчанию через `default_option/1`
- Проверять допустимые значения через `valid_option/1`
- Объединять пользовательские опции со значениями по умолчанию

</text-box>

Прикладные программы и инструменты часто принимают **опции** — параметры, которые можно не указывать. Например, сервер может иметь настройки `host`, `port` и `workers`. Если пользователь не задал значение, программа должна взять значение по умолчанию.

В Logtalk для этого есть библиотека `options`. Она оформлена как категория, которую можно импортировать в свой объект.

## Подключение библиотеки

```logtalk
:- object(server_config,
    imports(options)).

    default_option(host(localhost)).
    default_option(port(8080)).

:- end_object.
```

Категория `options` добавляет в объект предикаты `check_options/1`, `merge_options/2`, `option/2` и `option/3`. Имена `default_option/1` и `valid_option/1` — это **хуки**: библиотека вызывает их, а вы определяете их в своём объекте.

## Значения по умолчанию

```logtalk
:- object(server_config,
    imports(options)).

    :- public(configure/3).

    configure(UserOptions, Host, Port) :-
        ^^check_options(UserOptions),
        ^^merge_options(UserOptions, Options),
        ^^option(host(Host), Options),
        ^^option(port(Port), Options).

    default_option(host(localhost)).
    default_option(port(8080)).

    valid_option(host(Host)) :-
        atom(Host).
    valid_option(port(Port)) :-
        integer(Port),
        Port > 0,
        Port < 65536.

:- end_object.
```

Вызов:

```logtalk
?- server_config::configure([], Host, Port).
Host = localhost,
Port = 8080.

?- server_config::configure([port(9000)], Host, Port).
Host = localhost,
Port = 9000.
```

Пользовательская опция имеет приоритет над значением по умолчанию. Если одна и та же опция указана несколько раз, `option/2` и `option/3` берут **первое** вхождение слева.

## Проверка опций

`check_options/1` проверяет, что:

- аргумент является списком;
- каждый элемент — составной терм;
- для каждой опции определено `valid_option/1`;
- значение опции удовлетворяет этому предикату.

Если проверка не проходит, выбрасывается исключение, например `domain_error(option, port(70000))`. Поэтому ошибочную конфигурацию лучше обнаружить до запуска программы.

```logtalk
?- server_config::configure([port(70000)], _, _).
ERROR: Domain error: `port(70000)` is not in domain `option`
```

## Опции с необязательным значением

Иногда значение нужно не объединять со значением по умолчанию, а просто проверить. Для этого удобен `option/3` с третьим аргументом:

```logtalk
:- public(timeout/2).
timeout(Options, Timeout) :-
    ^^option(timeout(Timeout), Options, 5).
```

Если `timeout/1` нет в списке, получим `Timeout = 5`; если есть — будет использовано указанное значение.

## Что важно запомнить

- `options` — категория, её подключают через `imports(options)`.
- `default_option/1` задаёт значение по умолчанию, `valid_option/1` — допустимость.
- `^^check_options/1` проверяет список, `^^merge_options/2` добавляет значения по умолчанию.
- `^^option/3` удобен, когда нужен запасной вариант без изменения списка.
- Опции — это обычные термы Logtalk, поэтому их можно проверять и преобразовывать как любые другие данные.
