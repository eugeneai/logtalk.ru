---
path: '/part-8/6-more-examples-with-classes'
title: 'Языки конфигурации'
hidden: false
---

<text-box variant='learningObjectives' name="Цель изучения раздела">

После изучения этого раздела вы сможете

- Описывать конфигурацию компонентов данными
- Собирать цепочку адаптеров по описанию
- Отделять конфигурацию от кода компонентов
- Использовать загрузчики и флаги для настройки проекта

</text-box>

В предыдущих разделах цепочка адаптеров собиралась вручную:

```logtalk
uppercase_adapter(prefix_adapter('>> ', plain_text(hello)))
```

Для большой системы удобнее хранить конфигурацию отдельно от кода. Тогда состав цепочки можно менять без правки программы.

## Конфигурация как данные

```logtalk
:- object(config).

    :- public(adapters/1).
    adapters([uppercase, prefix('>> ')]).

:- end_object.
```

`config::adapters/1` возвращает список описаний адаптеров. Это обычные термы, которые ничего не знают о реализации.

## Сборщик цепочки

```logtalk
:- object(assembler).

    :- public(build/2).
    build(Source, Component) :-
        config::adapters(Adapters),
        build_chain(Adapters, Source, Component).

    :- private(build_chain/3).
    build_chain([], Component, Component).
    build_chain([uppercase|Rest], Source, Component) :-
        build_chain(Rest, uppercase_adapter(Source), Component).
    build_chain([prefix(Prefix)|Rest], Source, Component) :-
        build_chain(Rest, prefix_adapter(Prefix, Source), Component).

:- end_object.
```

Использование:

```logtalk
?- assembler::build(plain_text(hello), Component),
   Component::text(Text).
Component = uppercase_adapter(prefix_adapter('>> ', plain_text(hello))),
Text = '>> HELLO'.
```

Сборщик знает, как превратить описание в объект. Компоненты не знают о конфигурации.

## Загрузчик как язык конфигурации

Файл `loader.lgt` тоже является формой конфигурации:

```logtalk
:- initialization((
    set_logtalk_flag(report, warnings),
    logtalk_load([
        'plain_text',
        'uppercase_adapter',
        'prefix_adapter',
        'assembler'
    ])
)).
```

Он задаёт, какие файлы загружать и с какими флагами. Для больших проектов загрузчик может подключать разные наборы компонентов в зависимости от режима:

```logtalk
:- initialization((
    (   current_logtalk_flag(debug, on)
    ->  logtalk_load(debug_components)
    ;   logtalk_load(release_components)
    )
)).
```

## Параметры и конфигурация

Параметрические объекты позволяют передавать настройки в компонент:

```logtalk
:- object(logger(_Level_)).

    :- public(log/1).
    log(Message) :-
        _Level_ = debug,
        write(Message), nl.

:- end_object.
```

Параметр `_Level_` — часть конфигурации. Один и тот же класс используется с разными параметрами:

```logtalk
?- logger(debug)::log(hello).
hello.

?- logger(info)::log(hello).
false.
```

## Термическая расширяемость

Logtalk поддерживает раскрытие термов и целей (`term_expansion/2`, `goal_expansion/2`). С их помощью можно строить более сложные языки конфигурации, но это тема части 10. В большинстве случаев достаточно:

- фактов конфигурации;
- загрузчиков;
- параметрических объектов;
- сборщиков цепочек.

## Что важно запомнить

- Конфигурация должна быть данными, а не разбросанными по коду условиями.
- Сборщик превращает описание в объекты.
- Загрузчик настраивает состав и режим проекта.
- Параметрические объекты удобны для небольших настроек.
- Макросы и раскрытие термов нужны только для сложных языков конфигурации.
