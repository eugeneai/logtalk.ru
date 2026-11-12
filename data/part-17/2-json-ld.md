---
path: '/part-17/2-json-ld'
title: 'JSON-LD'
hidden: false
---

<text-box variant='learningObjectives' name="Цель изучения раздела">

После изучения этого раздела вы сможете

- Разбирать JSON-LD-документ
- Раскрывать контекст в полные IRI
- Генерировать JSON-LD из термов
- Понимать, зачем нужны `@context`, `@id` и `@type`

</text-box>

**JSON-LD** — это JSON-представление связанных данных. Оно добавляет к обычному JSON контекст, который превращает короткие имена свойств в полные IRI.

## Подключение

```logtalk
?- logtalk_load(json_ld(loader)).
```

Библиотека `json_ld` использует те же представления JSON, что и `json`: объекты в фигурных скобках, пары через дефис, строки-атомы.

## Разбор

```logtalk
:- object(linked_data).
    :- public(parse_person/2).

    parse_person(JSON, Document) :-
        json_ld::parse(atom(JSON), Document).

:- end_object.
```

```logtalk
?- linked_data::parse_person(
       '{"@context":{"name":"http://schema.org/name"},
         "name":"Ada"}',
       Document).
Document = {'@context'-{name-'http://schema.org/name'}, name-ada}.
```

## Раскрытие контекста

`expand/2` заменяет короткие имена полными IRI:

```logtalk
:- object(linked_data).
    :- public(expand_person/2).

    expand_person(Name, Expanded) :-
        json_ld::expand(
            {'@context'-{name-'http://schema.org/name'}, name-Name},
            Expanded
        ).

:- end_object.
```

```logtalk
?- linked_data::expand_person(ada, Expanded).
Expanded = [{'http://schema.org/name'-[{'@value'-ada}]}].
```

После раскрытия документ не зависит от контекста. С ним проще сравнивать данные из разных источников.

## Генерация

```logtalk
?- json_ld::generate(atom(JSON),
       {'@context'-{name-'http://schema.org/name'},
        '@id'-'http://example.org/ada',
        name-ada}).
```

На выходе получится JSON-LD-документ с контекстом и идентификатором.

## Основные ключи

| Ключ | Значение |
|---|---|
| `@context` | словарь префиксов и коротких имён |
| `@id` | IRI узла |
| `@type` | тип ресурса |
| `@value` | литеральное значение |
| `@language` | язык литерала |

## Зачем это нужно

JSON-LD позволяет публиковать данные в машиночитаемом виде, не изобретая собственный формат. Один и тот же документ можно:

- разобрать как обычный JSON;
- раскрыть в RDF-подобные тройки;
- сжать обратно с нужным контекстом;
- объединить с данными из другого источника.

## Что важно запомнить

- JSON-LD — это JSON с контекстом.
- `@context` задаёт соответствие коротких имён и IRI.
- `expand/2` убирает контекст и оставляет полные IRI.
- `generate/2` создаёт JSON-LD из термов Logtalk.
- JSON-LD удобен для обмена связанными данными.
