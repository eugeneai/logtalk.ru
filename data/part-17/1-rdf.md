---
path: '/part-17/1-rdf'
title: 'RDF и тройки'
hidden: false
---

<text-box variant='learningObjectives' name="Цель изучения раздела">

После изучения этого раздела вы сможете

- Представлять факты в виде RDF-троек
- Хранить граф в объекте Logtalk
- Делать простые запросы к графу
- Понимать разницу между IRI, литералом и пустым узлом

</text-box>

**RDF** (Resource Description Framework) представляет данные в виде троек:

```text
субъект — предикат — объект
```

Например:

```text
http://example.org/ada  http://schema.org/name  "Ada Lovelace"
```

В Logtalk тройку удобно представлять термом `rdf/3`:

```logtalk
rdf(Subject, Predicate, Object)
```

## Граф как набор фактов

```logtalk
:- object(graph).
    :- public(triple/3).

    triple('http://example.org/ada', 'http://schema.org/name', 'Ada Lovelace').
    triple('http://example.org/ada', 'http://schema.org/knows', 'http://example.org/charles').
    triple('http://example.org/charles', 'http://schema.org/name', 'Charles Babbage').

:- end_object.
```

Самый простой запрос — выбрать все тройки:

```logtalk
?- graph::triple(S, P, O).
```

Или найти имя конкретного субъекта:

```logtalk
:- object(graph_query).
    :- public(name/2).

    name(Subject, Name) :-
        graph::triple(Subject, 'http://schema.org/name', Name).

:- end_object.
```

```logtalk
?- graph_query::name('http://example.org/ada', Name).
Name = 'Ada Lovelace'.
```

## IRI, литералы и пустые узлы

- **IRI** — абсолютный идентификатор ресурса. В Logtalk его удобно хранить атомом в кавычках.
- **Литерал** — значение данных: строка, число, дата. Может иметь языковой тег или тип.
- **Пустой узел** — безымянный узел графа. В Logtalk можно представить атомом с префиксом `_:`.

```logtalk
rdf('_:b1', 'http://schema.org/name', 'Anonymous').
```

## Префиксы

Длинные IRI быстро делают код нечитаемым. Введите префиксы:

```logtalk
:- object(prefixes).
    :- public(expand/2).

    expand(schema, 'http://schema.org/').
    expand(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').

:- end_object.
```

На практике проще хранить уже раскрытые IRI либо раскрывать префикс отдельным предикатом перед запросом.

## Запросы

Для небольших графов достаточно `findall/3` и `member/2`:

```logtalk
:- object(graph_query).
    :- use_module(library(lists), [member/2]).
    :- public(people/1).

    people(People) :-
        findall(Person,
            ( graph::triple(Person, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
                           'http://schema.org/Person') ),
            People).

:- end_object.
```

Для больших графов нужен индексированный store или SPARQL-движок.

## Что важно запомнить

- RDF-тройка — это `rdf(Subject, Predicate, Object)`.
- Субъект и предикат обычно являются IRI, объект может быть IRI или литералом.
- Граф можно хранить как набор фактов `triple/3`.
- Простые запросы делаются через перебор фактов.
- Для больших данных нужен специализированный store.
