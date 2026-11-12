---
path: '/part-17/3-sparql'
title: 'SPARQL и HTTP'
hidden: false
---

<text-box variant='learningObjectives' name="Цель изучения раздела">

После изучения этого раздела вы сможете

- Формулировать простой SPARQL-запрос
- Отправлять запрос на endpoint через HTTP
- Разбирать JSON-ответ
- Понимать ограничения прямых HTTP-запросов

</text-box>

**SPARQL** — язык запросов к RDF-графам. Типичный endpoint принимает запрос по HTTP и возвращает результаты в JSON.

## Запрос

Пример: найти имена людей.

```sparql
PREFIX schema: <http://schema.org/>

SELECT ?person ?name
WHERE {
  ?person a schema:Person ;
          schema:name ?name .
}
```

Результат в формате `application/sparql-results+json` выглядит так:

```json
{
  "head": {"vars": ["person", "name"]},
  "results": {
    "bindings": [
      {
        "person": {"type": "uri", "value": "http://example.org/ada"},
        "name": {"type": "literal", "value": "Ada Lovelace"}
      }
    ]
  }
}
```

## HTTP-клиент

Библиотека `http_client` умеет отправлять GET-запросы:

```logtalk
?- logtalk_load(http_client(loader)).
```

Схема запроса:

```logtalk
:- object(sparql_client).
    :- public(query/3).

    query(Endpoint, EncodedQuery, Result) :-
        atomic_list_concat([Endpoint, '?query=', EncodedQuery], URL),
        http_client::get(URL, Response, []),
        http_core::status(Response, status(200, _)),
        http_core::body(Response, content(_, text(JSON))),
        json::parse(atom(JSON), Result).

:- end_object.
```

`EncodedQuery` должен быть URL-кодированным. Для этого используйте библиотеку работы с URI или backend-предикаты кодирования. Не склеивайте запрос с пробелами и спецсимволами напрямую.

## Разбор результата

Ответ содержит `head` и `results.bindings`. Каждая привязка — объект с переменными запроса:

```logtalk
?- sparql_client::query(Endpoint, Query, Result),
   Result = {head-_, results-{bindings-Bindings}}.
```

Дальше можно преобразовать `Bindings` в список собственных термов:

```logtalk
person_name(Binding, person(Name, Person)) :-
    Binding = {person-{value-Person}, name-{value-Name}}.
```

Точная структура зависит от представления JSON-объектов (`curly` или `list`).

## Ограничения

- Endpoint может ограничивать размер ответа и время выполнения.
- Нужна обработка сетевых ошибок и таймаутов.
- URL-кодирование обязательно.
- Некоторые endpoint требуют POST и заголовок `Content-Type`.
- JSON-ответ может быть большим; не загружайте его целиком без необходимости.

## Что важно запомнить

- SPARQL-запрос описывает шаблон поиска по RDF-графу.
- Endpoint принимает запрос по HTTP и возвращает JSON.
- `http_client` отправляет запрос, `json` разбирает ответ.
- Запрос нужно URL-кодировать.
- Сетевые ошибки и таймауты нужно обрабатывать явно.
