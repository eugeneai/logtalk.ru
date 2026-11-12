---
path: '/part-11/5-json'
title: 'JSON'
hidden: false
---

<text-box variant='learningObjectives' name="Цель изучения раздела">

После изучения этого раздела вы сможете

- Разбирать JSON в термы Logtalk
- Генерировать JSON из термов
- Выбирать представление объектов, пар и строк
- Обрабатывать JSON-ошибки

</text-box>

Библиотека `json` реализует разбор и генерацию JSON в соответствии с RFC 8259. JSON-объекты представляются термами, а массивы — списками.

## Подключение

```logtalk
?- logtalk_load(json(loader)).
```

Основной объект `json` использует представления по умолчанию:

- объекты — в фигурных скобках: `{name-ada, age-36}`;
- пары — через дефис: `name-ada`;
- строки — атомы.

## Разбор JSON

```logtalk
:- object(json_demo).
    :- public(roundtrip/2).

    roundtrip(JSON, Term) :-
        json::parse(atom(JSON), Term).

:- end_object.
```

```logtalk
?- json_demo::roundtrip('{"name":"ada","age":36}', Term).
Term = {name-ada, age-36}.
```

Источником может быть не только атом, но и `codes(Codes)`, `chars(Chars)`, `file(File)` или `stream(Stream)`.

## Генерация JSON

```logtalk
:- object(json_writer).
    :- public(to_json/2).

    to_json(Term, JSON) :-
        json::generate(atom(JSON), Term).

:- end_object.
```

```logtalk
?- json_writer::to_json({name-ada, age-36}, JSON).
JSON = '{"name":"ada","age":36}'.
```

Важно: атомы в Logtalk должны начинаться с маленькой буквы или быть взяты в кавычки. Терм `{name-Ada, ...}` содержит переменную `Ada`, а не строку `"Ada"`.

## Специальные значения

JSON-значения `true`, `false` и `null` представляются термами `@true`, `@false` и `@null`:

```logtalk
?- json::parse(atom('{"active":true,"deleted":false,"note":null}'), Term).
Term = {active-@true, deleted-@false, note-@null}.
```

## Альтернативные представления

Параметрический объект `json/3` позволяет выбрать:

- объекты: `curly` или `list`;
- пары: `dash`, `equal` или `colon`;
- строки: `atom`, `chars` или `codes`.

```logtalk
?- json(list, equal, atom)::parse(
       atom('{"a":1,"b":2}'),
       Term).
Term = json([a=1, b=2]).
```

Такой вариант удобен, если вы хотите обрабатывать JSON-объект как список пар.

## Ошибки

Если входная строка не является корректным JSON, выбрасывается исключение:

```logtalk
?- json::parse(atom('{"a":}'), _).
ERROR: Domain error: `json` ...
```

Поэтому при разборе внешних данных используйте `catch/3` и возвращайте понятную ошибку приложению.

## Что важно запомнить

- `json::parse/2` разбирает JSON, `json::generate/2` создаёт его.
- JSON-объект по умолчанию — `{Key-Value, ...}`, массив — список.
- `true`, `false`, `null` — это `@true`, `@false`, `@null`.
- `json/3` позволяет выбрать представление объектов, пар и строк.
- Внешний JSON нужно разбирать с обработкой исключений.
