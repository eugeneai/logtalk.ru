---
path: '/part-19/2-json-rpc'
title: 'JSON-RPC 2.0'
hidden: false
---

<text-box variant='learningObjectives' name="Цель изучения раздела">

После изучения этого раздела вы сможете

- Создавать запросы, уведомления и ответы JSON-RPC
- Кодировать и декодировать сообщения
- Извлекать метод, параметры и идентификатор
- Формировать стандартные ошибки

</text-box>

Библиотека `json_rpc` реализует JSON-RPC 2.0. Она строит и разбирает сообщения, а также помогает классифицировать их.

## Подключение

```logtalk
?- logtalk_load(json_rpc(loader)).
```

## Запрос

```logtalk
?- json_rpc::request(subtract, [42, 23], 1, Request).
Request = {jsonrpc-'2.0', method-subtract, params-[42,23], id-1}.
```

Первый аргумент — метод, второй — параметры, третий — идентификатор.

## Кодирование

```logtalk
?- json_rpc::encode(Request, JSON).
JSON = '{"jsonrpc":"2.0","method":"subtract","params":[42,23],"id":1}'.
```

## Декодирование

```logtalk
?- json_rpc::decode(
       '{"jsonrpc":"2.0","result":19,"id":1}',
       Message).
Message = {jsonrpc-'2.0', result-19, id-1}.
```

## Классификация

```logtalk
?- json_rpc::is_request(Message).
?- json_rpc::is_notification(Message).
?- json_rpc::is_response(Message).
?- json_rpc::is_error_response(Message).
```

Эти предикаты позволяют обработчику выбрать ветку без ручного анализа структуры.

## Поля сообщения

```logtalk
?- json_rpc::method(Message, Method).
?- json_rpc::params(Message, Params).
?- json_rpc::id(Message, Id).
?- json_rpc::result(Message, Result).
?- json_rpc::error_code(Message, Code).
```

## Ответ и ошибки

```logtalk
?- json_rpc::response(19, 1, Response).
Response = {jsonrpc-'2.0', result-19, id-1}.

?- json_rpc::error_response(-32601, 'Method not found', 1, Error).
```

Стандартные конструкторы ошибок:

```logtalk
json_rpc::parse_error(Error)
json_rpc::invalid_request(Error)
json_rpc::method_not_found(Id, Error)
json_rpc::invalid_params(Id, Error)
json_rpc::internal_error(Id, Error)
```

## Пакетные запросы

JSON-RPC поддерживает массив сообщений. Проверить пакет можно через `is_batch/1`, а затем обработать элементы по отдельности. Ответ на пакет — тоже массив, порядок может не совпадать с порядком запросов.

## Что важно запомнить

- JSON-RPC 2.0 использует поля `jsonrpc`, `method`, `params`, `id`.
- `request/4` создаёт запрос, `response/3` — ответ.
- `encode/2` и `decode/2` переводят между термами и JSON.
- Ошибки имеют стандартные коды.
- Классификация сообщений упрощает обработчик.
