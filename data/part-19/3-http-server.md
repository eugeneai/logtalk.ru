---
path: '/part-19/3-http-server'
title: 'HTTP-сервер'
hidden: false
---

<text-box variant='learningObjectives' name="Цель изучения раздела">

После изучения этого раздела вы сможете

- Поднимать HTTP-сервер средствами Logtalk
- Реализовывать обработчик запросов
- Связывать HTTP с JSON-RPC
- Останавливать сервер и обрабатывать ошибки

</text-box>

Библиотека `http_server` позволяет запустить HTTP-сервер и передать запросы объекту-обработчику.

## Подключение

```logtalk
?- logtalk_load(http_server(loader)).
?- logtalk_load(json_rpc(loader)).
```

## Обработчик

Обработчик реализует протокол `http_handler_protocol` и определяет `handle/2`:

```logtalk
:- object(calc_handler,
    implements(http_handler_protocol)).

    handle(Request, Response) :-
        http_core::version(Request, Version),
        http_core::body(Request, content(_, text(JSON))),
        json_rpc::decode(JSON, Message),
        json_rpc::method(Message, subtract),
        json_rpc::params(Message, [A, B]),
        Result is A - B,
        json_rpc::id(Message, Id),
        json_rpc::response(Result, Id, Reply),
        json_rpc::encode(Reply, ReplyJSON),
        http_core::response(
            Version,
            status(200, 'OK'),
            [],
            content('application/json', text(ReplyJSON)),
            [],
            Response
        ).

:- end_object.
```

Разберём шаги:

1. `http_core::version/2` получает версию HTTP.
2. `http_core::body/2` извлекает тело запроса.
3. `json_rpc::decode/2` разбирает JSON-RPC.
4. Проверяются метод и параметры.
5. Формируется JSON-RPC-ответ.
6. `http_core::response/6` создаёт HTTP-ответ.

## Запуск

```logtalk
?- http_server::start('127.0.0.1', 8080, calc_handler, Server, []).
```

Сервер работает в отдельном потоке. Остановить его можно так:

```logtalk
?- http_server::stop(Server).
```

Если нужен сервер, который обслуживает заданное число соединений и завершается, используйте `serve/4`:

```logtalk
?- http_server::serve(8080, calc_handler, 10, [scheme(http)]).
```

## Ошибки

В реальном сервере нужно обрабатывать:

- некорректный JSON;
- неизвестный метод;
- неверные параметры;
- внутренние исключения обработчика.

Не возвращайте клиенту стек вызовов и внутренние пути. Используйте стандартные коды JSON-RPC и общее сообщение.

```logtalk
handle(Request, Response) :-
    catch(
        dispatch(Request, Response),
        Error,
        handle_error(Request, Error, Response)
    ).
```

Здесь `dispatch/3` — ваша бизнес-логика, а `handle_error/3` формирует безопасный ответ и пишет подробности в журнал.

## Безопасность

- Ограничивайте размер тела запроса.
- Проверяйте `Content-Type`.
- Не выполняйте произвольные цели из параметров.
- Используйте аутентификацию для непубличных методов.
- Запускайте сервер за reverse proxy с TLS.

## Что важно запомнить

- `http_handler_protocol` требует `handle/2`.
- `http_server::start/4-5` запускает сервер, `stop/1` останавливает.
- HTTP-слой можно отделить от JSON-RPC-логики.
- Ошибки обработчика нужно превращать в корректные ответы.
- Публичный сервер требует ограничений и аутентификации.
