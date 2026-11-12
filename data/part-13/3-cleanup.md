---
path: '/part-13/3-cleanup'
title: 'Освобождение ресурсов'
hidden: false
---

<text-box variant='learningObjectives' name="Цель изучения раздела">

После изучения этого раздела вы сможете

- Закрывать потоки и файлы даже при исключении
- Использовать `setup_call_cleanup/3`
- Понимать разницу между `call_cleanup/2` и `setup_call_cleanup/3`
- Писать безопасные предикаты работы с внешними ресурсами

</text-box>

Файлы, сокеты, соединения с базой данных и блокировки нужно освобождать. Если между открытием и закрытием произойдёт исключение, обычный `close/1` может не выполниться. Для этого есть `setup_call_cleanup/3`.

## `setup_call_cleanup/3`

```logtalk
setup_call_cleanup(Setup, Goal, Cleanup)
```

- `Setup` выполняется один раз;
- `Goal` — основная работа;
- `Cleanup` выполняется всегда: при успехе, неудаче и исключении.

Пример чтения первой строки файла:

```logtalk
:- object(resource_demo).
    :- use_module(library(readutil), [read_line_to_string/2]).
    :- public(read_first/2).

    read_first(File, Line) :-
        setup_call_cleanup(
            open(File, read, Stream),
            read_line_to_string(Stream, Line),
            close(Stream)
        ).

:- end_object.
```

```logtalk
?- resource_demo::read_first('data.txt', Line).
Line = hello.
```

Даже если `read_line_to_string/2` выбросит исключение, поток будет закрыт.

## `call_cleanup/2`

`call_cleanup(Goal, Cleanup)` выполняет очистку после `Goal`, но не содержит отдельной стадии подготовки:

```logtalk
with_lock(Lock, Goal) :-
    call_cleanup(
        ( acquire_lock(Lock), call(Goal) ),
        release_lock(Lock)
    ).
```

Если `acquire_lock/1` сам выбросит исключение, `Cleanup` всё равно будет вызван. Однако `setup_call_cleanup/3` точнее выражает намерение: «сначала подготовить, потом работать, затем освободить».

## Несколько ресурсов

Вложенные `setup_call_cleanup/3` освобождают ресурсы в обратном порядке:

```logtalk
copy_file(From, To) :-
    setup_call_cleanup(
        open(From, read, In),
        setup_call_cleanup(
            open(To, write, Out),
            copy_stream(In, Out),
            close(Out)
        ),
        close(In)
    ).
```

Сначала закроется выходной поток, затем входной.

## Ошибки в очистке

Если `Cleanup` выбрасывает исключение, оно может заменить исходное. Поэтому очистка должна быть максимально простой и надёжной. Для закрытия потоков часто используют `catch(close(Stream), _, true)`, если повторное закрытие допустимо.

## Что важно запомнить

- `setup_call_cleanup/3` гарантирует выполнение очистки.
- `Setup` выполняется до `Goal`, `Cleanup` — после.
- Вложенные вызовы освобождают ресурсы в обратном порядке.
- `call_cleanup/2` полезен, когда подготовка уже выполнена.
- Очистка не должна выбрасывать новые исключения без необходимости.
