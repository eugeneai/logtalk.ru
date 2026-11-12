---
path: '/part-11/6-logging'
title: 'Журналирование'
hidden: false
---

<text-box variant='learningObjectives' name="Цель изучения раздела">

После изучения этого раздела вы сможете

- Подключать категорию `logging`
- Заводить и инициализировать журнальные файлы
- Записывать события в журнал
- Включать и выключать журналирование во время работы

</text-box>

Для диагностики и аудита приложению нужен журнал. Библиотека `logging` предоставляет простой механизм записи событий в файлы.

## Подключение

```logtalk
?- logtalk_load(logging(loader)).
```

Категория `logging` реализует протокол `loggingp`. Её можно импортировать в объект приложения:

```logtalk
:- object(app_log,
    imports(logging)).

    :- public(start/1).

    start(File) :-
        ^^init_log_file(app, File),
        ^^log_event(app, started).

:- end_object.
```

`init_log_file/2` создаёт файл заново, а `define_log_file/2` открывает существующий файл и сохраняет его содержимое.

## Запись событий

```logtalk
:- object(job_runner,
    imports(logging)).

    :- public(run/1).

    run(File) :-
        ^^init_log_file(job, File),
        ^^log_event(job, begin),
        do_work,
        ^^log_event(job, done).

    do_work :-
        thread_sleep(0.01).

:- end_object.
```

В файле появятся строки:

```text
begin
done
```

`log_event/2` принимает произвольный непустой терм. Обычно используют атомы или составные термы вроде `processed(42)`.

## Включение и выключение

```logtalk
?- app_log::disable_logging(app).
?- app_log::logging(app).
false.

?- app_log::enable_logging(app).
?- app_log::logging(app).
true.
```

Это позволяет включать подробный журнал только при необходимости, не меняя код приложения.

## Несколько журналов

Один объект может вести несколько независимых журналов:

```logtalk
:- object(multi_log,
    imports(logging)).

    :- public(start/2).

    start(AppFile, ErrorFile) :-
        ^^init_log_file(app, AppFile),
        ^^init_log_file(errors, ErrorFile),
        ^^log_event(app, started),
        ^^log_event(errors, none).

:- end_object.
```

Каждый журнал идентифицируется атомом-алиасом. Файлы можно открывать и закрывать независимо.

## Ограничения

Библиотека `logging`:

- не выполняет ротацию файлов;
- не добавляет дату и время автоматически;
- не синхронизирует запись между потоками.

Если нужны метки времени, добавьте их в событие:

```logtalk
log_event(app, at('2026-11-12T10:00:00', started)).
```

Для высоконагруженных систем лучше использовать специализированную библиотеку журналирования backend-компилятора.

## Что важно запомнить

- `logging` — категория, подключается через `imports(logging)`.
- `init_log_file/2` очищает файл, `define_log_file/2` сохраняет содержимое.
- `log_event/2` записывает одно событие в журнал с указанным алиасом.
- `enable_logging/1` и `disable_logging/1` управляют записью во время работы.
- Метки времени и ротацию нужно добавлять самостоятельно.
