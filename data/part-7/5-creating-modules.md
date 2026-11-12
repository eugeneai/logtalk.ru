---
path: '/part-7/5-testing'
title: 'Разработка тестов'
hidden: false
---

<text-box variant='learningObjectives' name="Цель изучения раздела">

После изучения этого раздела вы сможете

- Писать модульные тесты для Logtalk-объектов
- Использовать инструмент `lgtunit`
- Понимать устройство учебного набора тестов курса
- Запускать отдельные тесты и целые наборы

</text-box>

Тестирование в Logtalk строится на тех же идеях, что и в других языках: тест описывает входные данные, действие и ожидаемый результат. Стандартный инструмент — **`lgtunit`**. В упражнениях курса используется совместимый по духу учебный набор `studyunit`, который печатает результаты по-русски.

## Тесты в стиле lgtunit

Тестовый объект наследует `lgtunit`:

```logtalk
:- object(counter_tests,
    extends(lgtunit)).

    test(initial_value, true(counter::value(0))).
    test(increment, true((
        counter::increment,
        counter::value(1)
    ))).

:- end_object.
```

Тест `test(Name, true(Goal))` успешен, если `Goal` завершается успешно. Для проверки неудачи используется `fail(Goal)`:

```logtalk
test(unknown_message, fail(counter::unknown)).
```

Для проверки исключения:

```logtalk
test(division_by_zero, throws(error(evaluation_error(zero_divisor), _)),
    X is 1 / 0).
```

## Запуск тестов

```logtalk
?- logtalk_load(lgtunit(loader)).
?- logtalk_load(counter_tests).
?- lgtunit::run_test_sets([counter_tests]).
```

`lgtunit` печатает число успешных, проваленных и пропущенных тестов. Если тест не прошёл, выводится имя теста и причина.

## Подготовка и очистка

Для тестов с общим состоянием используются `setup/1` и `cleanup/1`:

```logtalk
:- object(registry_tests,
    extends(lgtunit)).

    setup(create_registry).
    cleanup(delete_registry).

    create_registry :-
        create_object(test_registry, [], [], []).

    delete_registry :-
        abolish_object(test_registry).

:- end_object.
```

## Учебный набор курса

В каталоге `exercises` используется собственный набор `studyunit`. Он поддерживает:

- `test(Name, Outcome, Options, Goal)`;
- исходы `true`, `fail`, `all(...)`, `none(...)`;
- опции `condition/1`, `explain/1`, `each_*`;
- русские сообщения об ошибках и подсказки.

Пример:

```logtalk
test(counter_starts_at_zero, true,
    [ explain(::error('Новый счётчик должен начинаться с нуля.')) ],
    ( counter::new(C),
      C::value(0) )).
```

Исключения в тестовых целях не прерывают весь набор: тест помечается как проваленный, а остальные тесты продолжают выполняться.

Запуск набора:

```bash
$ swilgt -l Set5TestsLoader.lgt -g halt
```

## Что тестировать

- начальные значения и граничные случаи;
- изменение состояния после сообщений;
- наследование и переопределение методов;
- обработку ошибок и исключений;
- взаимодействие нескольких объектов.

## Что важно запомнить

- Тест — это обычный предикат, который должен завершиться успешно или неуспешно.
- `lgtunit` — стандартный инструмент; `studyunit` — учебный набор курса.
- Тесты должны быть независимыми и очищать за собой динамические объекты.
- Падающий тест — это точка входа для отладчика.
