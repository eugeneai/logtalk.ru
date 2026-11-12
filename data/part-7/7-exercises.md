---
path: '/part-7/7-exercises'
title: 'Практикум по инструментам'
hidden: false
---

<text-box variant='learningObjectives' name="Практикум">

Вы изучили инструменты разработки. Выполните практические задания,
используя наборы `Set5` и `Set6` из каталога `exercises`.

</text-box>

## Задание 1. Загрузчик и проверка проекта

1. Запустите набор 5:

   ```bash
   $ swilgt -l Set5TestsLoader.lgt -g halt
   ```

2. Запустите набор 6:

   ```bash
   $ swilgt -l Set6TestsLoader.lgt -g halt
   ```

3. Создайте собственный `loader.lgt`, который загружает оба набора и
   устанавливает `report(warnings)`.

## Задание 2. Инструктор

Подключите `tutor`:

```logtalk
?- logtalk_load(tutor(loader)).
```

Внесите в копию `Set6.lgt` намеренную ошибку (например, вызов неизвестного
предиката) и сравните сообщение компилятора с `tutor` и без него.

## Задание 3. Отладчик

1. Загрузите `debugger`.
2. Скомпилируйте `Set6Solution.lgt` с флагом `debug(on)`.
3. Проследите выполнение `generator::make_items(3, Items)`.
4. Объясните, на каком шаге создаётся каждый объект.

## Задание 4. Документация

1. Добавьте `info/1` к объекту `generator` и `info/2`/`mode/2` к
   `make_items/2`.
2. Загрузите `lgtdoc` и сгенерируйте документацию:

   ```logtalk
   ?- logtalk_load(lgtdoc(loader)).
   ?- lgtdoc::file('Set6Solution.lgt').
   ```

3. Преобразуйте XML в HTML или Markdown скриптом `lgt2html`/`lgt2md`.

## Задание 5. Тесты

1. Откройте `Set6Tests.lgt` и найдите тест `object_kind`.
2. Добавьте новый тест, который проверяет, что `item` является классом.
3. Убедитесь, что новый тест проходит на `Set6Solution.lgt`.

## Задание 6. Справка

Загрузите `help` и получите справку:

```logtalk
?- help::help.
?- help::logtalk_load/2.
?- help::tool(lgtunit).
```

Найдите в Handbook раздел о флагах компилятора и объясните разницу между
`set_logtalk_flag/2`-директивой и встроенным предикатом.
