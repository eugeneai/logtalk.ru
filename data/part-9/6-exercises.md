---
path: '/part-9/6-exercises'
title: 'Упражнения'
hidden: false
---

<text-box variant='learningObjectives' name="Упражнения к части 9">

Вы изучили события, мониторы и перехват сообщений. Выполните упражнения из
файла ```Set9.lgt``` с набором тестов ```Set9Tests.lgt```.

</text-box>

Запуск из каталога `exercises`:

```bash
$ swilgt -l Set9TestsLoader.lgt -g halt
```

## Задания

1. Реализуйте монитор-трассировщик `tracer`.
2. Реализуйте управление подпиской в `event_control`.
3. Реализуйте счётчик сообщений `counter_monitor`.

Тесты проверяют запись событий `before`/`after`, отписку от событий и подсчёт
сообщений, отправленных конкретному объекту.
