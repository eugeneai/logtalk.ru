---
path: '/part-8/7-exercises'
title: 'Упражнения'
hidden: false
---

<text-box variant='learningObjectives' name="Упражнения к части 8">

Вы изучили компоненты, адаптеры, прокси и мультиадаптеры. Выполните
упражнения из файла ```Set8.lgt``` с набором тестов ```Set8Tests.lgt```.

</text-box>

Запуск из каталога `exercises`:

```bash
$ swilgt -l Set8TestsLoader.lgt -g halt
```

## Задания

1. Опишите протокол `text_p` с публичным `text/1`.
2. Реализуйте компонент `plain_text/1`.
3. Реализуйте адаптер `uppercase_adapter/1`.
4. Реализуйте прокси-адаптер `prefix_adapter/2`.
5. Реализуйте мультиадаптер `composite_text/1`.
6. Реализуйте реестр компонентов `component_registry`.

Тесты проверяют интерфейс, разбор текста, порядок компонентов и отсутствие
дубликатов в реестре.
