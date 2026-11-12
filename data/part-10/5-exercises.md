---
path: '/part-10/5-exercises'
title: 'Упражнения'
hidden: false
---

<text-box variant='learningObjectives' name="Упражнения к части 10">

Вы изучили макросы, DCG и экспертные системы. Выполните упражнения из файла
```Set10.lgt``` с набором тестов ```Set10Tests.lgt```.

</text-box>

Запуск из каталога `exercises`:

```bash
$ swilgt -l Set10TestsLoader.lgt -g halt
```

## Задания

1. Реализуйте грамматику приветствия `greeting//1`.
2. Реализуйте разбор чисел и суммы в `arithmetic_parser`.
3. Реализуйте правила экспертной системы `animal_expert::solve/2`.

Тесты проверяют разбор списков, вычисление суммы и классификацию животных по
фактам `has/2`.
