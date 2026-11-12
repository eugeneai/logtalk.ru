---
path: '/part-6/5-exercises'
title: 'Упражнения'
hidden: false
---

<text-box variant='learningObjectives' name="Упражнения к части 6">

Вы изучили рефлексию, управление компилятором и генераторы экземпляров.
Выполните упражнения из файла ```Set6.lgt``` с набором тестов
```Set6Tests.lgt```.

</text-box>

Запуск из каталога `exercises`:

```bash
$ swilgt -l Set6TestsLoader.lgt -g halt
```

## Задания

1. Реализуйте `inspector::all_objects/1`, `public_predicates/2` и
   `object_kind/2`.
2. Реализуйте фабрику `object_factory::spawn/2` и `destroy/1`.
3. Реализуйте генератор `generator::make_items/2`.
4. Реализуйте `compiler::flag/2` и `set_flag/2` для работы с флагами Logtalk.

Тесты проверяют не только наличие методов, но и поведение: классификацию
объектов, создание и уничтожение экземпляров, генерацию набора и работу с
флагами.
