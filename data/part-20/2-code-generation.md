---
path: '/part-20/2-code-generation'
title: 'Генерация кода'
hidden: false
---

<text-box variant='learningObjectives' name="Цель изучения раздела">

После изучения этого раздела вы сможете

- Генерировать исходный код из модели
- Использовать `format/3` для шаблонов
- Создавать объекты во время выполнения
- Понимать ограничения генерации

</text-box>

Из модели можно сгенерировать исходный код. Простейший способ — подставить данные в шаблон через `format/3`.

## Шаблон

```logtalk
:- object(generator).
    :- public(generate/2).

    generate(Entity, Code) :-
        course_model::entity(Entity, Fields, _),
        format(
            atom(Code),
            ':- object(~w).~n    fields(~w).~n:- end_object.~n',
            [Entity, Fields]
        ).

:- end_object.
```

```logtalk
?- generator::generate(person, Code).
Code = ':- object(person).\n    fields([name,age]).\n:- end_object.\n'.
```

Полученный текст можно записать в файл и загрузить:

```logtalk
?- generator::generate(person, Code),
   setup_call_cleanup(
       open('person.lgt', write, Stream),
       write(Stream, Code),
       close(Stream)
   ),
   logtalk_load('person.lgt').
```

## Генерация методов

Для каждого поля можно сгенерировать getter и setter. Шаблон усложняется, но остаётся обычной строкой:

```logtalk
field_methods(Fields, Code) :-
    findall(
        Line,
        ( member(Field, Fields),
          format(atom(Line),
                 '    ~w(Value) :- value_(~w, Value).~n',
                 [Field, Field]) ),
        Lines
    ),
    atomic_list_concat(Lines, Code).
```

Затем `Lines` вставляются в общий шаблон объекта.

## Динамическое создание

Вместо генерации текста можно сразу создать объект:

```logtalk
:- object(runtime_generator).
    :- public(entity_object/2).

    entity_object(Entity, Object) :-
        course_model::entity(Entity, Fields, _),
        create_object(
            Object,
            [],
            [public(fields/1)],
            [fields(Fields)]
        ).

:- end_object.
```

```logtalk
?- runtime_generator::entity_object(person, Object),
   Object::fields(Fields).
Fields = [name, age].
```

Динамический объект существует только в текущем сеансе. Если нужен постоянный код, генерируйте файл.

## Ограничения

- Шаблоны легко ломаются при изменении модели.
- Сгенерированный код сложнее отлаживать.
- Нужна проверка модели до генерации.
- Не генерируйте код, который затем правится вручную: изменения потеряются.
- Для сложных моделей лучше использовать промежуточное представление и несколько трансформаций.

## Что важно запомнить

- `format/3` подставляет данные модели в шаблон.
- Сгенерированный текст можно загрузить как обычный файл Logtalk.
- `create_object/4` создаёт объект без генерации файла.
- Генерация требует валидации модели.
- Сгенерированный код должен быть воспроизводимым.
