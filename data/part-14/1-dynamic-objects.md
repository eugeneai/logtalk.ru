---
path: '/part-14/1-dynamic-objects'
title: 'Динамические объекты'
hidden: false
---

<text-box variant='learningObjectives' name="Цель изучения раздела">

После изучения этого раздела вы сможете

- Создавать объекты во время выполнения
- Задавать интерфейс и начальные факты динамического объекта
- Удалять временные объекты
- Понимать, когда динамические объекты уместны

</text-box>

Обычно объекты Logtalk объявляются в исходном файле. Но иногда объект нужно создать **во время выполнения**: например, для конфигурации, временного обработчика или представления данных, загруженных из внешнего источника.

## `create_object/4`

```logtalk
create_object(Identifier, Relations, Directives, Clauses)
```

- `Identifier` — имя объекта или переменная;
- `Relations` — список отношений (`implements(...)`, `extends(...)`);
- `Directives` — директивы (`public(...)`, `dynamic(...)`);
- `Clauses` — начальные предложения.

Пример фабрики:

```logtalk
:- object(factory).
    :- public(make/1).

    make(Object) :-
        create_object(
            Object,
            [implements(shapep)],
            [public(area/1)],
            [area(42)]
        ).

:- end_object.
```

```logtalk
?- factory::make(Object),
   Object::area(Area).
Object = ...,
Area = 42.
```

Если имя не задано, Logtalk сгенерирует его автоматически. Такой объект существует, пока его не удалят или пока не завершится сеанс.

## Начальные факты и динамические предикаты

Динамический объект часто используют как хранилище:

```logtalk
:- object(store_factory).
    :- public(new_store/1).

    new_store(Store) :-
        create_object(
            Store,
            [],
            [public(get/2), public(put/2), dynamic(value_/2)],
            [
                (get(Key, Value) :- value_(Key, Value)),
                (put(Key, Value) :-
                    retractall(value_(Key, _)),
                    assertz(value_(Key, Value)))
            ]
        ).

:- end_object.
```

```logtalk
?- store_factory::new_store(Store),
   Store::put(a, 1),
   Store::get(a, Value).
Value = 1.
```

Для изменения таких фактов объект должен быть объявлен динамическим или содержать динамические предикаты.

## Удаление объекта

```logtalk
abolish_object(Object)
```

удаляет объект и его предложения. Используйте его для временных объектов, чтобы не накапливать состояние.

```logtalk
with_temp_object(Goal) :-
    create_object(Temp, [], [public(run/0)], [run :- call(Goal)]),
    call(Temp::run),
    abolish_object(Temp).
```

## Когда это нужно

- Генерация объектов из конфигурации или модели.
- Плагины и обработчики, создаваемые по запросу.
- Временные представления данных.
- Тесты, где нужно изолировать состояние.

Не создавайте динамические объекты там, где достаточно обычного терма или словаря: они усложняют отладку и управление памятью.

## Что важно запомнить

- `create_object/4` создаёт объект во время выполнения.
- Можно передать отношения, директивы и начальные предложения.
- `abolish_object/1` удаляет объект.
- Динамические объекты удобны для плагинов и конфигураций.
- Для простых данных лучше использовать термы или словари.
