---
path: '/part-5/3-objects-as-terms-of-dynamic-fact'
title: 'Экземпляры в базах данных'
hidden: false
---

<text-box variant='learningObjectives' name="Цель изучения раздела">

После изучения этого раздела вы сможете

- Хранить экземпляры объектов в динамических фактах
- Добавлять, удалять и перечислять экземпляры через методы объекта-реестра
- Отличать базу данных объекта от глобальной базы данных Prolog
- Использовать `findall/3` для получения списка объектов

</text-box>

Объект в Logtalk — это терм. Имя объекта (атом или составной терм) можно сохранить в динамическом факте, передать другому объекту или вернуть из метода. Это позволяет строить объектные базы данных: объект-реестр хранит ссылки на экземпляры и предоставляет операции добавления, поиска и удаления.

## Класс person

Сначала определим класс `person`, который хранит имя в собственном состоянии. Используем категорию `name_state` — такую же по устройству, как `count_state` из предыдущего раздела:

```logtalk
:- category(name_state).

    :- public(set_name/1).
    :- public(get_name/1).

    :- private(name_/1).
    :- dynamic(name_/1).

    set_name(Name) :-
        ::retractall(name_(_)),
        ::assertz(name_(Name)).

    get_name(Name) :-
        ::name_(Name).

:- end_category.

:- object(person,
    instantiates(class),
    specializes(object),
    imports(name_state)).

    :- public(name/1).
    :- public(greet/1).

    name(Name) :-
        ::get_name(Name).

    greet(Greeting) :-
        ::name(Name),
        format(atom(Greeting), 'Hello, ~w!', [Name]).

:- end_object.
```

Создадим статический экземпляр `alice` и установим ему имя при загрузке:

```logtalk
:- object(alice,
    instantiates(person)).

    :- initialization(::set_name(alice)).

:- end_object.
```

Теперь `alice::name(Name)` возвращает `alice`, а `alice::greet(Greeting)` — атом `'Hello, alice!'`.

## Объект-реестр

Реестр хранит экземпляры в динамическом предикате `person_/1`. Методы `add/1`, `all/1`, `remove/1` и `count/1` объявлены публичными:

```logtalk
:- object(registry).

    :- public(add/1).
    :- public(all/1).
    :- public(remove/1).
    :- public(count/1).

    :- public(clear/0).
    clear :-
        retractall(person_(_)).

    :- private(person_/1).
    :- dynamic(person_/1).

    add(Person) :-
        (   person_(Person)
        ->  true
        ;   assertz(person_(Person))
        ).

    all(Persons) :-
        findall(Person, person_(Person), Persons).

    remove(Person) :-
        retract(person_(Person)).

    count(Count) :-
        findall(_, person_(_), Persons),
        length(Persons, Count).

:- end_object.
```

Обратите внимание: `person_/1` — приватный предикат. Снаружи доступны только методы реестра. Такой объект инкапсулирует свою базу данных и не позволяет клиенту случайно изменить её в обход интерфейса.

## Работа с реестром

```logtalk
?- person::new(bob), bob::set_name(bob).
true.

?- registry::add(alice), registry::add(bob).
true.

?- registry::all(Persons).
Persons = [alice, bob].

?- registry::count(Count).
Count = 2.

?- registry::remove(alice), registry::count(Count).
Count = 1.
```

В `all/1` используется `findall/3`: он собирает все решения предиката `person_/1` в список. В `count/1` список не нужен, поэтому `findall/3` собирает анонимные решения, а `length/2` вычисляет их количество.

## Почему это база данных объекта, а не глобальная база

В обычном Prolog факты `person_(alice)`, `person_(bob)` находились бы в одной глобальной базе данных. В Logtalk предикат `person_/1` принадлежит объекту `registry`. Другой объект может определить собственный `person_/1` и не конфликтовать с реестром. Это одно из главных преимуществ объектов: они разделяют пространства имён и данные.

Если нужно хранить не только объект, но и связанные с ним данные, в динамическом предикате можно использовать составные термы:

```logtalk
:- dynamic enrollment/2.

add_course(Student, Course) :-
    assertz(enrollment(Student, Course)).
```

Такой предикат также будет принадлежать конкретному объекту, а не всей программе.

## Связь с Haskell

В Haskell список значений и функции над ним — это обычные данные и чистые функции. В Logtalk реестр — это объект с изменяемым состоянием. Оба подхода решают одну задачу: хранить набор объектов и обрабатывать его. Выбор зависит от того, нужна ли изменяемость и инкапсуляция состояния в предметной модели.

В следующем разделе вы примените эти идеи в упражнениях: создадите классы, экземпляры, реестр и фабрику динамических объектов.
