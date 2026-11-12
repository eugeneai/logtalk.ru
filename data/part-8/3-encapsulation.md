---
path: '/part-8/3-encapsulation'
title: 'Адаптер-ориентированное программирование'
hidden: false
---

<text-box variant='learningObjectives' name="Цель изучения раздела">

После изучения этого раздела вы сможете

- Проектировать систему из компонентов и адаптеров
- Хранить компоненты в реестре по именам
- Подменять реализацию без изменения клиента
- Объяснять принцип разделения интерфейса и реализации

</text-box>

В предыдущем разделе адаптер создавался вручную: `uppercase_adapter(plain_text(hello))`. В большой системе удобно иметь **реестр компонентов**, который по имени выдаёт нужный объект. Клиент тогда зависит не от конкретного класса, а от имени и интерфейса.

## Реестр компонентов

```logtalk
:- object(component_registry).

    :- public(register/2).
    :- public(get/2).
    :- public(names/1).

    :- public(clear/0).
    clear :-
        retractall(component_(_, _)).

    :- private(component_/2).
    :- dynamic(component_/2).

    register(Name, Component) :-
        (   component_(Name, _)
        ->  true
        ;   assertz(component_(Name, Component))
        ).

    get(Name, Component) :-
        component_(Name, Component).

    names(Names) :-
        findall(Name, component_(Name, _), Names).

:- end_object.
```

Реестр хранит пары «имя — компонент». Компонентом может быть любой объект, реализующий нужный протокол:

```logtalk
?- component_registry::register(
       greeting,
       uppercase_adapter(plain_text(hello))
   ).
true.

?- component_registry::get(greeting, Component), Component::text(Text).
Component = uppercase_adapter(plain_text(hello)),
Text = 'HELLO'.
```

## Клиент зависит от интерфейса

Клиентский код должен работать с компонентом через протокол, а не через конкретный класс:

```logtalk
:- object(greeting_service).

    :- public(greet/1).
    greet(Greeting) :-
        component_registry::get(greeting, Component),
        Component::text(Text),
        atomic_list_concat(['Привет, ', Text], Greeting).

:- end_object.
```

Теперь можно заменить `plain_text` на другой компонент, не меняя `greeting_service`:

```logtalk
?- component_registry::clear,
   component_registry::register(
       greeting,
       uppercase_adapter(plain_text(world))
   ),
   greeting_service::greet(Greeting).
Greeting = 'Привет, WORLD'.
```

## Адаптер-ориентированный стиль

Адаптер-ориентированное программирование строится на нескольких правилах:

1. **Интерфейс стабилен.** Клиенты зависят от протокола, а не от реализации.
2. **Компоненты малы.** Каждый компонент решает одну задачу.
3. **Адаптеры соединяют.** Преобразование данных и вызовов выносится в отдельный слой.
4. **Реестр скрывает создание.** Клиент не знает, как собрана цепочка компонентов.
5. **Композиция вместо наследования.** Цепочка `uppercase_adapter(prefix_adapter(...))` собирается из независимых частей.

## Пример цепочки

```logtalk
?- Component =
       uppercase_adapter(
           prefix_adapter('>> ', plain_text(hello))
       ),
   Component::text(Text).
Text = '>> HELLO'.
```

Сообщение `text/1` проходит через несколько адаптеров. Каждый адаптер добавляет своё поведение и передаёт работу дальше.

## Ограничения

- Цепочка адаптеров может быть дорогой по сравнению с прямым вызовом.
- Слишком много мелких адаптеров усложняет отладку.
- Интерфейс должен быть действительно общим; иначе адаптеры превращаются в набор частных случаев.

## Что важно запомнить

- Реестр компонентов отделяет клиента от конкретной реализации.
- Клиент работает через протокол.
- Адаптеры можно комбинировать в цепочки.
- Композиция компонентов обычно проще и надёжнее множественного наследования.
