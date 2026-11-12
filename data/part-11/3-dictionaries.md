---
path: '/part-11/3-dictionaries'
title: 'Словари, кучи и очереди'
hidden: false
---

<text-box variant='learningObjectives' name="Цель изучения раздела">

После изучения этого раздела вы сможете

- Выбирать реализацию словаря из библиотеки `dictionaries`
- Работать с кучей и извлекать минимальный элемент
- Использовать очередь для обхода в ширину
- Понимать, почему внутреннее представление структур нужно скрывать

</text-box>

Стандартная библиотека Logtalk содержит несколько готовых структур данных. Их внутреннее представление считается **непрозрачным**: работать с ним нужно только через предикаты библиотеки.

## Словари

Библиотека `dictionaries` предоставляет протокол словаря и несколько реализаций: `bintree`, `avltree`, `rbtree`, `splaytree` и `two3tree`.

```logtalk
?- logtalk_load(dictionaries(loader)).
```

Создание и заполнение AVL-дерева:

```logtalk
:- object(dict_demo).
    :- public(run/0).

    run :-
        avltree::new(D0),
        avltree::insert(D0, apple, 3, D1),
        avltree::insert(D1, pear, 5, D2),
        avltree::lookup(apple, Value, D2),
        write(apple=Value), nl.

:- end_object.
```

```logtalk
?- dict_demo::run.
apple=3
```

Основные операции:

| Предикат | Назначение |
|---|---|
| `new/1` | создать пустой словарь |
| `insert/4` | вставить пару «ключ — значение» |
| `lookup/3` | найти значение по ключу |
| `update/4` | изменить значение существующего ключа |
| `delete/3` | удалить ключ |
| `keys/2` | получить список ключей |
| `values/2` | получить список значений |
| `size/2` | получить число элементов |

Для небольших словарей подойдёт `bintree`; для больших — `avltree`, `rbtree` или `two3tree`. `splaytree` полезен, когда недавно использованные элементы запрашиваются снова.

## Кучи

Куча хранит пары «ключ — значение» и быстро выдаёт элемент с минимальным или максимальным ключом. Реализации: `binary_heap(Order)` и `pairing_heap(Order)`, а также готовые объекты `binary_heap_min`, `pairing_heap_min`, `binary_heap_max`, `pairing_heap_max`.

```logtalk
:- object(heap_demo).
    :- public(run/0).

    run :-
        pairing_heap_min::new(H0),
        pairing_heap_min::insert(7, seven, H0, H1),
        pairing_heap_min::insert(3, three, H1, H2),
        pairing_heap_min::delete(H2, Min, _, _),
        write(min=Min), nl.

:- end_object.
```

```logtalk
?- heap_demo::run.
min=3
```

Обратите внимание: `insert/4` принимает ключ, значение, старую и новую кучу. `delete/4` возвращает ключ и значение верхнего элемента.

Кучи удобны для алгоритмов Дейкстры, A* и обработки событий по приоритету.

## Очереди

Библиотека `queues` реализует очередь FIFO.

```logtalk
:- object(queue_demo).
    :- public(run/0).

    run :-
        queue::new(Q0),
        queue::join(a, Q0, Q1),
        queue::join(b, Q1, Q2),
        queue::serve(Q2, Item, _),
        write(head=Item), nl.

:- end_object.
```

```logtalk
?- queue_demo::run.
head=a
```

Основные предикаты: `new/1`, `join/3`, `join_all/3`, `serve/3`, `head/2`. Очередь пригодится для обхода графа или дерева в ширину, когда важен порядок обработки.

## Что важно запомнить

- Словари, кучи и очереди — это непрозрачные термы; используйте только API библиотеки.
- `dictionaries` даёт несколько реализаций с общим протоколом.
- Куча упорядочивает пары по ключу и подходит для приоритетной обработки.
- Очередь реализует порядок FIFO.
- Выбор структуры данных влияет на сложность и читаемость алгоритма.
