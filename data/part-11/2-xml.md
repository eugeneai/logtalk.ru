---
path: '/part-11/2-xml'
title: 'Разбор XML'
hidden: false
---

<text-box variant='learningObjectives' name="Цель изучения раздела">

После изучения этого раздела вы сможете

- Загружать XML-документ в виде дерева термов
- Находить элементы и извлекать их содержимое
- Понимать, когда для XML нужна библиотека backend-компилятора

</text-box>

В стандартной библиотеке Logtalk нет универсального XML-парсера: работа с XML зависит от backend-компилятора. Для SWI-Prolog используется библиотека `sgml`, которая преобразует XML в обычные термы Logtalk.

## Представление XML

Документ

```xml
<catalog>
  <book id="1"><title>Logtalk</title></book>
  <book id="2"><title>Prolog</title></book>
</catalog>
```

загружается как терм:

```logtalk
element(catalog, [],
    [element(book, [id=1],
         [element(title, [], [Logtalk])]),
     element(book, [id=2],
         [element(title, [], [Prolog])])])
```

Общий вид элемента:

```logtalk
element(Имя, Атрибуты, ДочерниеЭлементы)
```

Атрибуты — список пар `Имя=Значение`. Текстовое содержимое — атомы или строки в списке детей.

## Загрузка файла

```logtalk
:- object(xml_reader).
    :- use_module(library(sgml), [load_xml_file/2]).
    :- use_module(library(lists), [member/2]).

    :- public(book_titles/2).

    book_titles(File, Titles) :-
        load_xml_file(File, [element(catalog, _, Books)]),
        findall(
            Title,
            ( member(element(book, _, Children), Books),
              member(element(title, _, [Title]), Children) ),
            Titles
        ).

:- end_object.
```

Вызов:

```logtalk
?- xml_reader::book_titles('books.xml', Titles).
Titles = [Logtalk, Prolog].
```

`load_xml_file/2` — предикат SWI-Prolog. В объекте Logtalk его нужно явно импортировать через `use_module/2`. В других backend-компиляторах имя библиотеки и предиката может отличаться.

## Почему не `load_structure/3`

В SWI-Prolog есть более общий `load_structure/3`, но это мета-предикат с нестандартной спецификацией аргументов. При импорте в Logtalk он может вызвать ошибку `domain_error(meta_argument_specifier, ...)`. Для простого чтения XML удобнее `load_xml_file/2`.

## Обработка пространств имён

Атрибуты пространств имён выглядят как обычные атрибуты с двоеточием в имени:

```logtalk
element(book, ['xmlns:dc'='http://purl.org/dc/elements/1.1/'], ...)
```

Если документ большой или требует проверки схемы, лучше использовать специализированную библиотеку backend-компилятора или внешний инструмент, а в Logtalk передавать уже нормализованные данные.

## Что важно запомнить

- XML в Logtalk обычно представляется деревом термов `element/3`.
- Для SWI-Prolog используйте `library(sgml)` и `load_xml_file/2`.
- Атрибуты — список пар `Имя=Значение`.
- XML-парсеры относятся к backend-компилятору, поэтому код может быть непереносимым.
- Для прикладной логики удобнее сразу преобразовать XML в собственные термы и объекты.
