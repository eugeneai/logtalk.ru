---
path: '/part-11/4-html'
title: 'Генерация HTML'
hidden: false
---

<text-box variant='learningObjectives' name="Цель изучения раздела">

После изучения этого раздела вы сможете

- Представлять HTML-документ в виде термов Logtalk
- Генерировать HTML 5 в файл или поток
- Собирать страницу из повторяющихся элементов
- Отделять данные от шаблона

</text-box>

Библиотека `html` позволяет строить HTML из обычных термов. Это удобно, когда страницу нужно сгенерировать программно: например, для отчёта, каталога или документа.

## Подключение

```logtalk
?- logtalk_load(html(loader)).
```

В библиотеке есть объекты `html5` и `xhtml11`. Для современного HTML используется `html5`.

## Представление элементов

Обычный элемент записывается как составной терм:

```logtalk
p('Привет')
```

Элемент с атрибутами:

```logtalk
a([href='https://logtalk.org/'], 'Logtalk')
```

Пустой элемент (например, `hr`) записывается с одним аргументом — списком атрибутов:

```logtalk
hr([class-separator])
```

Содержимое элемента может быть списком дочерних элементов:

```logtalk
ul([li('Первый'), li('Второй'), li('Третий')])
```

## Генерация страницы

```logtalk
:- object(html_demo).
    :- public(write_page/1).

    write_page(File) :-
        html5::generate(
            file(File),
            html([lang=ru], [
                head(title('Пример')),
                body([
                    h1('Привет'),
                    p('Сгенерировано Logtalk')
                ])
            ])
        ).

:- end_object.
```

Вызов:

```logtalk
?- html_demo::write_page('hello.html').
```

В файле появится полноценный HTML-документ с `<!DOCTYPE html>`.

## Генерация фрагмента

Если нужен не весь документ, а только фрагмент, передайте элемент или список элементов:

```logtalk
?- current_output(Stream),
   html5::generate(stream(Stream),
       ul([li(foo), li(bar), li(baz)])).
```

## Данные и шаблон

HTML удобно генерировать из списка данных:

```logtalk
:- object(book_page).
    :- public(write_page/2).

    write_page(File, Books) :-
        findall(
            li([strong(Title), ' — ', Author]),
            member(book(Title, Author), Books),
            Items
        ),
        html5::generate(
            file(File),
            html([lang=ru], [
                head(title('Книги')),
                body([h1('Книги'), ul(Items)])
            ])
        ).

:- end_object.
```

Такой подход разделяет **данные** (`Books`) и **представление** (HTML-термы). Шаблон можно менять независимо от источника данных.

## Что важно запомнить

- HTML-документ описывается термами: имя элемента, атрибуты, содержимое.
- `html5::generate/2` принимает `file(Path)` или `stream(Stream)`.
- Атрибуты задаются парами `Имя=Значение`.
- Повторяющиеся элементы удобно собирать через `findall/3` и затем вставлять в общий терм.
- Генерация HTML — это обычное преобразование термов, без строковой склейки.
