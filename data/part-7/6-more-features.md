---
path: '/part-7/6-help'
title: 'Система подсказки'
hidden: false
---

<text-box variant='learningObjectives' name="Цель изучения раздела">

После изучения этого раздела вы сможете

- Подключать инструмент `help`
- Получать справку по предикатам, объектам и библиотекам
- Открывать Handbook и API-документацию из верхнего уровня
- Настраивать браузер для просмотра документации

</text-box>

Инструмент **`help`** открывает документацию Logtalk прямо из верхнего уровня. Он дополняет `tutor`: `tutor` объясняет сообщения компилятора, а `help` даёт справку по языку и библиотекам.

## Подключение

```logtalk
?- logtalk_load(help(loader)).
```

После загрузки:

```logtalk
?- help::help.
```

Инструмент покажет краткую справку и список возможностей.

## Примеры запросов

Справка по встроенному предикату:

```logtalk
?- help::logtalk_load/2.
```

Справка по грамматическому правилу:

```logtalk
?- help::eos//0.
```

Открыть Handbook:

```logtalk
?- help::handbook.
```

Открыть API-документацию:

```logtalk
?- help::apis.
```

Справка по библиотечному предикату:

```logtalk
?- help::apis(member/2).
```

Документация библиотеки:

```logtalk
?- help::library(random).
```

Документация сущности:

```logtalk
?- help::entity(logtalk).
```

Документация инструмента:

```logtalk
?- help::tool(lgtunit).
```

Справка по скрипту:

```logtalk
?- help::man(logtalk_tester).
```

## Требования

На Linux нужен `xdg-open` для открытия документации в браузере. Для просмотра Handbook и API прямо в терминале нужен один из текстовых браузеров: `lynx`, `w3m`, `links` или `chawan`.

На Windows используется браузер по умолчанию. На macOS — команда `open`.

## Настройка

Предпочитаемый браузер задаётся пользовательским флагом `help_default_browser`:

```logtalk
:- initialization(
    create_logtalk_flag(help_default_browser, w3m, [type(atom), keep(true)])
).
```

Допустимые значения: `lynx`, `w3m`, `links`, `cha`, `default`.

Формат документации задаётся флагом `help_default_files`:

```logtalk
:- initialization(
    create_logtalk_flag(help_default_files, xhtml, [type(atom), keep(true)])
).
```

Значения: `html` (по умолчанию) или `xhtml`.

## Что важно запомнить

- `help` — быстрый доступ к Handbook и API из верхнего уровня.
- `help::help` — точка входа.
- `help::tool/1`, `help::library/1`, `help::entity/1` открывают нужный раздел.
- `tutor` объясняет ошибки, `help` показывает документацию.
