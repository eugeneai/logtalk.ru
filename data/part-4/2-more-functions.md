---
path: '/part-4/2-parametric-categories'
title: 'Параметризованные категории'
hidden: false
---

<text-box variant='learningObjectives' name="Цель освоения материала">

Освоение данного раздела позволит вам

- Задавать параметризированные категории
- Организовывать наследование свойств между параметризованными категориями
- Импортировать параметризованные категории в объекты

</text-box>

В предыдущей части мы изучили параметричекие объекты, с категориями мы можем действовать аналогично - использовать параметры. Пересмотрим пример ихз зраздела, посвященного параметрическим объектам, проиллюстрируем использование параметрических категорий.  Первая категория описывает вариант одежды, выбирает согласно сезону (параметр категории).

```logtalk

:- category(dress(_Season)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2010-02-17,
		comment is 'Dress advice according to the season.',
		parnames is ['Season']
	]).

	:- public(clothes/1).

	clothes(Clothes) :-
		parameter(1, Season),
		clothes(Season, Clothes).

	clothes(winter, [pants, sleeves, heavy]).
	clothes(spring, [shorts, sleeves, light]).
	clothes(summer, [shorts, light, white]).
	clothes(autumn, [pants, sleeves, light]).

:- end_category.
```

Зададим категорию для описания речей (тостов, высказываний, лекций и т.п.).

```logtalk
:- category(speech(_Event)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2010-02-17,
		comment is 'Speech advice according to the event.',
		parnames is ['Event']
	]).

	:- public(speech/1).

	speech(Speech) :-
		parameter(1, Event),
		speech(Event, Speech).

	speech(wedding, [happy, jokes]).
	speech(inauguration, [formal, long]).

:- end_category.
```

Снова создаем "плоский" вариант объекта, где все методы собраны в объекте.

```logtalk
:- object(speech(Season, Event),
	imports((dress(Season), speech(Event)))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2014-08-14,
		comment is 'Speech and dress advice according to the season and the event.',
		parnames is ['Season', 'Event']
	]).

	:- public(advice/0).
	advice :-
		^^clothes(Clothes),
		write('Clothes: '), write(Clothes), nl,
		^^speech(Speech),
		write('Speech:  '), write(Speech), nl, nl.

	:- public(advice/2).
	advice(Clothes, Speech) :-
		^^clothes(Clothes),
		^^speech(Speech).

:- end_object.
```

<!---
A quiz to review the contents of this section:

<quiz id="fb289a3b-288a-5a9f-b3d2-07ceae5866ea"></quiz>
-->
