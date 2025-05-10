---
path: '/part-4/1-categories'
title: 'Категории'
hidden: false
---

<text-box variant='learningObjectives' name="Цели изучения категорий">

После изучения данного раздела вы сможете

- Задавать категории, импортировать их в объекты
- Простые категории
- Задавать параметрические категории

</text-box>

Очень часто в объектно-ориентированном программировании надо внести в объект набор функций, решающую какой-то стандартизованный набор задач, но эти функции при реализации задач используют инфраструктуру объекта-импортера.  Например, в большинстве семей можно найти родителей (мать, отца), детей, сестер, братьев, дедушек, бабушек, внучатых племянников и другие семейные отношения.  Правила распознавания во всех семьях одинаковы, а состав семей разный.

Разовьем пример из Второго раздела Первой части, и зададим категорию, распознающую семейные отношения между членами семьи.  Конкретную семью будем задавать объектом-инкапсуляцией.

```logtalk
% File: relations_c.lgt

:- category(relations).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2015-09-23,
		comment is 'Семейные отношения.'
	]).

	:- public([
		father/2, mother/2,
		sister/2, brother/2
	]).

	:- public([
		parent/2,
		male/1, female/1
	]).

	father(Father, Child) :-
		::male(Father),
		::parent(Father, Child).

	mother(Mother, Child) :-
		::female(Mother),
		::parent(Mother, Child).

	sister(Sister, Child) :-
		::female(Sister),
		::parent(Parent, Sister),
		::parent(Parent, Child),
		Sister \== Child.

	brother(Brother, Child) :-
		::male(Brother),
		::parent(Parent, Brother),
		::parent(Parent, Child),
		Brother \== Child.

:- end_category.

```

Методы категории ```relations/0``` (да, они тоже могут быть параметризованными) опираются на как минимум ```protected``` предикаты ```male/1```, ```woman/1``` и ```parent/2```.  Скопируем семью из Первой части, удалив наследование методы и импортировав нашу первую категорию.  Не будем выносить на публику состав семьи.

```logtalk
% File: family_pat3.lgt
:- object(family_pat3,
      imports(relations)).

   :- protected([
      male/1,   % мужчина(<кто>)
      female/1, % женщина(<кто>)
      parent/2  % родитель(<кто>,<чей>)
   ]).

   male(max).

   female(pat).
   female(jill).

   parent(max, jill).
   parent(pat, jill).

:- end_object.
```

Троведем несколько тестов.

<sample-output>

?- *family_pat3::father(Father, Child).*
Father = max,
Child = jill *.*

?- *family_pat3::mother(Mother, Child).*
Mother = pat,
Child = jill *;*
<b class="red">false.</b>

?- *family_pat3::sister(Sister, Child).*
<b class="red">false.</b>

?- *family_pat3::brother(Brother, Child).*
<b class="red">false.</b>

</sample-output>

Семья маленькая, поэтому нет братьев и сестер.  Здесь видно, что, если в теле категории используется метод, не реализованный в категории и в объекте-импортере, результат будет отказ (fail, false, no).

Другим примером категории является самостоятельно реализованная система журналирования.

```logtalk
:- category(logging).

	:- public([
		init_log/0,
		add_log_entry/1,
		print_log/0,
        % публичный доступ к записям журнала
		log_entry/2
	]).

	% таблица записей журнала
	:- private(log_/2).
	:- dynamic(log_/2).

    % используется для представления времени события в журнале
	:- op(600, xfy, (:)).

	% retractall/1 удаляет фразы в "this",
    % т.е. в объекте, импортирующем категорию
	init_log :-
		retractall(log_(_, _)),
		add_log_entry(start).

	% assertz/1 добавляет фразы в "this",
    % т.е. в объекте, импортирующем категорию
	add_log_entry(Entry) :-
        get_time(TimeStamp),
        stamp_date_time(TimeStamp,
            date(Year, Month, Day,
                Hours, Mins, Secs,
                _,_,_), 'UTC'),
		assertz(log_(Year/Month/Day-Hours:Mins:Secs, Entry)).

	% log_/2 является приватным динамическим предикатом;
    % он вызывается в "this",
	% т.е. в объекте, импортирующем категорию
	print_log :-
		log_(Date, Entry),
		write(Date), write(' - '), write(Entry), nl,
		fail.
	print_log.

	log_entry(Date, Entry) :-
		log_(Date, Entry).

:- end_category.
```

Далее приведем пример объекта, импортирующего категорию журнала, и автоматически инициализирующего его.

```logtalk
:- object(object_with_logging,
	imports(logging)).

    % следующие две цели инициализации эквивалентны, поскольку директивы
    % инициализации/1 в "self" и "this" - это одно и то же:

    % ищет сообщение init_log/0 в "self"
	%:- initialization(::init_log).

    % ищет сообщение init_log/0 в "this"
	:- initialization(^^init_log).

:- end_object.
```

Вообще не обязательно свои объекты, использующие категорию журнала, наследовать от ```object_with_logging/0```, но в данном случае рассмотрения примеров это удобно.  Разработаем объект, тестирующи, чтоб внутри семьи выполнялись базовые свойства, например, не должно быть человека обоих полов.  Тестирование, однако, тоже повторяющаяся задача, оформим его в категорию тоже.

```logtalk
:- category(testing).

   :- protected(test/3).
   :- mode(test(+symbol, +atom, +goal), zero_or_more).
   :- info(test/3,[
      comment is 'Задает тест (test case)',
      argnames is ['TestName', 'TestType', 'TestGoal']
   ]).


   :- public(run/0).
   :- protected([
      apply_test/3
   ]).

   :- meta_predicate(apply_test(*, 0, *)).

   run :-
      forall(
        ::test(Name, Type, Goal),
        (
          apply_test(Type, Goal, Result),
          ::record_result(Name, Type, Result)
        )
      ), !.

   run. % всегда true.

   apply_test(true, Goal, 'Pass') :-
      call(Goal), !.

   apply_test(true, _, 'Fail') :- !.

   apply_test(fail, Goal, 'Fail') :-
      call(Goal), !.

   apply_test(fail, _, 'Pass') :- !.

   apply_test(Type, _, 'UnknownType'(Type)):-!.

   % record_result реализуется в
   % объекте, содержащем тесты.

:- end_category.
```

И вот теперь "соберем" наш объект для тестирования из разных объектов и категорий.  Такой вариант объектно-ориентированного программирования называется *композиционный* (*compositional*): объект, точнее *компонента*, собирается из отдельных объектов при помощи  механизмов наследования, импорта, реализации недостающих методов.  *Компоненты* - это объекты, *оснащающие* (*provide*) некоторый *интерфейс* (*interface*).  Компонентами в Logtalk выступают объекты-инкапсуляции, категории, экземпляры классов, классы, метаклассы.  Последние три вида объектов мы еще не изучали.   *Интерфейс* компоненты задается при помощи *протоколов*, изученных нами в Первой части.

```logtalk
:- object(test_family,
      extends([family_pat3,
               object_with_logging]),
      imports(testing)).

   test(pat_is_woman,
      true,
      ::female(pat)
   ).

   test(males_are_not_females,
      fail,
      (::female(Human), ::male(Human))
   ).

   test(unknown_test,
      unknown,
      true
   ).

   :- protected(record_result/3).
   record_result(Name, Type, Result) :-
      format(atom(Entry),
         'Test "~w" (~w) results in "~w"',
         [Name, Type, Result]),
      ^^add_log_entry(Entry).

   run :-
      ^^run,
      ^^print_log.

:- end_object.
```

Проведем тесты, посмотрим, что получилось.

<sample-output>

?- *test_family::run.*
2025/5/10-17:4:26.075670003 - start
2025/5/10-17:4:29.986160039 - Test "pat_is_woman" (true) results in "Pass"
2025/5/10-17:4:29.986389636 - Test "males_are_not_females" (fail) results in "Pass"
2025/5/10-17:4:29.986438751 - Test "unknown_test" (unknown) results in "UnknownType(unknown)"
true.

</sample-output>

Все тесты выполнены успешно.  Наступило время подробнее изучить инструменты Logtalk, использующиеся для указания какие именно методы вызываются и из каких родительских сущностей (объектов и категорий).

## Методы ```self/1``` и ```this/1```


В заключение этого раздела еще одно наблюдение: категории - это сущности, являющиеся полными противоположностями протоколам.  Протоколы задают сигнатуры методам, которые объекты и категории должны реализовать.  Категории, наоборот, предназначены только для реализации повторяющихся задач средствами объекта, в который она импортируется.  Также не стоит полагать, что категории Logtalk обладают фундаментальными свойствами категорий, предмета изучения теории категорий.  Идея категорий заимствована из языка Objective-C, также у нее много общего с Python-классами *mixing* (*подмешиваний*), не вносящими дополнительных полей при множественном наследовании.
