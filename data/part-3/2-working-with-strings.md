---
path: '/part-3/2-combine-parametric-objects'
title: 'Комбинирование параметрических объектов'
hidden: false
---

<text-box variant='learningObjectives' name="Цель освоения материала">

Изучив этот раздел вы научитесь

- Создавать параметрические объекты, состоящие из других объектов
- Обрабатывать "наборы" объектов с одинаковой сигнатурой в одном так называемом *proxy*-запросе
- Разбивать реализацию объектов в зависимости от структурных ограничений параметров

</text-box>

## Комбинирование параметрических объектов

Из объектов, которые являются термами, простыми и сложными, можно строить еще более сложные композиции, моделирующими, очевидно, объекты предметной области, напрример, графы-деревья, однонаправленные списки и т.п.

В данном примере решается задача композиции объекта, представляющего рекомендации одежды по сезону и спича (тоста, локлада, лекции и т.п.) по определенному поводу.

```logtalk
:- object(dress(_Season)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura, Evgeny Cherkashin',
		date is 2024-02-17,
		comment is 'Советы по одежде в зависимости от сезона.',
		parnames is ['Season']
	]).

	:- public(clothes/1).

    % Выбор одежды по сезону
	clothes(Clothes) :-
		parameter(1, Season),
		clothes(Season, Clothes).

	clothes(winter, [pants, sleeves, heavy]).
	clothes(spring, [shorts, sleeves, light]).
	clothes(summer, [shorts, light, white]).
	clothes(autumn, [pants, sleeves, light]).

:- end_object.
```

Второй объект - представления спича.

```logtalk
:- object(speech(_Event)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura, Evgeny Cherkashin',
		date is 2024-02-17,
		comment is 'Речевые советы варианта спича по событию (поводу).',
		parnames is ['Event']
	]).

	:- public(speech/1).

	speech(Speech) :-
		parameter(1, Event),
		speech(Event, Speech).

	speech(wedding, [happy, jokes]).
	speech(inauguration, [formal, long]).

:- end_object.

```

Третий объект - комбинация рекомендателей одежды и спича.

```logtalk
:- object(speech(_Season_, _Event_)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Mora, Evgeny Cherkashin',
		date is 2024-08-14,
		comment is 'Рекомендации по спичу и одежде в зависимости от сезона и события.',
		parnames is ['Season', 'Event']
	]).

	:- public(advice/0).
	advice :-
        ::advice(Clothes, Speech),
		write('Clothes: '), write(Clothes), nl,
		write('Speech:  '), write(Speech), nl, nl.

	:- public(advice/2).
	advice(Clothes, Speech) :-
		_Season_::clothes(Clothes),
		_Event_::speech(Speech).

:- end_object.
```

<sample-output>

?- *dress(summer)::clothes(Clothes).*
Clothes = [shorts, light, white].

?- *speech(dress(summer), speech(wedding))::advice(Clothes, Speech).*
Clothes = [shorts, light, white],
Speech = [happy, jokes].

</sample-output>

С первого взгляда качется, что все усложняется: надо создавать сложный объект, потом запрос делать.  Но это только с первого взгляда. На практике такие конструкции формируются в результате решения других задач, используются при помощи ссылки их переменной.  На всякий случай еще пример - множественное наследование без "усложнения" структуры.


```logtalk
:- object(speech2(Season, Event),
	extends((dress(Season), speech(Event)))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura, Evgeny Cherkashin',
		date is 2024-08-14,
		comment is 'Speech and dress advice according to the season and the event.',
		parnames is ['Season', 'Event']
	]).

	:- public(advice/0).
	advice :-
        ::advice(Clothes, Speech),
		write('Clothes: '), write(Clothes), nl,
		write('Speech:  '), write(Speech), nl, nl.

	:- public(advice/2).
	advice(Clothes, Speech) :-
		^^clothes(Clothes),
		^^speech(Speech).

:- end_object.
```
<sample-output>

?- speech2(summer, wedding)::advice(Clothes, Speech).
Clothes = [shorts, light, white],
Speech = [happy, jokes].

</sample-output>

С такими объектами проще работать в статическом случае, когда не надо во время исполнения программы комбинировать оъекты.  Другой вариант эффективного применения множественного наследования - адаптер-фасад, объект реализующий "простой" интерфейс к интерфейсам сложного набора объектов некоторой библиотеки.

Одним из интересных направлений использования кобинированных объектов - это моделирование документов для качественной печати на примнтере или в PDF.  Отдельные элементы документов представляются в виде параметричеких объектов, например, различные названия организации и реквизиты, руководство организацией, заинтересованные лица, форма представления заголовков, данные контрагентов, семантические и синтаксические структуры предмета документа и так далее.

## Объекты-заместители

Рассмотрим случай, где задан набор фактов о людях и сотрудников из предыдущего раздела.

```logtalk
person('Donald Trump', 71).
person('Joseph Biden', 82).
person('Barak Obama', 69).

employee('Ilon Musk', 58, 1_000_000).
employee('Steve Bulmer', 68, 100_000).
employee('John Doe', 60, 10_000).
```

Теперь нам надо к каждому выполнить один и тот же запрос, вроде этого:

<sample-output>
$ *swilgt -s person.lgt -s proxy_emp.lgt* # грузим два файла подряд

% [ /home/eugeneai/tmp/tst/speech.lgt loaded ]
% (0 warnings)
% [ /home/eugeneai/tmp/tst/proxy_emp.lgt loaded ]
% (0 warnings)
Welcome to SWI-Prolog (threaded, 64 bits, version 9.2.9)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- *forall(person(Name, Age), (person(Name, Age)::grow_older(NewPerson),
   format('~w.\n', [NewPerson]))).*
person(Donald Trump,72).
person(Joseph Biden,83).
person(Barak Obama,70).
true.

</sample-output>

Здесь важен одни момент - первый аргумет ```forall/2``` - это цель Prolog/Logtalk. С этим знанием рассмотрим следующий запрос:

<sample-output>

?- *{person(Name, Age)}::grow_older(NewPerson).*
Name = 'Donald Trump',
Age = 71,
NewPerson = person('Donald Trump', 72) ;
Name = 'Joseph Biden',
Age = 82,
NewPerson = person('Joseph Biden', 83) ;
Name = 'Barak Obama',
Age = 69,
NewPerson = person('Barak Obama', 70).

</sample-output>

В запросе использована структура вида ```..., {<прокси-структура>}::<сообщение>, ...```, обеспечивающая доступ к *прокси-объектам*.

```logtalk
% параметрические прокси-объекты person/2
person('Donald Trump', 71).
person('Joseph Biden', 82).
person('Barak Obama', 69).

% параметрические прокси-объекты employee/3
employee('Ilon Musk', 58, 1_000_000).
employee('Steve Bulmer', 68, 100_000).
employee('John Doe', 60, 10_000).
```

Прокси-объекты (заместители [терма, определенного в виде объекта]) можно хранить в базе данных, выводить из правил, то есть хранить различные конкретизации параметров.  Вышеприведенная структура запроса позволяет элегантно делать запросы к этим объектам.   Вариант с круглыми скобками так же работает.

<sample-output>

?- *{employee(Name, Age, Salary)}::(grow_older(NewPerson),
    give_raise(1_000,NewEmployee)).*
Name = 'Ilon Musk',
Age = 58,
Salary = 1000000,
NewPerson = employee('Ilon Musk', 59, 1000000),
NewEmployee = employee('Ilon Musk', 58, 1001000) ;
Name = 'Steve Bulmer',
Age = 68,
Salary = 100000,
NewPerson = employee('Steve Bulmer', 69, 100000),
NewEmployee = employee('Steve Bulmer', 68, 101000) ;
Name = 'John Doe',
Age = 60,
Salary = 10000,
NewPerson = employee('John Doe', 61, 10000),
NewEmployee = employee('John Doe', 60, 11000).

</sample-output>

Продолжая пример с документами, перечень документов для печати можно представить при помощи прокси-объектов. Еще раз напомним, что терм-выражение фигурных скобках - это атомарная цель, ее можно выводить отдельно, например, делать запрос к базе данных документов, синтезировать терм, затем печатать его.

## Параметрические объекты - варианты

Определения параметрических объектов можно распределять по отдельныйм объектом ("разносить"), причем используя наследование между вариантами.  Следующий пример иллюстрирует, как связать набор предикатов со сложным термом - списком, причем для разных альтернатив задается свой объект.

Распознавание непустого списка, первого варианта структурных ограничений параметра параметрического объекта, - удачная унификация с термом ```[_ | _]```.

**Замечание** Следующий код не работает, пока  не работает.

```logtalk
:- object([_| _]).

	:- public(last/1).
	:- mode(last(?term), zero_or_one).

	:- public(length/1).
	:- mode(length(?term), zero_or_one).

	:- public(member/1).
	:- mode(member(?term), zero_or_more).

	:- public(nextto/2).
	:- mode(nextto(?term, ?term), zero_or_more).

	length(Count) :-
		this([_Head| Tail]),
		Tail::length(TL),
        Count is TL + 1.

	last(Last) :-
		this([_ | Tail]),
		Tail::last(Last).

	member(Element) :-
		this([Element| _]).

    member(Element) :-
		this([_ | List]),
		List::member(Element).

	nextto(X, Y) :-
		this([X, Y| _]).

	nextto(X, Y) :-
		this([_| Tail]),
        Tail::nextto(X,Y).

:- end_object.
```

Рассмотрим второй вариант аргумента - пустой список, частный случай непустого.  При использовании наследования серез расширение надо исключить неправильные варианты для пустых списков.  Пустой список является атомом, а не сложным термом, поэтому методы из ```'[_ | _]'``` в ```[]``` всегда будет ложными, то есть невыводимыми, так как неприменимы к пустому списку, за исключением метода ```length/1```.

```logtalk
:- object('[]',
	extends([[_| _]])).

    % переопределим унаследованные предикаты, чтобы
    % решать задачу над пустыми списками.

    length(0).

	last(_) :-
		fail.

	member(_) :-
		fail.

	nextto(_, _) :-
		fail.

:- end_object.
```

Потестируем наш распределенный объект.

<sample-output>

?- *'[]'::length(L).*
L = 0.

?- *[]::length(L).*
<b class="red">!     Type error: expected object_identifier but got []
!       in goal: []::length(A)
!       with execution context:
!         entity:            user
!         sender:            user
!         this:              user
!         self:              []
!         meta-call context: []
!         coinduction stack: []
!     </b>
?-

</sample-output>


<!---
A quiz to review the contents of this section:

<quiz id="6bfa7eab-80de-52e2-afe5-285af914099f"></quiz>
-->
