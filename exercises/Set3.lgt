
% Задача этой практической работы состоит в освоении
% необычного феномена в ООП - параметрических объектов
% Logtalk - язык программирования, который, в первую
% очередь является макропакетом для Prolog, т.е.
% можно проектировать ООП-программы "в статике".

% -----------------------------------------------------
% Упражнение 2: Создание параметрического объекта.
% Задача - спроектировать объекты, задающие геометрические
% фигуры: circle, rectangle. Заголовки уже есть, надо
% определить public-методы area/1 (площадь) и
% perimeter/1 (периметр). Заодно давайте сделаем

% -----------------------------------------------------
% Упражнение 1: Сформулируем протокол parameter_p для этих
% public-методов и зададим в объектах, что они реализуют
% этот протокол.

:- protocol(parameter_p).
   :- public(area/1).
   :- public(perimeter/1).
:- end_protocol.

% Проложение упражнения 2.
:- object(circle(_Radius_),
   implements(parameter_p)).

   area(Sq) :-
     Sq is _Radius_ * _Radius_ * 3.14.

   perimeter(Per) :-
     Per is _Radius_ * 2 * 3.14.

:- end_object.

:- object(rectangle(_Width_, _Height_),
   implements(parameter_p)).

   area(Sq) :-
     _Width_ * _Height_.

   perimeter(Per) :-
     Per is (_Width_ + _Height_) * 2.

:- end_object.

% Вот тестовый объект для фигур
% ?- test_fig_objects::run.

:- object(test_fig_objects,
   extends(studyunit)).

   test_name('Тест объектов - геометрических фигур').
   test_type(problem).

   test(figure_parameters,
      all((::object_list(L),::mem(Obj-q(Sq, Per)))),
       [],
       ((Eps = 0.001,
         Obj::area(Sq1),
         Obj::perimeter(Per1),
         abs(Sq1 - Sq) < Eps,
         abs(Per1 - Per) < Eps
         ))).

   :- protected(object_list/1).
   object_list([
          circle(2)-q(12.5663, 12.5663),
          rectangle(1,2)-q(2, 6)
        ]).

:- end_object.

% -----------------------------------------------------
% Упражнение 3: Теперь надо унаследовать от rectangle(_,_)
% объект square(_) и прогнать следующий тест.

:- object(square(_Width_),
   extends(rectangle(_Width_, _Width_))).

:- end_object.

% ?- test_fig_objects_ex::run.

:- object(test_fig_objects_ex,
   extends(test_fig_objects)).

   object_list(
      [
        square(2) - a(4, 8)
      |L]) :-
      ::object_list(L).

:- end_object.

% -----------------------------------------------------
% Упражнение 4: Теперь создадим объект 'figures', содержащий
% proxy-предикаты с нашими фигурами. Задача - создать
% объект, задать по одной-две фигуры и реализовать
% методы из протокола parameter_p для 'figures'.

:- object(figures,
   implements(parameter_p)).

   circle(2).
   circle(3).

   rectangle(2,4).
   rectangle(4,5).

   square(2).
   square(8).

   area(Sq) :-
      findall(S, {circle(_)}::area(S), Sc),
      findall(S, {rectangle(_,_)}::area(S), Sr),
      findall(S, {square(_)}::area(S), Ss),
      sum_list(Sc, S1), sum_list(Sr, S2),
      sum_list(Ss, S3),
      Sq is S1 + S2 + S3.

   perimeter(Sq) :-
      findall(S, {circle(_)}::perimeter(S), Sc),
      findall(S, {rectangle(_,_)}::perimeter(S), Sr),
      findall(S, {square(_)}::perimeter(S), Ss),
      sum_list(Sc, S1), sum_list(Sr, S2),
      sum_list(Ss, S3),
      Sq is S1 + S2 + S3.

   sum_list([],0).
   sum_list([X|T], S) :-
      sum_list(T, S1),
      S ia S1 + X.

:- end_object.

% -----------------------------------------------------
% Упражнение 5: Конечный детерминированный автомат.
% Надо разработать абстрактный параметрический объект - конечный
% детерминированный автомат 'automaton', параметр -
% начальное состояние.
% Переходы и выходы задаются protected-предикатом q/4.
% В automaton спроектировать public-предикат model/3, моделирующий
% переход из состояния в состояние, анализируя входную
% последовательность символов. Если автомат не принимает
% входную последовательность, то model/3 полжен быть
% ложным (fail).

:- object(automaton(_InitialState_)).
   :- protected(q/4).
   :- mode(q(+symbol, +symbol, +symbol, +symbol)).
   :- info(q/4, [
      comment is 'Задает переход из одного состояния, имея на входе символ, выдавая на выход другой символ',
      argnames is ['CurState', 'InputSymbol','NewState','OutputSymbol']
   ]).

   :- public(model/3).
   :- mode(model(?list[?symbol], ?list[?symbol], ?symbol)).
   :- info(model/2, [
      comment is 'Моделирует переход из состояния в состояние соглавно входному потоку и генерирует выходной',
      argnames  is ['InputSybolList', 'OutputSymbolList', 'TerminateState']
   ]).

   model(I, O, S) :-
     ::model(_InitialState_, I, O, S).

   :- protected(model/4).

   model(S, [], [], S). % Нет вхдоных символов.
   model(S, [X|T],[Y|L], Q) :-
      ::q(S,X, Y,Q1),
      model(Q1, T,L, Q).

:- end_object.

% -----------------------------------------------------
% Упражнение 6: Программирование конкретного автомата
% Мили (mealy)
% Автомат, принимающий число с плавающей запятой
% в инженерном формате


% -----------------------------------------------------
% Упражнение 7: Генератор последовательностей, принимаемых
% автоматом. Продолжение Задачи 6

% -----------------------------------------------------
% Упражнение 8: Автомата для транслирования регулярного
% выражения. Пример - безскобочная запись арифметического
% выражения из целых чисел.

% -----------------------------------------------------
% Упражнение 9: Разработка транслятора технического
% английского языка.

% Грамматика языка:
% 1. <предположение> ::= <группа существительного> <глагольная группа>
% 2. <группа существительного> ::= <детерминант> <существительное>
% 3. <глагольная группа> ::= <глагол> <группа существительного>
% 4. <детерминант> ::= a | an | the | my
% 5. <существительное> ::= cow | tail
% 6. <глагол> ::= walks | shakes

% Сделаем сначала объект с перыми тремя синтаксическими
% структурами - 'parser', с двумя параметрами: лексика -
% перечень детерминантов, существительных и глаголов,
% предложение на синтаксический анализ.

% Пусть все предикаты, реализующие синтаксический анализ
% будут трехаргументными: Входной поток лексем, выходной
% и результат трансляции, например, дерево синтаксического
% разбора.

:- object(parser(_Lexic_)).
   :- protected(sentence/3).    % <предложение>
   :- protected(nonun_group/3). % <группа существительного>
   :- protected(verb_group/3).  % <глагольная группа>
   :- protected(determinant/3).
   :- protected(noun/3).
   :- protected(verb/3).
   :- public(parse/2).   % транслирует Sentence в структуру

   parse(Sentence, Struct) :-
      ::sentence(Sentence, [], Struct).

   sentence(I, O, sent(NG, VG)) :-
      ::noun_group(I, R, NG),
      ::verb_group(R, O, VG).

   noun_group(I, O, ng(D, N)) :-
      ::determinant(I, R, D),
      ::noun(R, O, N).

   verb_group(I, O, vg(V, NG)) :-
      ::verb(I, R, V),
      ::nonun_group(R, O, NG).

   noun([Word | O], O, noun(Word)) :-
      _Lexic_::noun(Word).

   verb([Word | O], O, verb(Word)) :-
      _Lexic_::noun(Word).

   determinant([Word | O], O, det(Word)) :-
      _Lexic_::determinant(Word).

:- end_object.


% -----------------------------------------------------
% Упражнение 10: Создание лексики языка.
% Задача сформировать лексикон языка - набор слов,
% из которых создается язык. Каждое слово "размечено"
% частью речи.

% Лексика, общая для всех "английских" языков
:- object(gen_lexic).
   :- public(determinant/1).  % Артикли, местоименя
   :- public(noun/1).   % существительные
   :- public(verb/1).   % глаголы

   determinant(a).
   determinant(an).
   determinant(the).
   determinant(my).
   determinant(your).

:- end_object.

% Лексический состав 'cow'-языка
:- object(cow_lexic,
   extends(gen_lexic)).

   noun(cow).
   noun(tail).

   verb(shakes).
   verb(walks).

:- end_object.

% Тестирование cow-языка.

:- object(test_cow_lang,
   extends(studyunit)).

   test_name('Тест cow-языка').
   test_type(problem).

   test(test_cow_shakes_tail, true,
      [],
      ((parser(cow_lexic)::translate([a, cow, shakes, the, tail], Struct),
         format('Sent:~q\n', [Struct])))).
% Добавить сравнение с результатом.
:- end_object.


% -----------------------------------------------------
% Упражнение 11: Использовать синтаксический анализатор
% как генератор предложений.

:- object(cow_lang,
   extends(parser(cow_lexic))).
   :- public(generate/1).

   generate(Sentence) :-
      ::translate(Sentence, _).

:- end_object.

% ?- cow_lang::generate(Sentence).

% -----------------------------------------------------
% Упражнение 12: обработка дерева.
% Балансировка двоичного дерева
