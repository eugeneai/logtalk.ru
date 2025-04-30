% В этом наборе задач будем изучать возможность изменять состояние
% объектов, примерно как в обычных языках программирования.
% Состояние объекта меняется при помощи динамических предикатов
% и стандартны для языка программирования Prolog.

% Запуск тестирования:
%
% swilgt -l Set12TestsLoader.lgt -g halt
%
% Также можно запускать, при должном уровне завершения практической
% работы:
%
% swilgt -l Set12TestsLoader.lgt
% ?- ip_zero_test::run.
%

% -----------------------------------------------------
% Упражнение 10: Разработать программу-прувер для
% пропозиционального исчисления. Источник: И.Братко
% Язык программирования Prolog для искусственного интеллекта.
% http://lib.ysu.am/open_books/125049.pdf стр. 530 вам в помощь.
% В объекте реализована машина вывода на типовых конфигурациях,
% есть предикаты для обнаружения элемента в дизъюнкте,
% несколько правил реализованы как примеры и т.п.
% Надо реализовать оставшиеся правила, так, чтобы сработал тест.

:- op(100, fy, ~ ).
:- op(110, xfy, & ).
:- op(120, xfy, v ).
:- op(130, xfy, => ).
:- op(140, xfx, <=> ).


:- object(ip_zero).
   :- public(translate/1).
   :- mode(translate(+expression), one).
   :- info(translate/1, [
      comment is 'Выполняет редукцию формул (дизъюнктов) в канонический вид',
      argnames is ['Formula']
   ]).

   translate(F & G) :- !,
      translate(F),
      translate(G).

   translate(Expr) :-
      tr(Expr, Red), !, % Шаг редукции успешен
      translate(Red).

   translate(Clause) :- % Дальнейшая трансформация невозможна.
      assertz(clause(Clause)).

   :- protected(clause/1). % Дизъюнкт
   :- dynamic(clause/1).

   :- protected(tr/2).

   tr(~(~X), X) :-! .
   tr(X=>Y, ~X v Y) :- !.
   tr(X<=>Y, (X=>Y) & (Y=>X)) :- !.
   tr(~(X v Y), ~X & ~Y) :- !.
   tr(~(X & Y), ~X v ~Y) :- !.
   tr(X & Y v Z, (X v Z) & (Y v Z)) :-!.
   tr(X v Y & Z, (X v Y) & (X v Z)) :-!.
   tr(X v Y, X1 v Y) :- tr(X, X1), !.
   tr(X v Y, X v Y1) :- tr(Y, Y1), !.
   tr(~X, ~X1) :- tr(X, X1).

   :- public(list/0).  % Список дизъюнктов (clause/1).
   list :-
     forall(clause(C),
       format('~w\n', [C])).

   :- public(proof/0).
   proof :-
     rule(Name, Action), !,
     format('Используем правило \'~w\'.\n', [Name]),
     (Action == halt ->
        format('Нет противоречия, исходная формула не является теоремой'), true;
      Action == qed ->
        format('Найдено противоречие!\n'), true ;
      proof).

   :- protected(rule/2).
   :- mode(rule(-atom, -atom), one).
   :- info(rule/2, [
      comment is 'Правила, реализующие метод резолюции.',
      argnames is ['Rule name', 'Action']
   ]).

   rule('Find a contradiction'(A, ~A), qed) :-
      clause(A), clause(~A),!.
      %tbd('Правило должно найти два дизьюнкта P и ~P, P может быть формулой.').

   rule('Remove trivially true clause', tuth_remove) :-
      clause(A), remove(P, A, _), remove(~P, A, _), % пример правила.
      !,
      retract(clause(A)).

   rule('Remove double'(C, P, C1), remove_double) :-
      clause(C), remove(P, C, C1), remove(P, C1, _), !,
      retract(clause(C)), assertz(clause(C1)).
      % tbd('Удалить повторения в дизъюнкте').

   % Можно удалять повторения дизъюнктов.
   % rule('Remove copy of a clause', remove_clause_copy) :-
   %    tbd('Удалить абсолютную копию дизъюнкта').

   % *Собственно механизм построения доказательства*
   % Эти правила должны для одного набора дизъюнктов выполняться один раз
   % Т.е. надо запоминать, что было сделано: done(clause1, clause2, literal).
   % Пример:
   rule('Reduce trivial positive literal'(P, C, P, true, C1), reduce_p_literal) :-
      clause(P), clause(C), remove(~P, C, C1), \+ done(P, C, P), !,
      assertz(clause(C1)), assertz(done(P, C, P)).

   rule('Reduce trivial negative literal'(P, C, ~P, true, C1), reduce_not_p_literal) :-
      clause(~P), clause(C), remove(P, C, C1), \+ done(P, C, ~P), !,
      assertz(clause(C1)), assertz(done(P, C, ~P)).
      % tbd('Аналогично предыдущему случаю только литерал отрицательный').

   rule('Reduction'(C1, C2, P, C1R v C2R), reduction) :-
      clause(C1), clause(C2),
      remove(P, C1, C1R),
      remove(~P, C2, C2R),
      \+ done(C1, C2, P), !,
      assertz(clause(C1R v C2R)), assertz(done(C1, C2, P)).
      % tbd('Правило должно найти два дизьюнкта A | ~p | B, C | p | D и создать новый A | B | C | D. A, B, C, D могут быть пустыми').

   % Это правило - последнее, т.е. с наименьшим приоритетом.
   rule('No contradiction', halt). % Невозможно продвинуться дальше - тупик, нет противоречия.

   :- protected(remove/3).
   :- mode(remove(+expression, +expression, +expression), zero_or_one).
   :- info(remove/3, [
      comment is 'Удалить из дизьюнкта литеру/подформулу',
      argnames is ['Litral', 'Clause', 'Clause']
   ]).

   remove(X, X v Y, Y).
   remove(X, Y v X, Y).
   remove(X, Y v Z, Y v Z1) :-
      remove(X, Z, Z1).
   remove(X, Y v Z, Y1 v Z) :-
      remove(X, Y, Y1).

   :- protected(done/3).
   :- dynamic(done/3).
   :- mode(done(?expression, ?expression, ?expression), zero_or_more).
   :- info(done/3, [
      comment is 'Информация об использованных резольвентах',
      argnames is ['Clause', 'Clause', 'Literal']
   ]).

   tbd(Message) :-
      format('~w\n', [Message]).

   :- public(clear_db/0).
   :- info(clear_db/0, [
      comment is 'Удаляет содержимое БД объекта. Нужен для теста.'
   ]).

   clear_db:-
      retractall(clause(_)),
      retractall(done(_,_,_)).

   :- public(proof/2).
   :- info(proof/2, [
      comment is 'Строит доказательство. Используется в тесте',
      argnames is ['ResultTerm','Print?']
   ]).

   :- use_module(library(lists), [member/2]).

   proof(Result, Aloud) :-
     rule(Name, R), !,
     ( Aloud == true ->
        format('Правило:~w\n',[Name]) ; true),
     ( member(R, [qed, halt]) -> Result = R;
      proof(Result, Aloud)).

   :- public(print/0).
   print :-
     forall(clause(C),
       format('clause(~p).\n', [C])),
     forall(done(C1, C2, R),
       format('done(~p, ~p, ~p).\n', [C1, C2, R])).


:- end_object.


% объект-тест.
%
% ?- ip_zero_test::run.
%

:- object(ip_zero_test,
   extends(studyunit)).

   test_name('Тест системы доказательства теорем в пропозициональном исчислении методом резолюции').

   debug_level(0).

   :- public(test_formula/1).

   test(prove_theorems,
      all(::mem(Formula, [
         c=>c,
         (a=>b) & (b=>c) => (a=>c),
         (p=>(q=>r))=>((p=>q)=>(p=>r)),
         ~(((~p v q v c) & (p v q v c) & ~(q v c)))
      ])),
      [
        each_explain(::error('Формула ~q является теоремой, а ваша машина не может это доказать!' +
        [Formula])),
        each_test_name(theorem(Formula))
      ],
      (
        ip_zero_test::test_formula(
        Formula
       ))
   ).

   test(fail_to_prove_neg_c_to_c, fail,
      [explain(::error('Формула ~q точно не теорема, а ваша машина ее доказывает!' +
       [~(c=>c)]))],
      ip_zero_test::test_formula(
        ~(c=>c)
      )
   ).

   test_formula(F) :-
     ::debug_level(DL),
     (DL>0 -> Aloud = true; Aloud = false),
     ip_zero::clear_db,
     ( Aloud == true -> format('Formula: ~q.\n', [F]); true),
     ip_zero::translate(~F),
     ( Aloud == true -> ip_zero::print; true),
     ::debug(1,"Proof:\n"-[]),
     ip_zero::proof(qed, Aloud),
     ::debug(1,"End of inference.\n"-[]),
     ( Aloud == true -> ip_zero::print; true),
     ::debug(1,"End of cycle.\n"-[]).


:- end_object.
