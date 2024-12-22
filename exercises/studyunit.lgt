
:- category(explain_c).
   :- protected(explain/2).
   :- meta_predicate(explain(0, 0)).
   explain(Test, Expl) :-
      (
        call(Test) -> true;
        (call(Expl) -> true; true)
      ).
   :- public(explain/0).
   % :- uses([dbg/1]).
   :- uses(logtalk, [
      print_message(error, test_suite, Message) as err(Message)
   ]).
   explain :-
      forall(
          ::explain(Name, Str, Args),
          ( format(atom(A), Str, Args),
            err('~w: ~w'+[Name, A]) ) ).
   :- protected(explain/3).
   :- public(runexp/0).
   runexp :-
     ::run, explain.

   :- private(runexp/2).
   :- meta_predicate(runexp(0,*)).
   runexp(Cond, TestSuite) :-
     call(Cond), !,
     TestSuite::runexp.

   :- private(runexp1/1).
   runexp1(Cond-Body):-!,
     runexp(Cond,Body).

   runexp1(Body):-!,
     runexp(true,Body).

   :- public(runexp/1).
   runexp([]).
   runexp([H|T]):-
     runexp1(H) -> runexp(T);
     true.

   :- public(ok/0).

:- end_category.

:- object(studyunit,
   extends(lgtunit)).

   :- info([
        version is 24:12:22,
        autor is 'Evgeny Cherkashin',
        date is 2024-12-22,
        comment is 'A student test case unit test framework'
      ]).

   :- public(number/1).
   :- mode(number(?integer), one).
   :- info(number/1, [
        comment is "Defines test number in study test case set. ",
        argnames is ['Number']
      ]).

   :- public(count/1).
   :- mode(count(?integer), one).
   :- info(count/1, [
        comment is "Number of test study cases, used in printing resulting info.",
        argnames is ['Number']
      ]).

   :- public(name/1).
   :- mode(name(?atom), one).
   :- info(name/1, [
        comment is "Name of test object assuming one object is devoted to one student problem.",
        argnames is ['Name']
      ]).

   name(Name) :-
      ::number(Name),!.
   name(Name) :-
      self(Name),!.

   :- protected(score/2).
   :- mode(score(?integer, ?integer), zero_or_more).
   :- info(score/2, [
        comment is "Set score for study case Number for Value from {0, 1}. If unset means 0",
        argnames is ['Number', 'Value']
      ]).

   score(Num, V) :-
       retractall(score_(Num,_)),
       assertz(score_(Num, V)).

   :- dynamic(score_/2).
   :- protected(clear_scores/0).
   :- mode(clear_scores, one).
   :- info(clear_scores/0, [
        comment is "Clear score data to 0 (unset)",
        argnames is []
      ]).
   clear_scores:-
       retractall(score_(_,_)).

   :- public(print/1).
   :- mode(print(+integer), one).
   :- info(print/1, [
        comment is "Print scores as 1's and 0's colored.",
        argnames is ['NumberOfTests']
      ]).
   print(MaxNum) :-
       nl,
       forall(between(1,MaxNum, V),
          (N is V div 10,
             (N == 0 -> write(' ') ; write(N)))),
       nl,
       forall(between(1,MaxNum, V),
          (N is V mod 10, write(N))),
       nl,
       forall(between(1,MaxNum, V),
          (score_(V, 1) -> write('\e[1;32m1\e[0m'); write('\e[1;31m0\e[0m'))),
       nl.

   :- public(print/0).
   :- mode(print, zero_or_one).
   :- info(print/0, [
        comment is "Print scores as 1's and 0's colored green and red.",
        argnames is []
      ]).
   print :-
      ::count(N),!,
      ::print(N).

   :- private(test_state/3).
   :- dynamic(test_state/3).
   :- protected(clear_test_state/0).
   clear_test_state:-
      retractall(test_state(_,_,_)).

   :- public(test_passed/2).
   :- mode(test_passed(+atom,+list(+atom)), one).
   :- info(test_passed/2, [
        comment is "Message received about test have passed from ourselves via print_message/3.",
        argnames is ['Test', 'Options']
      ]).
   test_passed(Test, Options):-
      assertz(test_state(pass, Test, Options)).

   :- public(test_failed/2).
   :- mode(test_failed(+atom,+list(+atom)), one).
   :- info(test_failed/2, [
        comment is "Message received about test have been failed from ourselves via print_message/3.",
        argnames is ['Test', 'Options']
      ]).
   test_failed(Test, Options):-
      assertz(test_state(fail, Test, Options)).

   :- public(has_failed_tests/0).
   has_failed_tests :-
      test_state(fail, _, _).

   :- uses(logtalk, [
      print_message(error, studyunit, Message) as err(Message)
   ]).

   :- public(explain/0).
   explain:-
      forall(::test_state(fail, Name, _),
         (explain(Name, Msg, Arguments) ->
           (
              format(atom(S), Msg, Arguments),
              err('~w!'+[A])
           ); true)).

   :- public(explain/3).

   run :-
      ::clear_scores,
      ::clear_test_state,
      ^^run,
      (::has_failed_tests ->
         self(Self),
         ::name(Name),
         err('Найдены неудавшиеся тесты для тест-задания \'~w\' втест-объекте \'~w\', .'+[Name, Self]),
         ::explain ; true).

:- end_object.

:- object(stub_tests,
   extends(lgtunit),
   imports(explain_c)).
:- end_object.

:- category(test_object_c(_O_)).

   succeeds(object_exists) :-
       current_object(_O_).

   explain(not(object_exists(_O_)),
        "Вы не создали объект:\n:- create_object(~w,...).\n % . . . \n:- end_object.",
        [_O_]):- \+ current_object(_O_).

:- end_category.

:- category(test_predicates_defined_c(_O_, _Predicates_)).

   succeeds(predicates_defined_test) :-
       ::predicates_defined.

   :- use_module(library(lists), [member/2]).
   :- public(predicates_defined/0).
   predicates_defined:-
       check(_Predicates_).

   check([]).
   check([Pred - Scope|T]) :-
      check(Pred, Scope), !,
      check(T).

   check(Pred, Scope) :-
      object_property(_O_, declares(Pred, Props)),
      member(Scope, Props).

   explain(not(predicate_defined(_O_, Pred)),
      "В объекте '~w' надо задекларировать '~w' предикат '~w'\n  :- ~w([~w,...]).",
      [_O_, Scope, Pred, Scope, Pred]) :-
          member(Pred - Scope, _Predicates_),
          \+ check(Pred, Scope).

:- end_category.

:- category(test_extending_c(_O_, _Parent_)).

   ok :-
      extends_object(_O_, _Parent_).

   succeeds(object_exstends_object) :-
      ok.

   explain(not(object_extends_object(_O_,_Parent_)),
      "Надо сделать так, чтобы объект '~w' был унаследован от '~w'.\n:- object(~w, extends(~w))",
      [_O_,_Parent_,_O_,_Parent_]) :- \+ ok.

:- end_category.
