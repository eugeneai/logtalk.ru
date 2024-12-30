
:- object(studyunit,
   extends(basicunit)).

   :- info([
        version is 24:12:22,
        autor is 'Evgeny Cherkashin',
        date is 2024-12-22,
        comment is 'A student test case unit test framework'
      ]).

   % :- public(print/1).
   % :- mode(print(+integer), one).
   % :- info(print/1, [
   %      comment is "Print scores as 1's and 0's colored.",
   %      argnames is ['NumberOfTests']
   %    ]).

   % print(MaxNum) :-
   %     nl,
   %     forall(between(1,MaxNum, V),
   %        (N is V div 10,
   %           (N == 0 -> write(' ') ; write(N)))),
   %     nl,
   %     forall(between(1,MaxNum, V),
   %        (N is V mod 10, write(N))),
   %     nl,
   %     forall(between(1,MaxNum, V),
   %        (score_(V, 1) -> write('\e[1;32m1\e[0m'); write('\e[1;31m0\e[0m'))),
   %     nl.

   :- public(print/0).
   :- mode(print, zero_or_one).
   :- info(print/0, [
        comment is "Print scores as 1's and 0's colored green and red.",
        argnames is []
      ]).
   print :-
      ::test_name(TaskName),!,
      (
        ::has_failed -> ::error("Задача '~w' не решена.\n" - [TaskName]);
        ::has_skipped-> ::error("Часть тестов задачи '~w' пропущено.\n" - [TaskName]);
        ::info("Задача '~w' решена!\n" - [TaskName])
      ).

   :- protected(test_type/1).
   test_type(test).  % or test_type(problem)

   run :-
      ^^run,
      (::test_type(problem) -> ::print;
       ::test_type(problem_set) ->
          ::print,
          ::info("Тестирование закончено.\n")
          ; true).

   :- public(ok/0).
   ok :-
      ::run,
      ::all_succeeded.

   :- uses(logtalk, [
      print_message(error, studyunit, Message) as err(Message)
   ]).

:- end_object.

:- object(test_extending(_O_, _Parent_),
   extends(studyunit)).

   test(object_exstends_object, true,
       [ condition((current_object(_O_), current_object(_Parent_))),
         explain(
          ::error("Надо сделать так, чтобы объект '~w' был унаследован от '~w'.\n:- object(~w, extends(~w))." +
          [_O_,_Parent_,_O_,_Parent_]))],
       extends_object(_O_, _Parent_)).

:- end_object.

:- category(object_exists_c(_O_),
   implements(testing_p)).

   test(basic_object_exists, true,
       [explain(::error("Вы не создали объект:\n:- create_object(~w,...).\n % . . . \n:- end_object." +
        [_O_]))],
       ( current_object(_O_) )
   ).

:- end_category.

:- category(objects_exist_c(_Object_List_),
   implements(testing_p)).

   test(basic_object_exists,
       all(::mem(Object, _Object_List_)),
       [each_explain(::error("Вы не создали объект '~w':\n:- create_object(~w,...).\n % . . . \n:- end_object." +
        [Object, Object])),
        each_test_name(basic_object_exist(Object))],
       ( current_object(Object) )
   ).

:- end_category.

:- category(object_exists_and_predicates_c(
     _O_, _Predicates_),
   extends(object_exists_c(_O_))).

   test(A,B,C,D) :- ^^test(A,B,C,D).

   test(basic_predicates_defined,
       all(::mem(Pred - Scope, _Predicates_)),
       [
         condition(success(basic_object_exists)),
         each_explain(
           ::error("В объекте '~w' надо задекларировать '~w' предикат '~w'\n  :- ~w(~w)." + [_O_, Scope, Pred, Scope, Pred])),
         each_test_name(predicate_defined(Pred)),
         explain(
           ::error("В объекте '~w' надо сделать необходимые декларации предикатов" + [_O_]))
       ], ( ::check(Pred, Scope) ) ).

   :- use_module(library(lists), [member/2]).
   :- protected(check/2).
   check(Pred, Scope) :-
      object_property(_O_, declares(Pred, Props)),
      member(Scope, Props).

:- end_category.

:- category(object_inherits(_O_, _Parent_),
   implements(testing_p)).

:- end_category.
