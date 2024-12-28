
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
      (::has_failed -> format("Task failed. ");
       ::has_skipped-> format("Task partially done. ");
       format("Task done. ")),
      format("Tests ended.\n").
      % ::count(N),!,
      % ::print(N).

   :-public(ok/0).
   ok :-
      ::run,
      ::all_succeeded.

   :- uses(logtalk, [
      print_message(error, studyunit, Message) as err(Message)
   ]).

:- end_object.

:- object(test_object(_O_),
   extends(studyunit)).

   test(object_exists, true,
       [explain(::error("Вы не создали объект:\n:- create_object(~w,...).\n % . . . \n:- end_object." +
        [_O_]))],
       ( current_object(_O_) )
   ).

:- end_object.

:- object(test_predicates_defined(_O_, _Predicates_),
   extends(studyunit)).

   test(predicate_defined(Pred),
       all(::mem(Pred - Scope, _Predicates_)),
       [
         condition(current_object(_O_)),
         each_explain(
           ::error("В объекте '~w' надо задекларировать '~w' предикат '~w'\n  :- ~w(~w)." + [_O_, Scope, Pred, Scope, Pred])),
         explain(
           ::error("В объекте '~w' надо сделать необходимые декларации предикатов" + [_O_]))
       ], ( ::check(Pred, Scope) ) ).

   :- use_module(library(lists), [member/2]).
   :- protected(check/2).
   check(Pred, Scope) :-
      object_property(_O_, declares(Pred, Props)),
      member(Scope, Props).

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
