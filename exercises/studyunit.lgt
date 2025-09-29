
:- object(studyunit,
   extends(basicunit)).

   :- info([
        version is 24:12:22,
        autor is 'Evgeny Cherkashin',
        date is 2024-12-22,
        comment is 'A student test case unit test framework'

   :- public(print/1).
   :- mode(print(+list(atom)), one).
   :- info(print/1, [
        comment is "Print results for specific tests",
        argnames is ['TestNames']
      ]).

   print(TestNames) :-
       ::test_name(TaskName),
       forall(
           member(Name, TestNames),
           (   ::test_result(Self, Name, Result),
               format("~w: ~w - ~w~n", [TaskName, Name, Result])
           )
       ).

   :- public(run_selected/1).
   :- mode(run_selected(+list(atom)), one).
   run_selected(TestNames) :-
       ::run_tests(TestNames),
       ::print(TestNames).

   :- public(export_results/1).
   :- mode(export_results(+atom), one).
   export_results(Filename) :-
       open(Filename, write, Stream),
       self(Self),
       forall(
           ::test_result(Self, Name, Result),
           format(Stream, "test_result(~q, ~q, ~q).~n", [Self, Name, Result])
       ),
       close(Stream).

   :- public(import_results/1).
   :- mode(import_results(+atom), one).
   import_results(Filename) :-
       open(Filename, read, Stream),
       repeat,
           read_term(Stream, Term, []),
           (   Term == end_of_file
           ->  !, close(Stream)
           ;   assertz(Term),
               fail
      ).

   :- protected(test_type/1).
   test_type(test).  % or test_type(problem)

   % Улучшенный метод run с дополнительными опциями
   run :-
       ::clear_results,
      ^^run,
      (::test_type(problem) -> ::print;
       ::test_type(problem_set) ->
          ::print,
          ::print_statistics,
          ::info("Тестирование закончено.\n")
          ; true).

   % Альтернативный метод ok с параметрами
   :- public(ok/1).
   :- mode(ok(+list(atom)), one).
   ok(TestNames) :-
       ::run_tests(TestNames),
       \+ ::has_failed.

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

% Новые вспомогательные категории
:- category(test_conditions_c,
   implements(testing_p)).

   :- protected(check_condition/1).
   check_condition(requires(object_exists, Object)) :-
       current_object(Object).
   check_condition(requires(predicate_defined, Object::Predicate)) :-
       functor(Predicate, Name, Arity),
       current_predicate(Object::Name/Arity).
   check_condition(requires(extends, Child-Parent)) :-
       extends_object(Child, Parent).
   check_condition(requires(implements, Object-Protocol)) :-
       implements_protocol(Object, Protocol).

:- end_category.

:- category(test_utilities_c,
   implements(testing_p)).

   :- protected(assert_equals/2).
   assert_equals(Expected, Actual) :-
       (   Expected == Actual
       ->  true
       ;   ::error("Ожидалось: ~w, но получено: ~w" + [Expected, Actual]),
           fail
       ).

   :- protected(assert_not_equals/2).
   assert_not_equals(Expected, Actual) :-
       (   Expected \== Actual
       ->  true
       ;   ::error("Ожидалось различие, но получены одинаковые значения: ~w" + [Expected]),
           fail
       ).

   :- protected(assert_fails/1).
   :- meta_predicate(assert_fails(0)).
   assert_fails(Goal) :-
       (   \+ call(Goal)
       ->  true
       ;   ::error("Ожидалось, что цель потерпит неудачу, но она успешно выполнилась: ~w" + [Goal]),
           fail
       ).

   :- protected(assert_succeeds/1).
   :- meta_predicate(assert_succeeds(0)).
   assert_succeeds(Goal) :-
       (   call(Goal)
       ->  true
       ;   ::error("Ожидалось успешное выполнение цели, но она потерпела неудачу: ~w" + [Goal]),
           fail
       ).

   :- protected(assert_throws/2).
   :- meta_predicate(assert_throws(*, 0)).
   assert_throws(ExpectedError, Goal) :-
       (   catch(Goal, Error, true),
           subsumes_term(ExpectedError, Error)
       ->  true
       ;   (   catch(Goal, Error, true)
           ->  ::error("Ожидалось исключение: ~w, но получено: ~w" + [ExpectedError, Error])
           ;   ::error("Ожидалось исключение: ~w, но цель выполнилась без ошибок: ~w" + [ExpectedError, Goal])
           ),
           fail
       ).

   :- protected(assert_true/1).
   :- meta_predicate(assert_true(0)).
   assert_true(Goal) :-
       (   call(Goal)
       ->  true
       ;   ::error("Ожидалось истинное значение, но цель вернула ложь: ~w" + [Goal]),
           fail
       ).

   :- protected(assert_false/1).
   :- meta_predicate(assert_false(0)).
   assert_false(Goal) :-
       (   \+ call(Goal)
       ->  true
       ;   ::error("Ожидалось ложное значение, но цель вернула истину: ~w" + [Goal]),
           fail
       ).

   :- protected(assert_empty/1).
   assert_empty(List) :-
       (   List == []
       ->  true
       ;   ::error("Ожидался пустой список, но получен: ~w" + [List]),
           fail
       ).

   :- protected(assert_not_empty/1).
   assert_not_empty(List) :-
       (   List \== []
       ->  true
       ;   ::error("Ожидался непустой список, но получен пустой список"),
           fail
       ).

   :- protected(assert_contains/2).
   assert_contains(List, Element) :-
       (   member(Element, List)
       ->  true
       ;   ::error("Ожидалось, что список ~w содержит элемент ~w" + [List, Element]),
           fail
       ).

   :- protected(assert_not_contains/2).
   assert_not_contains(List, Element) :-
       (   \+ member(Element, List)
       ->  true
       ;   ::error("Ожидалось, что список ~w НЕ содержит элемент ~w" + [List, Element]),
           fail
       ).

   :- protected(assert_length/2).
   assert_length(List, ExpectedLength) :-
       (   length(List, ExpectedLength)
       ->  true
       ;   length(List, ActualLength),
           ::error("Ожидалась длина списка: ~w, но фактическая длина: ~w (список: ~w)" +
                   [ExpectedLength, ActualLength, List]),
           fail
       ).

   :- protected(assert_string_contains/2).
   assert_string_contains(String, Substring) :-
       (   sub_string(String, _, _, _, Substring)
       ->  true
       ;   ::error("Ожидалось, что строка '~w' содержит подстроку '~w'" + [String, Substring]),
           fail
       ).

   :- protected(assert_string_not_contains/2).
   assert_string_not_contains(String, Substring) :-
       (   \+ sub_string(String, _, _, _, Substring)
       ->  true
       ;   ::error("Ожидалось, что строка '~w' НЕ содержит подстроку '~w'" + [String, Substring]),
           fail
       ).

   :- protected(assert_instance_of/2).
   assert_instance_of(Object, Protocol) :-
       (   implements_protocol(Object, Protocol)
       ->  true
       ;   ::error("Ожидалось, что объект '~w' реализует протокол '~w'" + [Object, Protocol]),
           fail
       ).

   :- protected(assert_extends/2).
   assert_extends(Child, Parent) :-
       (   extends_object(Child, Parent)
       ->  true
       ;   ::error("Ожидалось, что объект '~w' наследует от '~w'" + [Child, Parent]),
           fail
       ).

   :- protected(assert_object_exists/1).
   assert_object_exists(Object) :-
       (   current_object(Object)
       ->  true
       ;   ::error("Ожидалось, что объект '~w' существует" + [Object]),
           fail
       ).

   :- protected(assert_predicate_defined/2).
   assert_predicate_defined(Object, Predicate) :-
       functor(Predicate, Name, Arity),
       (   current_predicate(Object::Name/Arity)
       ->  true
       ;   ::error("Ожидалось, что в объекте '~w' определен предикат '~w'/~w" + [Object, Name, Arity]),
           fail
       ).

   :- protected(assert_scope/3).
   assert_scope(Object, Predicate, Scope) :-
       functor(Predicate, Name, Arity),
       (   predicate_property(Object::Name/Arity, Scope)
       ->  true
       ;   ::error("Ожидалось, что предикат '~w'/~w в объекте '~w' имеет область видимости '~w'" +
                   [Name, Arity, Object, Scope]),
           fail
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
       all((::mem(Pred - Scopes, _Predicates_), ::mem(Scope, Scopes))),
       [
         condition(success(basic_object_exists)),
         each_explain(
           ::error("В объекте '~w' надо задекларировать '~w' предикат '~w'\n  :- ~w(~w)." +
              [_O_, Scope, Pred, Scope, Pred])),
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
