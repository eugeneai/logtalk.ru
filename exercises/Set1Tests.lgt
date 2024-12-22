

:- object(note_lgtunit,
   extends(lgtunit)).

   :- protected(score/2).
   score(Num, V) :-
       retractall(score_(Num,_)),
       assertz(score_(Num, V)).

   :- dynamic(score_/2).
   :- protected(clear_scores/0).
   clearscores:-
       retractall(score_(_,_)).

   :- public(print/1).
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

:- end_object.

:- object(stub_tests,
   extends(lgtunit),
   imports(explain_c)).
:- end_object.

:- object(test_object(_O_),
   extends(lgtunit),
   imports(explain_c)).

   succeeds(object_exists) :-
       current_object(_O_).

   explain(object_not_defined,
        "Вы не создали объект:\n:- create_object(~w,...).\n % . . . \n:- end_object.",
        [_O_]):- \+ current_object(_O_).

:- end_object.

:- object(test_animals(_O_),
   extends(lgtunit),
   imports(explain_c)).

   succeeds(butsy_is_a_cat) :- _O_::cat(butsy).
   succeeds(flash_is_a_dog) :- _O_::dog(flash).
   explain(flash_is_not_a_dog,
       "В объекте '~w' должна быть задана собака (dog/1) 'flash'",
       [_O_]):-
           \+ _O_::dog(flash).
   explain(butsy_is_not_a_cat,
       "В объекте '~w' должна быть задана кошка (cat/1) 'butsy'",
       [_O_]):-
           \+ _O_::dog(flash).

   explain(not_a_cat,
       "В объекте '~w' нет ни одной кошки (cat/1)",
       [_O_]):- \+ _O_::cat(_).

   explain(not_a_dog,
       "В объекте '~w' нет ни одной собаки (dog/1)",
       [_O_]):- \+ _O_::dog(_).

:- end_object.

:- object(test_predicates_defined(_O_, _Predicates_),
   extends(lgtunit),
   imports(explain_c)).

   succeeds(predicates_defined_test) :-
       ::predicates_defined.

   % :- uses(logtalk, [
   %    object_property(_O_, Property) as has_prop(Property)
   % ]).
   :- use_module(lists, [member/2]).
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

   explain(should_have_predicate(Pred),
      "В объекте '~w' надо задекларировать '~w' предикат '~w'\n  :- ~w([~w,...]).",
      [_O_, Scope, Pred, Scope, Pred]) :-
          member(Pred - Scope, _Predicates_),
          \+ check(Pred, Scope).

:- end_object.

:- object(test_extending(_O_, _Parent_),
   extends(lgtunit),
   imports(explain_c)).

   ok :-
      extends_object(_O_, _Parent_).

   succeeds(object_exstends_object) :-
      ok.

   explain(object_estends_object(_O_,_Parent_),
      "Надо сделать так, чтобы объект '~w' был унаследован от '~w'.\n:- object(~w, extends(~w))",
      [_O_,_Parent_,_O_,_Parent_]) :- \+ ok.

:- end_object.

:- object(test_animals_inference(_O_),
   extends(lgtunit),
   imports(explain_c)).

   succeeds(x_is_a_cat_and_animal) :- _O_::cat(X), _O_::animal(X).
   succeeds(x_is_a_dog_and_animal) :- _O_::dog(X), _O_::animal(X).

   ok:-
      _O_::dog(X), _O_::animal(X),
      _O_::cat(Y), _O_::animal(Y).

   explain(being_a_cat_not_an_animal(X),
       "В объекте '~w' надо задать, что каждая кошка (cat/1) - это животное (animal/1).",
       [_O_]) :- _O_::cat(X), \+ _O_::animal(X).
   explain(being_a_dog_not_an_animal(X),
       "В объекте '~w' надо задать, что каждая собака (cat/1) - это животное (animal/1).",
       [_O_]) :- _O_::dog(X), \+ _O_::animal(X).
:- end_object.

:- object(test_animals_call(_O_, _Animals_),
   extends(lgtunit),
   imports(explain_c)).

   :- use_module(lists, [member/2]).

   :- protected(test_prop/1).
   test_prop(Animal) :- _O_::animal(Animal).

   :- protected(check_list/1).
   check_list([]).
   check_list([H|T]):-
      ::test_prop(H), !,
      check_list(T).

   succeeds(all_objects_have_property) :- ok.
   ok:-
      check_list(_Animals_).

   explain(is_animal(Animal),
      "В объекте '~w' не известно, что '~w' - это животное. \nПроверьте наличие деклараций dog/1, cat/1  в объекте 'third'\nПроверьте наличие правила animal/1 в объекте 'fourth'.",
      [_O_, Animal]) :-
          member(Animal, _Animals_),
          \+ _O_::animal(Animal).

:- end_object.

:- object(test_animals_call_fifth(_O_, _Animals_),
   extends(test_animals_call(_O_, _Animals_))).

   :- use_module(lists, [member/2]).
   explain(is_horse_is_animal(Animal),
      "В объекте '~w' не известно, что '~w' - это животное. \nПроверьте наличие деклараций horse/1 в объекте '~w'\nРавило 'лошади тоже животные' есть ли?",
      [_O_, Animal, _O_]) :-
          member(Animal, _Animals_),
          \+ _O_::animal(Animal).

   explain(lack_of_inheritance(X),
      "Вероятно забыт в объекте '~w' вызов унаследованного из объекта 'fourth' animal/1 при помощи оператора ^^/1.\nПричина: в 'fourth' известно, что '~w' - животное, а в '~w' - нет!",
      [_O_, X, _O_]) :-
        fourth::animal(X), \+ _O_::animal(X).

:- end_object.

:- object(test_pet_call(_O_, _Animals_),
   extends(test_animals_call(_O_, _Animals_))).

   :- use_module(lists, [member/2]).
   explain(is_pet(Animal),
      "В объекте '~w' не известно, что '~w' - это домашнее животное (pet/1). \nПроверьте наличие деклараци правила pet/1 в объекте '~w'",
      [_O_, Animal, _O_]) :-
          member(Animal, _Animals_),
          \+ _O_::pet(Animal).

   test_prop(Animal) :- _O_::pet(Animal).

:- end_object.


:- object(test_owners(_O_),
   extends(lgtunit),
   imports(explain_c)).

   :- use_module(lists, [list_to_set/2, subtract/3]).
   succeeds(kate_owns_all_pets) :- ok(kate, [butsy, flash]).
   succeeds(bob_owns_star) :- ok(bob, [star]).

   :- protected(ok/2).
   ok(kate, Animals):-
      findall(X, (_O_::pet(X), _O_::owner(kate, X)), L),
      list_to_set(L, S1),
      list_to_set(Animals, S2),
      subtract(S2, S1, []).

   ok(bob, [X]):-
      _O_::owner(bob, X).

   ok:-
      ok(kate, [butsy, flash]),
      ok(bob, [star]).

   explain(kate_must_own(Animal),
      "В объекте '~w' Kate (kate) должна владеть '~w'. Задано ли правило, что kate владеет всеми домашними животными? ",[_O_, Animal]) :-
      _O_::pet(Animal), \+ _O_::owner(kate, Animal).

   explain(bob_owns_star(star),
      "В объекте '~w' Bob (bob) должен владеть '~w'. Задано ли правило, что bob владеет лошадью 'star'? ",
      [_O_, star]) :- \+ _O_::owner(bob, star).


:- end_object.

:- object(tests,
   extends(studyunit),
   imports([explain_c])).

   count(10).

   succeeds(1-first_has_cat_and_dog_correct) :-
       Predicates = [dog/1 - public, cat/1 - public],
       ::runexp([
           test_object(first)
         , current_object(first)-test_predicates_defined(first, Predicates)
         , (test_predicates_defined(first, Predicates)::predicates_defined) - test_animals(first)
         , (::score(1, 1)) - stub_tests
       ]).

   succeeds(2-second_has_animal_defined) :-
       Predicates = [animal/1 - public],
       ::runexp([
           test_object(second)
         , current_object(second) - test_extending(second, first)
         , (test_extending(second, first)::ok) -
              test_predicates_defined(second, Predicates)
         , (test_predicates_defined(second, Predicates)::predicates_defined) -
              test_animals_inference(second)
         , (test_animals_inference(second)::ok) - stub_tests
         , (::score(2, 1)) - stub_tests
       ]).

   succeeds(3-third_has_cat_and_dog_protected) :-
       Predicates = [dog/1 - protected, cat/1 - protected],
       O = third,
       ::runexp([
           test_object(O)
         , current_object(O)-test_predicates_defined(O, Predicates)
         % , (test_predicates_defined(O, Predicates)::predicates_defined) - test_animals1(third_test)
         , (test_predicates_defined(O, Predicates)::predicates_defined) - stub_tests
         , (::score(3, 1)) - stub_tests
       ]).

   succeeds(4-fourth_has_animal_defined) :-
       Predicates = [animal/1 - public],
       O = fourth,
       ::runexp([
           test_object(O)
         , current_object(O) - test_extending(O, third)
         , (test_extending(O, third)::ok) -
              test_predicates_defined(O, Predicates)
         , (test_predicates_defined(O, Predicates)::predicates_defined) -
              test_animals_call(O, [butsy, flash])
         , (test_animals_call(O, [butsy, flash])::ok) - stub_tests
         , (::score(4, 1)) - stub_tests
       ]).

   succeeds(5-fifth_has_animal_defined_and_horses) :-
       Predicates = [horse/1 - protected, pet/1 - public],
       O = fifth,
       ::runexp([
           test_object(O)
         , current_object(O) - test_extending(O, fourth)
         , (test_extending(O, fourth)::ok) -
              test_predicates_defined(O, Predicates)
         , (test_predicates_defined(O, Predicates)::predicates_defined) -
              test_animals_call_fifth(O, [butsy, flash, star, iron])
         , (test_animals_call_fifth(O, [butsy, flash, star, iron])::ok) - test_pet_call(O, [butsy, flash])
         , (test_pet_call(O, [butsy, flash])::ok) - stub_tests
         , (::score(5, 1)) - stub_tests
       ]).

   rr:- true.

   test(6-sixth_owners, true, [note(bar(seconds-Time))]) :-
       Predicates = [owner/2 - public],
       O = sixth,
       ::runexp([
           test_object(O)
         , (test_extending(O, fifth)::ok) -
              test_predicates_defined(O, Predicates)
         , (test_predicates_defined(O, Predicates)::predicates_defined) -
              test_owners(O)
         , (test_owners(O)::ok) - stub_tests
         , (::score(6, 1)) - stub_tests
       ]).
:- end_object.
