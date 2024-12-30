

:- object(test_problem_1(_O_),
   extends(studyunit),
   imports(object_exists_and_predicates_c(
     _O_, [dog/1 - public, cat/1 - public]))).

   test_name("Задача 1").
   test_type(problem).

   test(A,B,C,D) :- ^^test(A,B,C,D).

   test(facts_on_animals_defined, true,
       [condition(success(basic_predicates_defined)),
        explain(::error("Не все факты о животных заданы правильно в объекте '~w'"+[_O_]))],
        ( ::test_name(Name),
          test_animals(Name, _O_)::ok )).
:- end_object.

:- object(test_animals(_Name_, _O_),
   extends(studyunit)).

   test_name(_Name_).

   test(flash_is_a_dog, true,
       [ explain(::error("В объекте '~w' должна быть задана собака (dog/1) 'flash'" +
         [_O_]))
       ],
       _O_::dog(flash)).

   test(butsy_is_a_cat, true,
       [ explain(::error("В объекте '~w' должна быть задана кошка (cat/1) 'butsy'" +
         [_O_]))
       ],
       _O_::cat(butsy)).

   test(unknown_dog(Name), fail,
       [
         explain(::error("В объекте '~w' найдена неизвестная собака (dog/1) '~w'" +
         [_O_, Name]))
       ],
       (_O_::dog(Name), Name \= flash)).

   test(unknown_cat(Name), fail,
       [
         explain(::error("В объекте '~w' найдена неизвестная кошка (cat/1) '~w'" +
         [_O_, Name]))
       ],
       (_O_::cat(Name), Name \= butsy)).

:- end_object.

:- object(test_problem_2(_O_, _first_),
   extends(studyunit),
   imports(object_exists_and_predicates_c(_O_, [animal/1 - public]))).

   test_name('Задача 2').
   test_type(problem).

   test(A,B,C,D) :- ^^test(A,B,C,D).

   test(first_extends_second, true,
        [condition(success(basic_object_exists))],
        test_extending(_O_, _first_)::ok).

   test(x_is_a_cat_and_an_animal, true,
       [condition(success(basic_predicates_defined)),
        explain(::error("В объекте '~w' надо задать, что каждая кошка (cat/1) - это животное (animal/1)." +
        [_O_]))],
       (::cat(X), _O_::animal(X))).

   test(x_is_a_dog_and_an_animal, true,
       [condition(success(basic_predicates_defined)),
        explain(::error("В объекте '~w' надо задать, что каждая собака (dog/1) - это животное (animal/1)." +

        [_O_]))],
       (::dog(X), _O_::animal(X))).

   :- protected([dog/1, cat/1]).
   cat(X) :- _O_::cat(X).
   dog(X) :- _O_::dog(X).

:- end_object.


:- object(test_problem_3(_O_),
   extends(studyunit),
   imports(object_exists_and_predicates_c(_O_, [dog/1 - protected, cat/1 - protected]))).

   test_name('Задача 3').
   test_type(problem).

   test(A,B,C,D) :- ^^test(A,B,C,D).

   test(butsy_is_a_cat, true,
       [ condition(success(predicate_defined(cat/1))),
         explain(::error("В объекте '~w' должна быть задана кошка (cat/1) 'butsy'" +
         [_O_]))
       ],
       catch(
         _O_<<cat(butsy),
         error(existence_error(procedure, cat/1), _),
         fail )).

   test(flash_is_a_dog, true,
       [ condition(success(predicate_defined(dog/1))),
         explain(::error("В объекте '~w' должна быть задана собака (dog/1) 'flash'" +
         [_O_]))
       ],
       catch(
         _O_<<dog(flash),
         error(existence_error(procedure, dog/1), _),
         fail )).

:- end_object.

:- object(test_problem_4(_O_, _third_),
     extends(studyunit),
     imports(object_exists_and_predicates_c(_O_, [animal/1 - public]))).

   test_name('Задача 4').
   test_type(problem).

   test(A,B,C,D) :- ^^test(A,B,C,D).

   test(fourth_extends_third, true,
        [condition(success(basic_object_exists))],
        test_extending(_O_, _third_)::ok).

   test(x_is_a_cat_and_an_animal, true,
       [condition(success(basic_predicates_defined)),
        explain(::error("В объекте '~w' надо задать, что каждая кошка (cat/1) - это животное (animal/1), используя ::/1." +
        [_O_]))],
       (::cat(X),
         catch(_O_::animal(X),
            error(existence_error(procedure, _), _),
            fail))).

   test(x_is_a_dog_and_an_animal, true,
       [condition(success(basic_predicates_defined)),
        explain(::error("В объекте '~w' надо задать, что каждая собака (dog/1) - это животное (animal/1), используя ::/1." +

        [_O_]))],
       (::dog(X),
         catch(_O_::animal(X),
               error(existence_error(procedure, _), _),
               fail))).

   :- protected([cat/1, dog/1]).
   cat(X) :- _third_<<cat(X).
   dog(X) :- _third_<<dog(X).

:- end_object.

:- object(test_problem_5(_O_, _Fourth_),
     extends(studyunit),
     imports(object_exists_and_predicates_c(_O_, [horse/1 - protected, pet/1 - public]))).

   test_name('Задача 5').
   test_type(problem).

   test(A,B,C,D) :- ^^test(A,B,C,D).

   test(fifth_extends_fourth, true,
        [condition(success(basic_object_exists))],
        test_extending(_O_, _Fourth_)::ok).

   test(horses_facts_are_defined,
       all(::mem(Horse, [star, iron])),
       [condition(success(predicate_defined(horse/1))),
        each_explain(::error("В объекте '~w' надо задать факт существования лошади '~w' (horse/1)." + [_O_, Horse])),
        each_task_name(horse_fact_defined(Horse)),
        explain(::error("Не все лошади заданы в объекте '~w'" + [_O_]))
       ],
       (_O_<<horse(Horse))).

:- end_object.

:- object(test_owners(_O_),
   extends(studyunit)).

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
   extends(studyunit)).

   test_type(problem_set).

   test_name('Практическая работа 1').

   test(1-first_has_cat_and_dog_correct, true, [],
       test_problem_1(first)::ok).

   test(2-second_has_animal_defined, true,
       [condition(success(1-first_has_cat_and_dog_correct))],
       test_problem_2(second, first)::ok).

   test(3-third_has_cat_and_dog_protected, true,
       [],
       test_problem_3(third)::ok).

   test(4-fourth_has_animal_defined, true,
       [condition(success(3-third_has_cat_and_dog_protected))],
       test_problem_4(fourth, third)::ok).

   test(5-fifth_has_animal_defined_and_horses, true,
       [condition(success(4-fourth_has_animal_defined))],
       test_problem_5(fifth, fourth)::ok).

% -----------------------------------------

   % succeeds(5-fifth_has_animal_defined_and_horses) :-
   %     Predicates = [horse/1 - protected, pet/1 - public],
   %     O = fifth,
   %     ::runexp([
   %         test_object(O)
   %       , current_object(O) - test_extending(O, fourth)
   %       , (test_extending(O, fourth)::ok) -
   %            test_predicates_defined(O, Predicates)
   %       , (test_predicates_defined(O, Predicates)::predicates_defined) -
   %            test_animals_call_fifth(O, [butsy, flash, star, iron])
   %       , (test_animals_call_fifth(O, [butsy, flash, star, iron])::ok) - test_pet_call(O, [butsy, flash])
   %       , (test_pet_call(O, [butsy, flash])::ok) - stub_tests
   %       , (::score(5, 1)) - stub_tests
   %     ]).

   % rr:- true.

   % succeeds(6-sixth_owners) :-
   %     Predicates = [owner/2 - public],
   %     O = sixth,
   %     ::runexp([
   %         test_object(O)
   %       , (test_extending(O, fifth)::ok) -
   %            test_predicates_defined(O, Predicates)
   %       , (test_predicates_defined(O, Predicates)::predicates_defined) -
   %            test_owners(O)
   %       , (test_owners(O)::ok) - stub_tests
   %       , (::score(6, 1)) - stub_tests
   %     ]).
:- end_object.
