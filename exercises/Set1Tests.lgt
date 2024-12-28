

:- object(test_problem_1(_O_),
   extends(studyunit)).

   test(basic_object_definition, true,
       [],
       test_object(_O_)::ok).

   test(basic_predicates_defined, true,
       [condition(\+ current_object(_O_))],
       test_predicates_defined(_O_, [dog/1 - public, cat/1 - public])::ok).

   test(facts_on_animals_defined, true,
       [condition(success(basic_predicates_defined)),
        explain(::error("Не все факты о животных заданы правильно в объекте '~w'"+[_O_]))],
        test_animals(_O_)::ok).
:- end_object.

:- object(test_animals(_O_),
   extends(studyunit)).

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
   extends(studyunit)).

   test(basic_object_definition, true,
       [],
       test_object(_O_)::ok).

   test(first_extends_second, true,
        [condition(success(basic_object_definition))],
        test_extending(_O_, _first_)::ok).

   test(basic_predicates_defined, true,
       [condition(success(first_extends_second))],
       test_predicates_defined(_O_, [animal/1 - public])::ok).

   test(x_is_a_cat_and_an_animal, true,
       [condition(success(basic_predicates_defined)),
        explain(::error("В объекте '~w' надо задать, что каждая кошка (cat/1) - это животное (animal/1)." +
        [_O_]))],
       (_O_::cat(X), _O_::animal(X))).

   test(x_is_a_dog_and_an_animal, true,
       [condition(success(basic_predicates_defined)),
        explain(::error("В объекте '~w' надо задать, что каждая собака (cat/1) - это животное (animal/1)." +

        [_O_]))],
       (_O_::dog(X), _O_::animal(X))).

:- end_object.

:- object(test_animals_call(_O_, _Animals_),
   extends(studyunit)).

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

   % count(10).

   test(1-first_has_cat_and_dog_correct, true, [],
       test_problem_1(first)::ok).

   test(2-second_has_animal_defined, true,
       [condition(success(1-first_has_cat_and_dog_correct))],
       test_problem_2(second, first)::ok).

   % succeeds(2-second_has_animal_defined) :-
   %     Predicates = [animal/1 - public],
   %     ::runexp([
   %         test_object(second)
   %       , current_object(second) - test_extending(second, first)
   %       , (test_extending(second, first)::ok) -
   %            test_predicates_defined(second, Predicates)
   %       , (test_predicates_defined(second, Predicates)::predicates_defined) -
   %            test_animals_inference(second)
   %       , (test_animals_inference(second)::ok) - stub_tests
   %       , (::score(2, 1)) - stub_tests
   %     ]).

   % succeeds(3-third_has_cat_and_dog_protected) :-
   %     Predicates = [dog/1 - protected, cat/1 - protected],
   %     O = third,
   %     ::runexp([
   %         test_object(O)
   %       , current_object(O)-test_predicates_defined(O, Predicates)
   %       % , (test_predicates_defined(O, Predicates)::predicates_defined) - test_animals1(third_test)
   %       , (test_predicates_defined(O, Predicates)::predicates_defined) - stub_tests
   %       , (::score(3, 1)) - stub_tests
   %     ]).

   % succeeds(4-fourth_has_animal_defined) :-
   %     Predicates = [animal/1 - public],
   %     O = fourth,
   %     ::runexp([
   %         test_object(O)
   %       , current_object(O) - test_extending(O, third)
   %       , (test_extending(O, third)::ok) -
   %            test_predicates_defined(O, Predicates)
   %       , (test_predicates_defined(O, Predicates)::predicates_defined) -
   %            test_animals_call(O, [butsy, flash])
   %       , (test_animals_call(O, [butsy, flash])::ok) - stub_tests
   %       , (::score(4, 1)) - stub_tests
   %     ]).

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
