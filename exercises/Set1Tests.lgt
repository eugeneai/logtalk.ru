

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
        each_test_name(horse_fact_defined(Horse)),
        explain(::error("Не все лошади заданы в объекте '~w'" + [_O_]))
       ],
       (_O_<<horse(Horse))).

   test(butsy_is_a_pet, true,
       [condition(success(basic_predicates_defined)),
        explain(::error("В объекте '~w' надо задать, что каждая кошка (cat/1) - это домашнее животное (pet/1), используя ::/1." +
        [_O_]))],
       (catch(_O_::pet(butsy),
            error(existence_error(procedure, _), _),
            fail))).

   test(flash_is_a_pet, true,
       [condition(success(basic_predicates_defined)),
        explain(::error("В объекте '~w' надо задать, что каждая собака (dog/1) - это домашнее животное (pet/1), используя ::/1." +

        [_O_]))],
       (
         catch(_O_::pet(flash),
               error(existence_error(procedure, _), _),
               fail))).

   test(not_a_horse_i_a_pet, fail,
       [condition(success(horses_facts_are_defined)),
        explain(::error("Где такое в постановке задачи указано, что лошади были б домашними животными? (смотрите pet/1 внимательно в объекте '~w').\nВы можете не согласиться с утвержением, что лошади не являются домашними животными, мы тоже не согласны. Но задание - есть звдвние. Сделаем согласно заданию!" +
        [_O_]))],
       ( _O_<<horse(Horse),
         catch(
               _O_::pet(Horse),
               error(existence_error(procedure, _), _),
               fail),!)).

   test(horses_are_animals,
       all(::mem(Horse, [star, iron])),
       [condition(success(predicate_defined(horse/1))),
        each_test_name(horse_is_a_animal(Horse)),
        explain(::error("В объекте '~w' не известно, что лошади (horse/1) - это животные (animal/1)! Используйте ::horse(..). " + [_O_]))
       ],
       (_O_<<horse(Horse),
        catch(
               _O_::animal(Horse),
               error(existence_error(procedure, _), _),
               fail),!)).

   test(animals_inheritance,
       all(::mem(Pet, [flash, butsy])),
       [condition(success(basic_predicates_defined)),
        each_test_name(is_an_animal(Pet)),
        each_explain(::error("В объекте '~w' не известно, что '~w' - это животные (animal/1)! Вероятно забыто обращение к унсладованному определению animal/1. Создайте правило, связывающее обновленное правило с унаследованным из '~w'. Подсказака: используйте в теле правила ^^animal(..). " + [_O_, Pet, _Fourth_]))
       ],
       (catch(
          _O_::animal(Pet),
          error(existence_error(procedure, _), _),
          fail),!)).

:- end_object.


:- object(test_problem_6(_O_, _Fifth_),
     extends(studyunit),
     imports(object_exists_and_predicates_c(_O_, [owner/2 - public]))).

   test_name('Задача 6').
   test_type(problem).

   test(A,B,C,D) :- ^^test(A,B,C,D).

   test(sixth_extends_fifth, true,
        [condition(success(basic_object_exists))],
        test_extending(_O_, _Fifth_)::ok).

   test(horse_owned_by_bob,
       all(::mem(Horse, [star])),
       [condition(success(predicate_defined(owner/2))),
        each_test_name(bob_owns_horse(Horse)),
        each_explain(::error("В объекте '~w' не известно, что лошадь (horse/1) '~w' принадлежит Бобу (bob). Не забываем про посылку сообщений при помощи ::! (::horse(..))." + [_O_, Horse]))
       ],
       (
        catch(
          _O_::owner(bob, Horse),
          error(existence_error(procedure, _), _),
          fail),!)).

   test(horse_not_owned_by_bob,
       none(::mem(Horse, [iron])),
       [condition(success(predicate_defined(owner/2))),
        each_test_name(bob_doesnt_own_horse(Horse)),
        each_explain(::error("Откуда вы взяли, что Боб (bob) владеет '~w' (в объекте '~w'), в задании такого нет, ;-) ! Не забываем, что в заголовке правил использование констант - это обычное дело, например, owner(ann,X) :- ..." + [Horse, _O_]))
       ],
       (
        catch(
          _O_::owner(bob, Horse),
          error(existence_error(procedure, _), _),
          fail),!)).

   test(ann_owns_some,
       none(::mem(Animal, [iron, star, butsy, flash])),
       [condition(success(predicate_defined(owner/2))),
        each_test_name(ann_doesnt_own(Animal)),
        each_explain(::error("Откуда взялась Аня (ann), и что она владеет '~w' (в объекте '~w'), в задании такого нет, ;-) ! Вероятно имеет место преступный копипаст из одной из подсказок тестов!" + [Animal, _O_]))
       ],
       (
        catch(
          _O_::owner(ann, Animal),
          error(existence_error(procedure, _), _),
          fail),!)).

   test(kate_owns_pets,
       all(::mem(Animal, [butsy, flash])),
       [condition(success(predicate_defined(owner/2))),
        each_test_name(kate_owns_pets(Animal)),
        each_explain(::error("Катя (kate) должна влыдеть '~w' (залается в объекте '~w'), однако такого не наблюдаю!" +
        [Animal, _O_]))
       ],
       (
        catch(
          _O_::owner(kate, Animal),
          error(existence_error(procedure, _), _),
          fail),!)).

   test(kate_doesnt_own_some,
       none(::mem(Animal, [star, iron])),
       [condition(success(predicate_defined(owner/2))),
        each_test_name(kate_owns_pets(Animal)),
        each_explain(::error("Катя (kate) не владеет '~w' (залается в объекте '~w')! Слишком много ответственности на Кате! Проверьте определение правил с заголовком owner(kete, ...) :- ... !" +
        [Animal, _O_]))
       ],
       (
        catch(
          _O_::owner(kate, Animal),
          error(existence_error(procedure, _), _),
          fail),!)).

   test(humans_own_humans,
       none(
           (
             Humans = [kate, bob, ann], !,
             ::mem(Human1, Humans), ::mem(Human2, Humans))),
       [condition(success(predicate_defined(owner/2))),
        each_test_name(human_owns_human(Human1, Human2)),
        each_explain(::error("Привет! Докатились до того, что люди владеют людьми! Откуда взялась утвержение, что '~w' (человек в данном контексте) владеет '~w' (тоже человек) в объекте '~w'? Рабовладение далеко в прошлом!" +
        [Human1, Human2, _O_]))
       ],
       (
        catch(
          _O_::owner(Human1, Human2),
          error(existence_error(procedure, _), _),
          fail),!)).
:- end_object.

:- object(test_problem_7(_O_, _World_),
     extends(studyunit),
     imports(object_exists_and_predicates_c(_O_, [print/1 - public]))).

   test_name('Задача 7').
   test_type(problem).

   test(A,B,C,D) :- ^^test(A,B,C,D).


   test(printing_owners_and_pets,
       all(::mem(Owner-Animal, [kate-flash, kate-butsy, bob-star])),
       [condition(success(predicate_defined(print/1))),
        each_test_name(owner_animal_printed(Owner-Animal)),
        each_explain(::error("Вывод сообщения ~w::print(~w) не включает строку '~w:~w'!" +
        [_O_, _World_, Owner, Animal]))
       ],
       (
         catch(
          (
             with_output_to(
               string(Out),
               _O_::print(_World_)
             )
             ,
             format(atom(SubString), "~w:~w", [Owner, Animal]),
             ( ::output_substring(Out, SubString) -> true ;
               ::info("Ваш объект '~w' выдал: ~q" + [_O_, Out]), fail )
          ),
          error(existence_error(procedure, _), _),
         fail), !)).

   % :- use_module(library(lists), [length/2]).

   test(printing_rows, true,
       [condition(success(printing_owners_and_pets)),
        explain(::error("Вывод сообщения ~w::print(~w) надо разбить на три строки! Подсказка: escape-симвод перевода строки - '\\n' ." +
        [_O_, _World_]))
       ],
       (
         catch(
          (
             with_output_to(
               string(Out),
               _O_::print(_World_)
             )
             ,
             %::info("str: ~w\n" - [Out]),
             split_string(Out, "\n", "", L),
             % L=[Out],
             %::info("List: ~w\n" - [L]),
             length(L, 4)
          ),
          error(existence_error(procedure, _), _),
          % dummy,
         fail), !)).

:- end_object.

:- object(test_problem_8(_Cat_, _Dog_),
     extends(studyunit),
     imports(objects_exist_c([_Cat_, _Dog_]))).

   test_name('Задача 8').
   test_type(problem).

   test(A,B,C,D) :- ^^test(A,B,C,D).

   test(animal_objects_define_say,
        all(::mem(Object, [_Cat_, _Dog_])),
        [
          each_test_name(animal_object_define_say(Object)),
          each_condition(success(basic_object_exist(Object))),
          each_explain(
            ::error("В существующем объекте '~w' не задан метод say/1, определяющий, что говорит животное!" + [Object]))
        ],
        (catch(
          (
           Object<<say(_),!
          ),
          error(existence_error(procedure, _), _),
          fail
        ),!)).

   test(animals_use_really_say,
        all(::mem(Object, [_Cat_, _Dog_])),
        [
          each_test_name(animal_object_use_really_say(Object)),
          each_condition(success(animal_object_define_say(Object))),
          each_explain(
             ::error("Объект '~w' не может ответить на сообщение ::say_something (~w::say_something)! Во время исполнения запроса возникает исключение 'Не найден метод'. Вероятно идет речь о том, что say(Something) используется в ::say_something без ::, т.е. надо проверить посылается ли сообщение say/1. Разница между использованием в теле правила say(Something) и ::say(Something) в том, что первый вариант обращается к предикату say/1, определенному локально в объекте, а ::say(Something) посылает сообщение себе и объектам-наследниками. Чтобы объекты-наследники могли отвечать на say/1, он [say/1] должен быть декларирован как protected или public (не private)." + [Object, Object]))
        ],
        (catch(
          catch(
             with_output_to(
                string(_),
                Object::say_something
             ),
             error(permission_error(access,private_predicate,say/1), _),
             true
          ),
          error(existence_error(procedure, _), context(_,_)),
          fail
        ),!)).

   test(animals_can_say_aloud,
        all(::mem(Object, [_Cat_, _Dog_])),
        [
          each_test_name(animal_object_can_say_aloud(Object)),
          each_condition(success(animal_object_use_really_say(Object))),
          each_explain(
             ::error("Объект '~w' не может ответить на сообщение ::say_something (~w::say_something)! Объект '~w' пытается запросить ::say(Something) (say/1), но у '~w' нет доступа к нему (private или вообще без декларации). Надо проверить чтобы в '~w' или в унаследованных объектах был определен public или protected say/1." + [Object, Object, Object, Object, Object]))
        ],
        (catch(
          (
             with_output_to(
                string(_),
                Object::say_something
             )
          ),
          error(permission_error(access,private_predicate,say/1), _),
          fail
        ),!)).

   test(animals_objects_say_aloud_correctly,
        all(::mem(Object, [_Cat_, _Dog_])),
        [
          each_test_name(animal_object_say_aloud(Object)),
          each_condition(success(animal_object_can_say_aloud(Object))),
          each_explain(
             ::error("Объект '~w' не отвечает на сообщение ::say_something (~w::say_something) согласно заданию! Надо проверить чтобы в '~w' или в унаследованных объектах был определен public say_something, печатающий сообщение согласно заданию." + [Object, Object, Object]))
        ],
        (catch(
          (
             with_output_to(
                string(S),
                Object::say_something
             ),
             Object<<say(Said),
             format(string(SS), "~w!!!\n",[Said]),
             (  sub_string(S,_,_,_,SS)
             -> true
             ;
                ::error("Объект '~w' сгенерировал ~q, а должно быть ~q" + [Object, S, SS]),
                fail)
          ),
          error(existence_error(procedure, _), _),
          fail
        ),!)).

:- end_object.

:- object(test_problem_9(_Taylor_),
   extends(studyunit),
   imports(object_exists_and_predicates_c(
     _Taylor_, [fact/2 - private, sqr/2 - private, exp/3 - public]))).

   test_name("Задача 9").
   test_type(problem).

   test(A,B,C,D) :- ^^test(A,B,C,D).

   test(exp_3_can_run, true,
        [condition(success(basic_predicates_defined)),
        explain(::error("При выполнении запроса ~w::exp/3 возникла проблема." + [_Taylor_]))],
        catch(
           _Taylor_::exp(0, 10, _),
           Except,
           (
              ::error("Произошла исключение при пробном запуске exp/3: ~q." + Except),
              fail,!
           )
        )
   ).

   test(exp_of_0_is_0,
     all(between(0, 20, Number)),
     [
       each_test_name(exp_of_o_at(Number, Value)),
       each_condition(success(exp_3_can_run)),
       explain(
          (
             _Taylor_::exp(0, 10, Value),
             ::error("Объект '~w' с методом exp/3 при сообщении ~w::exp(0, N, Value). Value сильно отличается от 1.0. Например для 0, 10, ~w." +
             [_Taylor_, _Taylor_, Value])
          )
       )
     ],
     (
       _Taylor_::exp(0, Number, Value),
       abs(Value-1.0) < 0.0001
     )
   ).

   test(exp_of_1_converges_to_e_on_number,
     all(between(1, 20, Number)),
     [
       each_test_name(exp_of_1_at(Number, Value)),
       each_condition(success(exp_3_can_run)),
       explain(
          (
             _Taylor_::exp(0, 10, Value),
             ::error("Объект '~w' с методом exp/3 при сообщении ~w::exp(1, N, Value). Value сильно отличается от 1.0. Например для 1, 10, ~w." +
             [_Taylor_, _Taylor_, Value])
          )
       )
     ],
     (
       E = 2.7182818284590452353602874713527,
       _Taylor_::exp(1, Number, Value1),
       N1 is Number + 1,
       _Taylor_::exp(1, N1, Value2),
       abs(E - Value1) >= abs(E - Value2)
     )
   ).

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

   test(6-sixth_has_owners_defined, true,
       [condition(success(5-fifth_has_animal_defined_and_horses))],
       test_problem_6(sixth, fifth)::ok).

   test(7-table_animal_printer_prints_table, true,
       [condition(success(6-sixth_has_owners_defined))],
       test_problem_7(table_animal_printer, sixth)::ok).

   test(8-animals_speaking, true,
       [],
       test_problem_8(cat_object, dog_object)::ok).

   test(9-taylor_sequence, true,
       [],
       test_problem_9(taylor)::ok).

:- end_object.
