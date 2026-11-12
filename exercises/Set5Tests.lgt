:- object(test_set5,
   extends(studyunit)).

   :- use_module(library(lists), [member/2]).

   test_name('Набор 5 - Классы и экземпляры').
   test_type(problem_set).

   test(A, B, C, D) :- ^^test(A, B, C, D).

   test(counter_is_class, true,
      [ explain(::error('counter должен быть классом: instantiates(class), specializes(object).')) ],
      ( current_object(counter),
        instantiates_class(counter, class),
        specializes_class(counter, object) )).

   test(counter_api, true,
      [ condition(success(counter_is_class)),
        explain(::error('В counter должны быть public value/1 и increment/0.')) ],
      ( object_property(counter, public(CounterPredicates)),
        lists:member(value/1, CounterPredicates),
        lists:member(increment/0, CounterPredicates) )).

   test(counter_starts_at_zero, true,
      [ condition(success(counter_api)),
        explain(::error('Новый экземпляр counter должен иметь значение 0.')) ],
      ( catch(abolish_object(set5_counter_1), _, true),
        counter::new(set5_counter_1),
        set5_counter_1::value(0),
        catch(abolish_object(set5_counter_1), _, true) )).

   test(counter_increment, true,
      [ condition(success(counter_api)),
        explain(::error('После increment/0 значение counter должно увеличиться на 1.')) ],
      ( catch(abolish_object(set5_counter_2), _, true),
        counter::new(set5_counter_2),
        set5_counter_2::value(0),
        set5_counter_2::increment,
        set5_counter_2::value(1),
        catch(abolish_object(set5_counter_2), _, true) )).

   test(counter_instances_are_independent, true,
      [ condition(success(counter_increment)),
        explain(::error('Состояние экземпляров counter не должно разделяться.')) ],
      ( catch(abolish_object(set5_counter_a), _, true),
        catch(abolish_object(set5_counter_b), _, true),
        counter::new(set5_counter_a),
        counter::new(set5_counter_b),
        set5_counter_a::increment,
        set5_counter_a::value(1),
        set5_counter_b::value(0),
        catch(abolish_object(set5_counter_a), _, true),
        catch(abolish_object(set5_counter_b), _, true) )).

   test(step_counter_is_class, true,
      [ condition(success(counter_is_class)),
        explain(::error('step_counter должен специализировать counter и иметь public increment_by/1.')) ],
      ( current_object(step_counter),
        specializes_class(step_counter, counter),
        object_property(step_counter, public(StepPredicates)),
        lists:member(increment_by/1, StepPredicates) )).

   test(step_counter_increment_by, true,
      [ condition(success(step_counter_is_class)),
        explain(::error('increment_by/1 должен увеличивать счётчик на заданное число.')) ],
      ( catch(abolish_object(set5_step_counter), _, true),
        step_counter::new(set5_step_counter),
        set5_step_counter::value(0),
        set5_step_counter::increment_by(5),
        set5_step_counter::value(5),
        set5_step_counter::increment,
        set5_step_counter::value(6),
        catch(abolish_object(set5_step_counter), _, true) )).

   test(person_is_class, true,
      [ explain(::error('person должен быть классом и иметь public name/1 и greet/1.')) ],
      ( current_object(person),
        instantiates_class(person, class),
        specializes_class(person, object),
        object_property(person, public(PersonPredicates)),
        lists:member(name/1, PersonPredicates),
        lists:member(greet/1, PersonPredicates) )).

   test(alice_is_static_instance, true,
      [ condition(success(person_is_class)),
        explain(::error('alice должен быть статическим экземпляром person с именем alice.')) ],
      ( current_object(alice),
        instantiates_class(alice, person),
        alice::name(alice),
        alice::greet('Hello, alice!') )).

   test(person_dynamic_instance, true,
      [ condition(success(person_is_class)),
        explain(::error('Динамический экземпляр person должен принимать имя и возвращать приветствие.')) ],
      ( catch(abolish_object(set5_bob), _, true),
        person::new(set5_bob),
        set5_bob::set_name(bob),
        set5_bob::name(bob),
        set5_bob::greet('Hello, bob!'),
        catch(abolish_object(set5_bob), _, true) )).

   test(registry_api, true,
      [ explain(::error('registry должен иметь public add/1, all/1, remove/1 и count/1.')) ],
      ( current_object(registry),
        object_property(registry, public(RegistryPredicates)),
        lists:member(add/1, RegistryPredicates),
        lists:member(all/1, RegistryPredicates),
        lists:member(remove/1, RegistryPredicates),
        lists:member(count/1, RegistryPredicates) )).

   test(registry_add_all_count, true,
      [ condition(success(registry_api)),
        explain(::error('registry должен добавлять, перечислять и удалять экземпляры без дубликатов.')) ],
      ( registry::clear,
        catch(abolish_object(set5_reg_bob), _, true),
        person::new(set5_reg_bob),
        set5_reg_bob::set_name(bob),
        registry::add(alice),
        registry::add(set5_reg_bob),
        registry::add(alice),
        registry::all(Persons),
        lists:member(alice, Persons),
        lists:member(set5_reg_bob, Persons),
        registry::count(2),
        registry::remove(alice),
        registry::count(1),
        registry::clear,
        catch(abolish_object(set5_reg_bob), _, true) )).

   test(factory_api, true,
      [ explain(::error('instance_factory должен иметь public spawn/2 и destroy/1.')) ],
      ( current_object(instance_factory),
        object_property(instance_factory, public(FactoryPredicates)),
        lists:member(spawn/2, FactoryPredicates),
        lists:member(destroy/1, FactoryPredicates) )).

   test(factory_spawn_destroy, true,
      [ condition(success(factory_api)),
        explain(::error('spawn/2 должен создавать экземпляр, а destroy/1 — уничтожать его.')) ],
      ( catch(abolish_object(set5_person_1), _, true),
        instance_factory::spawn(person, set5_person_1),
        current_object(set5_person_1),
        instantiates_class(set5_person_1, person),
        instance_factory::destroy(set5_person_1),
        \+ current_object(set5_person_1) )).

   test(factory_spawn_counter, true,
      [ condition(success(factory_api)),
        explain(::error('Через instance_factory должен создаваться рабочий экземпляр counter.')) ],
      ( catch(abolish_object(set5_counter_3), _, true),
        instance_factory::spawn(counter, set5_counter_3),
        set5_counter_3::value(0),
        set5_counter_3::increment,
        set5_counter_3::value(1),
        instance_factory::destroy(set5_counter_3) )).


:- end_object.

:- object(tests,
   extends(studyunit)).

   test_name('Практическая работа 5 - Классы и экземпляры').
   test_type(problem_set).

   run :-
       test_set5::run.

:- end_object.
