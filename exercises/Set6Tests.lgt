:- object(test_set6,
   extends(studyunit)).

   test_name('Набор 6 - Саморефлексия и генераторы экземпляров').
   test_type(problem_set).

   test(A, B, C, D) :- ^^test(A, B, C, D).

   test(all_objects, true,
      [ explain(::error('inspector::all_objects/1 должен возвращать список загруженных объектов.')) ],
      ( inspector::all_objects(Objects),
        lists:member(class, Objects),
        lists:member(item, Objects),
        lists:member(inspector, Objects) )).

   test(public_predicates, true,
      [ condition(success(all_objects)),
        explain(::error('inspector::public_predicates/2 должен возвращать public-предикаты объекта.')) ],
      ( inspector::public_predicates(inspector, Predicates),
        lists:member(all_objects/1, Predicates),
        lists:member(public_predicates/2, Predicates),
        lists:member(object_kind/2, Predicates) )).

   test(object_kind, true,
      [ condition(success(all_objects)),
        explain(::error('object_kind/2 должен различать prototype, class и instance.')) ],
      ( inspector::object_kind(item, class),
        inspector::object_kind(prototype_item, prototype),
        catch(abolish_object(set6_instance), _, true),
        class::new(set6_instance),
        inspector::object_kind(set6_instance, instance),
        catch(abolish_object(set6_instance), _, true) )).

   test(factory_api, true,
      [ explain(::error('object_factory должен иметь public spawn/2 и destroy/1.')) ],
      ( current_object(object_factory),
        object_property(object_factory, public(FactoryPredicates)),
        lists:member(spawn/2, FactoryPredicates),
        lists:member(destroy/1, FactoryPredicates) )).

   test(factory_spawn_destroy, true,
      [ condition(success(factory_api)),
        explain(::error('spawn/2 должен создавать экземпляр, а destroy/1 — уничтожать его.')) ],
      ( catch(abolish_object(set6_factory_item), _, true),
        object_factory::spawn(item, set6_factory_item),
        current_object(set6_factory_item),
        instantiates_class(set6_factory_item, item),
        object_factory::destroy(set6_factory_item),
        \+ current_object(set6_factory_item) )).

   test(generator_make_items, true,
      [ explain(::error('generator::make_items/2 должен создавать N экземпляров item с именами generated_item_N.')) ],
      ( catch(abolish_object(generated_item_1), _, true),
        catch(abolish_object(generated_item_2), _, true),
        catch(abolish_object(generated_item_3), _, true),
        generator::make_items(3, Items),
        length(Items, 3),
        Items = [generated_item_1, generated_item_2, generated_item_3],
        forall(lists:member(Item, Items), ( current_object(Item), instantiates_class(Item, item) )),
        catch(abolish_object(generated_item_1), _, true),
        catch(abolish_object(generated_item_2), _, true),
        catch(abolish_object(generated_item_3), _, true) )).

   test(compiler_flags, true,
      [ explain(::error('compiler::flag/2 и set_flag/2 должны читать и менять флаги Logtalk.')) ],
      ( compiler::flag(report, Original),
        compiler::set_flag(report, off),
        compiler::flag(report, off),
        compiler::set_flag(report, Original),
        compiler::flag(report, Original) )).

:- end_object.

:- object(tests,
   extends(studyunit)).

   test_name('Практическая работа 6 - Саморефлексия и генераторы').
   test_type(problem_set).

   run :-
       test_set6::run.

:- end_object.
