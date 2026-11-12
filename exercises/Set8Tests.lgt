:- object(test_set8,
   extends(studyunit)).

   test_name('Набор 8 - Компоненты и адаптеры').
   test_type(problem_set).

   test(A, B, C, D) :- ^^test(A, B, C, D).

   test(text_protocol, true,
      [ explain(::error('Протокол text_p должен объявлять public text/1.')) ],
      ( current_protocol(text_p),
        protocol_property(text_p, public(Predicates)),
        lists:member(text/1, Predicates) )).

   test(plain_text_component, true,
      [ condition(success(text_protocol)),
        explain(::error('plain_text(_Text_) должен реализовывать text_p и возвращать текст.')) ],
      ( plain_text(hello)::text(hello) )).

   test(uppercase_adapter, true,
      [ condition(success(plain_text_component)),
        explain(::error('uppercase_adapter должен переводить текст источника в верхний регистр.')) ],
      ( uppercase_adapter(plain_text(hello))::text('HELLO') )).

   test(prefix_adapter, true,
      [ condition(success(plain_text_component)),
        explain(::error('prefix_adapter должен добавлять префикс к тексту источника.')) ],
      ( prefix_adapter('>> ', plain_text(hello))::text('>> hello') )).

   test(composite_text, true,
      [ condition(success(plain_text_component)),
        explain(::error('composite_text должен склеивать тексты компонентов в заданном порядке.')) ],
      ( composite_text([plain_text(foo), plain_text(bar)])::text(foobar) )).

   test(registry_api, true,
      [ explain(::error('component_registry должен иметь public register/2, get/2 и names/1.')) ],
      ( current_object(component_registry),
        object_property(component_registry, public(Predicates)),
        lists:member(register/2, Predicates),
        lists:member(get/2, Predicates),
        lists:member(names/1, Predicates) )).

   test(registry_operations, true,
      [ condition(success(registry_api)),
        explain(::error('Реестр должен хранить компоненты по имени без дубликатов.')) ],
      ( component_registry::clear,
        component_registry::register(a, plain_text(a)),
        component_registry::register(b, plain_text(b)),
        component_registry::register(a, plain_text(other)),
        component_registry::get(a, Component),
        Component::text(a),
        component_registry::names(Names),
        length(Names, 2),
        lists:member(a, Names),
        lists:member(b, Names),
        component_registry::clear )).

:- end_object.

:- object(tests,
   extends(studyunit)).

   test_name('Практическая работа 8 - Компоненты и адаптеры').
   test_type(problem_set).

   run :-
       test_set8::run.

:- end_object.
