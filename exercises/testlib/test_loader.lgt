:- object(test_loader).

   :- public(load_and_run/1).
   load_and_run(TestObjects) :-
       logtalk_load(TestObjects),
       forall(
           member(TestObject, TestObjects),
           (   TestObject::run,
               TestObject::print
           )
       ).

   :- public(load_and_run/2).
   load_and_run(TestObjects, SpecificTests) :-
       logtalk_load(TestObjects),
       forall(
           member(TestObject, TestObjects),
           TestObject::run_selected(SpecificTests)
       ).

   :- public(batch_run/1).
   batch_run(ConfigFile) :-
       open(ConfigFile, read, Stream),
       read_term(Stream, Tests, []),
       close(Stream),
       load_and_run(Tests).

:- end_object.
