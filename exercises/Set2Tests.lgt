

:- object(tests,
   extends(studyunit)).
   debug(20).
   test_name('Тестовый набор по теме динамические предикаты').

   test_type(problem_set).



   test(prover_test, true,
       [],
       (ip_zero_test::ok)).

:- end_object.
