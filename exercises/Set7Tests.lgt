







:- object(tests,
   extends(studyunit)).

   test_type(problem_set).
   test_name('Практическая работа 7. Символьные вычисления').




   % ....

   test(calculus_test_7, true,
      [],
      (calculus_test::ok)
   ).

   test(newton_test_8, true,
      [],
      (newton_test::ok)
   ).

   % .....

:- end_object.
