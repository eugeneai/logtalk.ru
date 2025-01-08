

:- object(tests,
   extends(studyunit)).

   test_type(problem_set).
   test_name('Практическая работа 6. Планирование действий').

   test(test_alg_depth_first, true,
      [],
      (test_alg(depth_first(labyrinth))::ok)).

   test(test_alg_wo_cycles, true,
      [],
      (test_alg(depth_first_contracted(labyrinth, 10))::ok)).

   test(test_alg_wo_cycles, true,
      [],
      (test_alg(depth_first_iterative(labyrinth, 10))::ok)).

   test(test_alg_breadth_first, true,
      [],
      (test_alg(breadth_first(labyrinth))::ok)).

   test(test_ufs, true,
      [],
      (test_halg(ucs(ssge_test))::ok)).

:- end_object.
