

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
      (test_halg(ucs(ssgh_test))::ok)).

   test(test_astar, true,
      [],
      (test_halg(a_star(ssgh_test))::ok)).

      test(test_astar_on_puzzle, true,
      [],
      (test_puzzle_solver(a_star(game8_ssgh), game8_ssgh)::ok)).
:- end_object.
