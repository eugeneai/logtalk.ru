

:- object(test_problem_2(_O_),
   extends(studyunit),
   imports(object_exists_and_predicates_c(
      _O_,
      [
         clause/1 - [protected, dynamic],
         done/3 - [protected, dynamic],
         clear_db/0 - public
      ]
   ))).

   test_name('Задача 1').

   test(A,B,C,D) :- ^^test(A,B,C,D).

:- end_object.

:- object(test_problem_x(_O_),
   extends(studyunit),
   imports(object_exists_and_predicates_c(
      _O_,
      [test_formula/1 - public]
   ))).

   test_name('Задача запуска теста').

   test(A,B,C,D) :- ^^test(A,B,C,D).

   test(ip_zero_test_extends_studyunit, true,
       [condition(success(basic_object_exists))],
       (test_extending(_O_, studyunit)::ok)).

:- end_object.




:- object(tests,
   extends(studyunit)).
   debug(60).
   test_name('Практическая работа 12. Система доказательства теорем.').

   test_type(problem_set).

   test(check_operators,
       all((::mem(op(Prec, Assoc, Op),[
         op(100, fy, ~ ),
         op(110, xfy, & ),
         op(120, xfy, v ),
         op(130, xfy, => ),
         op(140, xfx, <=> )
       ]))),
       [
         each_test_name(operator_defined(Op)),
         each_explain(format('!ERROR:\n!Оператор \'~w\' определен неправильно, приоритет должен быть ~p,\nлевоассативный - yfx,\nправоассоциативный - xfy,\nнеассоциативный - xfx...\n',
         [Op, Prec]))
       ],
       (current_op(Prec, Assoc, Op))).

   test(check_problem_2, true,
       [condition(success(check_operators))],
       (test_problem_2(ip_zero)::ok)).

   test(check_ip_zero_test, true,
        [condition(success(check_problem_2))],
        (test_problem_x(ip_zero_test)::ok)).

   test(prover_test, true,
       [condition(success(check_ip_zero_test))],
       (ip_zero_test::ok)).

:- end_object.
