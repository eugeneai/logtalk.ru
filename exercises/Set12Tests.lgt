






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

   test(prover_test, true,
       [],
       (ip_zero_test::ok)).

:- end_object.
