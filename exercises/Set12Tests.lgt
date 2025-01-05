

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

:- op(100, fy, ~ ).
:- op(110, xfy, & ).
:- op(120, xfy, v ).
:- op(130, xfy, => ).
:- op(140, xfx, <=> ).

:- object(test_ex_3,
   extends(studyunit)).

   test_name('Задача 3. Правила преобразования.').

   test(rule_test,
       all((
         ::mem(c(F,RF),
           [
             c( ~(~a), a),
             c( a=>b, ~a v b),
             c( a<=>b, (a=>b) & (b=>a)),
             c( ~(a v b), (~a & ~b)),
             c( ~(a & b), ~a v ~b),
             c( a & b v c, (a v c) & (b v c)),
             c( b v c & a, (b v c) & (b v a)),
             c( ~(~a) v b, a v b ),
             c( a v (~(~b)), a v b),
             c( ~(b v (~(~a))), ~(b v a))
           ]
         ))),
       [
         each_test_name(reduction(F,RF)),
         each_explain(
           format('!ERROR:\n!Тест преобразования формулы ~q в ~q провален!\n', [F, RF])
         )
       ],
       (ip_zero<<tr(F,RF))).

:- end_object.

:- object(test_ex_4,
   extends(studyunit)).

   test_name('Задача 4. Правила преобразования во фразы.').

   test(translation_test,
      all((
        ::mem(c(F1, L1),
        [
          c(c=>c, [~c v c]),
          c(~(c=>c), [~c, c]),
          c((a=>b) & (b=>c) => (a=>c), [
             (a v b)v~a v c,
             (a v ~c)v~a v c,
             (~b v b)v~a v c,
             (~b v ~c)v~a v c])
        ]
      ),
        ::mem(Clause, L1)
      )),
      [
        each_test_name(genclause(F1, L1)),
        each_explain(
          format('!ERROR:\n!Формула ~q должна преобразоавться в дизъюнкты ~q\n',[F1, L1])
         )
      ],
      (ip_zero::clear_db,
       ip_zero::translate(F1),
       ip_zero<<clause(Clause))).

   test(remove_db_fake_test, true,
      [],
      (ip_zero::clear_db)
   ).

:- end_object.

:- object(test_machine,
   extends([ip_zero, studyunit])).

   :- protected(number/1).
   :- dynamic(number/1).
   number(10).
   number(2).

   rule(subtract, sub) :-
      number(X), number(Y), X>Y, !,
      retract(number(X)),
      X1 is X-Y,
      assertz(number(X1)).
   rule(stop, qed).

   test(test_pd_machine, true,
     [explain(::error('Такое впечатление, что основной цикл машины функционирует неправильно.'+[]))],
     (
      with_output_to(string(Out),
         test_machine::proof),
      ::output_substring(Out, "Найдено противоречие")
      )).


:- end_object.

:- object(test_ex_7,
   extends([ip_zero, studyunit])).

   test(test_rules,
      all((::mem(q(Name, Descr, Answer),
         [
           q(contradict, 'противоречие', qed),
           q(trivial_true, 'тривиально истинный дизъюнкт', _),
           q(double_literal, 'копии литералов в дизъюнкте', _),
           q(triv_pos_lit, 'резолюцию с тривиальным позитивным литералом', _),
           q(triv_neg_lit, 'резолюцию с тривиальным негативным литералом', _),
           q(general_resolution, 'общий вариант резолюции', _)
         ]
      ))),
      [
       each_test_name(test_rule_recognition(Name)),
       each_explain(::error('Ваша машина не распознала ~w.' +
         [Descr]))
       ],
      ( ip_zero::clear_db,
        % debugger::trace,
        test_ex_7::set_clauses(Name),
        ip_zero<<rule(_,Answer),
        test_ex_7::check(Name))).

   :- public(set_clauses/1).
   set_clauses(contradict) :-
      ip_zero::translate(~(c=>c)).

   set_clauses(triv_pos_lit) :-
      ip_zero::translate(p & (~p v q)).

   set_clauses(triv_neg_lit) :-
      ip_zero::translate(~p & (p v q)).

   set_clauses(trivial_true) :-
      ip_zero::translate(c=>c).

   set_clauses(double_literal) :-
      ip_zero::translate(c v c).

   set_clauses(general_resolution) :-
      ip_zero::translate((a v p v b) & (q v ~p v w)).

   :- public(check/1).
   check(contradict).

   check(triv_pos_lit) :-
      ip_zero<<clause(q).

   check(triv_neg_lit) :-
      ip_zero<<clause(q).

   check(trivial_true) :-
      \+ ip_zero<<clause(_).

   check(double_literal) :-
      ip_zero<<clause(c).

   check(general_resolution) :-
      ip_zero<<clause(A v B),
      (A == a v b -> B == q v w;
       A == q v w -> A == a v b;
       false).

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
         each_test_name(operator_defined),
         % each_test_name(operator_defined(Op)),
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

   test(check_reduction_rules, true,
        [condition(success(check_problem_2))],
        (test_ex_3::ok)).

   test(check_translation, true,
        [condition(success(check_reduction_rules))],
        (test_ex_4::ok)).

   % Proving cycle
   test(check_cycle, true,
        [condition(success(check_problem_2))],
        (test_machine::ok)).

   % Removal procedure
   test(check_removals,
       all((::mem(q(P, F, FR),
         [
          q(a, a v b, b),
          q(b, a v b, a),
          q(a, (a v b) v c, b v c),
          q(a, c v (a v b), c v b)
         ]))),
       [each_explain(::error('Процедура удаления элемента дизъюнкта работает неправильно!\nДля ~q, удаляя ~q, должно получаться ~q.'+
       [F, P, FR])),
        each_test_name(test_removal(P,F,FR)),
        condition(success(check_problem_2))],
       ((
         ip_zero<<remove(P, F, F1),
         (F1 == FR -> true;
          ::error('Неправильный результат удаления ~q из ~q: ~q, должен быть ~q'+[P, F, F1, FR]),
          fail)))).

   test(check_resolutions, true,
        [condition(success(check_translation))],
        (test_ex_7::ok)).

   % Run the final test of the ATP system.
   test(prover_test, true,
       [condition(success(check_ip_zero_test))],
       (ip_zero_test::ok)).

   % debug_level(60).

:- end_object.
