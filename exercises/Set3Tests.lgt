% ТЕСТЫ ДЛЯ ПРАКТИЧЕСКОЙ РАБОТЫ 3: ПАРАМЕТРИЧЕСКИЕ ОБЪЕКТЫ
% ======================================================

:- object(test_protocol,
   extends(studyunit)).

   test_name('Упражнение 1 - Протокол для геометрических фигур').
   test_type(problem).

   test(protocol_exists, true,
      [explain(::error('Протокол parameter_p не существует! Создайте его с помощью :- protocol(parameter_p).'))],
      current_protocol(parameter_p)).

   test(protocol_has_area, true,
      [condition(success(protocol_exists)),
       explain(::error('Протокол должен объявлять area/1 как public метод. Добавьте: :- public(area/1).'))],
      protocol_property(parameter_p, declares(area/1, public))).

   test(protocol_has_perimeter, true,
      [condition(success(protocol_exists)),
       explain(::error('Протокол должен объявлять perimeter/1 как public метод. Добавьте: :- public(perimeter/1).'))],
      protocol_property(parameter_p, declares(perimeter/1, public))).

   test(protocol_can_be_implemented, true,
      [condition(success(protocol_exists)),
       explain(::error('Протокол должен быть reusable. Убедитесь, что он не содержит конкретной реализации.'))],
      protocol_property(parameter_p, static)).

:- end_object.


:- object(test_geometric_figures,
   extends(studyunit)).

   test_name('Упражнение 2 - Геометрические фигуры').
   test_type(problem).

   test(circle_implements_protocol, true,
      [explain(::error('Объект circle должен реализовывать протокол parameter_p. Добавьте: implements(parameter_p).'))],
      implements_protocol(circle(_), parameter_p)).

   test(rectangle_implements_protocol, true,
      [explain(::error('Объект rectangle должен реализовывать протокол parameter_p. Добавьте: implements(parameter_p).'))],
      implements_protocol(rectangle(_,_), parameter_p)).

   test(circle_area_calculation, true,
      [condition(success(circle_implements_protocol)),
       explain(::error('Метод area/1 для круга должен вычислять площадь по формуле: π × R². Используйте is/2 для вычислений.'))],
      (circle(2)::area(Area), abs(Area - 12.566) < 0.001)).

   test(circle_perimeter_calculation, true,
      [condition(success(circle_implements_protocol)),
       explain(::error('Метод perimeter/1 для круга должен вычислять периметр по формуле: 2 × π × R.'))],
      (circle(3)::perimeter(Per), abs(Per - 18.84) < 0.01)).

   test(rectangle_area_calculation, true,
      [condition(success(rectangle_implements_protocol)),
       explain(::error('Метод area/1 для прямоугольника должен вычислять площадь по формуле: Width × Height.'))],
      (rectangle(3,4)::area(12))).

   test(rectangle_perimeter_calculation, true,
      [condition(success(rectangle_implements_protocol)),
       explain(::error('Метод perimeter/1 для прямоугольника должен вычислять периметр по формуле: 2 × (Width + Height).'))],
      (rectangle(3,4)::perimeter(14))).

:- end_object.


:- object(test_square_inheritance,
   extends(studyunit)).

   test_name('Упражнение 3 - Наследование квадрата от прямоугольника').
   test_type(problem).

   test(square_extends_rectangle, true,
      [explain(::error('Объект square должен наследовать от rectangle. Используйте: extends(rectangle(_,_)).'))],
      extends_object(square(_), rectangle(_,_))).

   test(square_parameters_passed_correctly, true,
      [condition(success(square_extends_rectangle)),
       explain(::error('Квадрат должен передавать свой параметр дважды в rectangle. Используйте: extends(rectangle(Side, Side)).'))],
      (square(5)::area(25), square(5)::perimeter(20))).

   test(square_uses_inherited_methods, true,
      [condition(success(square_extends_rectangle)),
       explain(::error('Квадрат не должен переопределять методы area/1 и perimeter/1 - они должны наследоваться от rectangle.'))],
      object_property(square(_), defines(area/1, _))).

:- end_object.


:- object(test_figures_collection,
   extends(studyunit)).

   test_name('Упражнение 4 - Коллекция фигур').
   test_type(problem).

   test(figures_implements_protocol, true,
      [explain(::error('Объект figures должен реализовывать протокол parameter_p.'))],
      implements_protocol(figures, parameter_p)).

   test(add_methods_exist, true,
      [condition(success(figures_implements_protocol)),
       explain(::error('Добавьте методы для добавления фигур: add_circle/1, add_rectangle/2, add_square/1 как public.'))],
      (figures::predicate_property(add_circle(_), public),
       figures::predicate_property(add_rectangle(_,_), public),
       figures::predicate_property(add_square(_), public))).

   test(dynamic_storage, true,
      [condition(success(figures_implements_protocol)),
       explain(::error('Используйте dynamic предикаты для хранения фигур. Пример: :- private(circle/1), :- dynamic(circle/1).'))],
      catch(figures<<circle(_), error(existence_error(procedure, _), _), fail)).

   test(total_area_calculation, true,
      [condition(success(dynamic_storage)),
       explain(::error('Метод area/1 должен суммировать площади всех фигур. Используйте findall/3 для сбора результатов.'))],
      (figures::add_circle(2),
       figures::add_rectangle(3,4),
       figures::area(Total),
       abs(Total - (12.566 + 12)) < 0.001)).

   test(total_perimeter_calculation, true,
      [condition(success(dynamic_storage)),
       explain(::error('Метод perimeter/1 должен суммировать периметры всех фигур. Не забудьте про sum_list/2.'))],
      (figures::add_square(5),
       figures::perimeter(Total),
       Total >= 0)).

:- end_object.


:- object(test_abstract_automaton,
   extends(studyunit)).

   test_name('Упражнение 5 - Абстрактный конечный автомат').
   test_type(problem).

   test(automaton_has_parameter, true,
      [explain(::error('Автомат должен быть параметрическим объектом с параметром начального состояния.'))],
      current_object(automaton(_))).

   test(protected_q_defined, true,
      [condition(success(automaton_has_parameter)),
       explain(::error('Объявите protected предикат q/4: :- protected(q/4).'))],
      predicate_property(automaton(_)::q(_,_,_,_), protected)).

   test(model_method_public, true,
      [condition(success(automaton_has_parameter)),
       explain(::error('Добавьте public метод model/3: :- public(model/3).'))],
      predicate_property(automaton(_)::model(_,_,_), public)).

   test(model_recursive_implementation, true,
      [condition(success(model_method_public)),
       explain(::error('Реализуйте model/3 рекурсивно. Используйте protected model/4 для обработки текущего состояния.'))],
      predicate_property(automaton(_)::model(_,_,_,_), protected)).

:- end_object.


:- object(test_float_automaton,
   extends(studyunit)).

   test_name('Упражнение 6 - Автомат для чисел с плавающей запятой').
   test_type(problem).

   test(extends_automaton, true,
      [explain(::error('float_automaton должен наследовать от automaton.'))],
      extends_object(float_automaton(_), automaton(_))).

   test(accepts_valid_numbers, true,
      [condition(success(extends_automaton)),
       explain(::error('Автомат должен принимать числа в формате: 123, 12.34, -5.6e7, +8.9E-10'))],
      (float_automaton(start)::model(['1','2','.','3','4'], _, accept),
       float_automaton(start)::model(['-','5','.','6','e','7'], _, accept))).

   test(rejects_invalid_sequences, fail,
      [condition(success(extends_automaton)),
       explain(::error('Автомат должен отвергать некорректные последовательности: 12., .34, e5, 12e'))],
      float_automaton(start)::model(['1','2','.'], _, accept)).

   test(handles_exponent_part, true,
      [condition(success(extends_automaton)),
       explain(::error('Реализуйте переходы для экспоненциальной части: e/E, знак +/-, цифры.'))],
      float_automaton(start)::model(['1','e','+','2'], _, accept)).

:- end_object.


:- object(test_float_generator,
   extends(studyunit)).

   test_name('Упражнение 7 - Генератор последовательностей').
   test_type(problem).

   test(extends_float_automaton, true,
      [explain(::error('float_generator должен наследовать от float_automaton.'))],
      extends_object(float_generator(_), float_automaton(_))).

   test(generate_method_exists, true,
      [condition(success(extends_float_automaton)),
       explain(::error('Добавьте public метод generate/1 для генерации последовательностей.'))],
      predicate_property(float_generator(_)::generate(_), public)).

   test(generate_valid_method_exists, true,
      [condition(success(extends_float_automaton)),
       explain(::error('Добавьте public метод generate_valid/1 для генерации только валидных чисел.'))],
      predicate_property(float_generator(_)::generate_valid(_), public)).

   test(generates_valid_numbers, true,
      [condition(success(generate_valid_method_exists)),
       explain(::error('generate_valid/1 должен использовать atom_number/2 для проверки валидности чисел.'))],
      (float_generator(start)::generate_valid(Number),
       atom(Number),
       catch(atom_number(Number, _), _, fail))).

:- end_object.


:- object(test_arithmetic_automaton,
   extends(studyunit)).

   test_name('Упражнение 8 - Арифметический автомат-транслятор').
   test_type(problem).

   test(extends_automaton, true,
      [explain(::error('arithmetic_automaton должен наследовать от automaton.'))],
      extends_object(arithmetic_automaton(_), automaton(_))).

   test(evaluate_method_exists, true,
      [condition(success(extends_automaton)),
       explain(::error('Добавьте public метод evaluate/2 для вычисления результата выражения.'))],
      predicate_property(arithmetic_automaton(_)::evaluate(_,_), public)).

   test(recognizes_simple_expressions, true,
      [condition(success(evaluate_method_exists)),
       explain(::error('Автомат должен распознавать выражения вида: 3+4, 5*2, 8-3, 6/2'))],
      arithmetic_automaton(start)::model(['3','+','4'], _, accept)).

   test(computes_correct_results, true,
      [condition(success(recognizes_simple_expressions)),
       explain(::error('evaluate/2 должен вычислять правильный результат. Проверьте порядок операций.'))],
      (arithmetic_automaton(start)::evaluate('3+4*2', 11),
       arithmetic_automaton(start)::evaluate('8-3+2', 7))).

   test(supports_all_operations, true,
      [condition(success(computes_correct_results)),
       explain(::error('Поддержите все операции: +, -, *, /. Проверьте каждую в q/4.'))],
      (arithmetic_automaton(start)::evaluate('6/2', 3),
       arithmetic_automaton(start)::evaluate('5*3', 15))).

:- end_object.


:- object(test_parser,
   extends(studyunit)).

   test_name('Упражнение 9 - Синтаксический анализатор').
   test_type(problem).

   test(parser_has_parameter, true,
      [explain(::error('Парсер должен быть параметрическим объектом с параметром лексикона.'))],
      current_object(parser(_))).

   test(parse_method_exists, true,
      [condition(success(parser_has_parameter)),
       explain(::error('Добавьте public метод parse/2 для разбора предложений.'))],
      predicate_property(parser(_)::parse(_,_), public)).

   test(grammar_rules_defined, true,
      [condition(success(parser_has_parameter)),
       explain(::error('Определите protected предикаты для каждого правила грамматики: sentence/3, noun_group/3, verb_group/3.'))],
      (predicate_property(parser(_)::sentence(_,_,_), protected),
       predicate_property(parser(_)::noun_group(_,_,_), protected))).

   test(produces_syntax_tree, true,
      [condition(success(parse_method_exists)),
       explain(::error('parse/2 должен возвращать дерево разбора в формате sent(noun_group, verb_group).'))],
      catch(parser(_)::parse([a,cow,shakes,the,tail], sent(_,_)), _, fail)).

:- end_object.


:- object(test_lexicon,
   extends(studyunit)).

   test_name('Упражнение 10 - Лексикон языка').
   test_type(problem).

   test(gen_lexic_has_determinants, true,
      [explain(::error('gen_lexic должен определять determinant/1 с общими детерминантами.'))],
      (gen_lexic::determinant(a),
       gen_lexic::determinant(the),
       gen_lexic::determinant(my))).

   test(cow_lexic_extends_gen_lexic, true,
      [explain(::error('cow_lexic должен наследовать от gen_lexic.'))],
      extends_object(cow_lexic, gen_lexic)).

   test(cow_lexic_has_nouns, true,
      [condition(success(cow_lexic_extends_gen_lexic)),
       explain(::error('cow_lexic должен определять noun/1 с существительными cow и tail.'))],
      (cow_lexic::noun(cow),
       cow_lexic::noun(tail))).

   test(cow_lexic_has_verbs, true,
      [condition(success(cow_lexic_extends_gen_lexic)),
       explain(::error('cow_lexic должен определять verb/1 с глаголами walks и shakes.'))],
      (cow_lexic::verb(walks),
       cow_lexic::verb(shakes))).

:- end_object.


:- object(test_sentence_generator,
   extends(studyunit)).

   test_name('Упражнение 11 - Генератор предложений').
   test_type(problem).

   test(extends_parser, true,
      [explain(::error('cow_lang должен наследовать от parser(cow_lexic).'))],
      extends_object(cow_lang, parser(cow_lexic))).

   test(generate_method_exists, true,
      [condition(success(extends_parser)),
       explain(::error('Добавьте public метод generate/1 для генерации предложений.'))],
      predicate_property(cow_lang::generate(_), public)).

   test(generates_valid_sentences, true,
      [condition(success(generate_method_exists)),
       explain(::error('generate/1 должен использовать sentence/3 в режиме генерации для создания предложений.'))],
      (cow_lang::generate(Sentence),
       is_list(Sentence),
       cow_lang::parse(Sentence, _))).

   test(sentences_follow_grammar, true,
      [condition(success(generates_valid_sentences)),
       explain(::error('Сгенерированные предложения должны соответствовать грамматике: determinant noun verb determinant noun.'))],
      forall(cow_lang::generate(Sentence), valid_sentence_structure(Sentence))).

   :- private(valid_sentence_structure/1).
   valid_sentence_structure([Det1, Noun1, Verb, Det2, Noun2]) :-
      cow_lexic::determinant(Det1),
      cow_lexic::noun(Noun1),
      cow_lexic::verb(Verb),
      cow_lexic::determinant(Det2),
      cow_lexic::noun(Noun2).

:- end_object.


:- object(test_tree_balancer,
   extends(studyunit)).

   test_name('Упражнение 12 - Балансировка двоичного дерева').
   test_type(problem).

   test(balance_method_exists, true,
      [explain(::error('Добавьте public метод balance/2 для балансировки деревьев.'))],
      predicate_property(tree_balancer::balance(_,_), public)).

   test(is_balanced_method_exists, true,
      [explain(::error('Добавьте public метод is_balanced/1 для проверки сбалансированности.'))],
      predicate_property(tree_balancer::is_balanced(_), public)).

   test(height_method_exists, true,
      [explain(::error('Добавьте public метод height/2 для вычисления высоты дерева.'))],
      predicate_property(tree_balancer::height(_,_), public)).

   test(balances_unbalanced_tree, true,
      [condition(success(balance_method_exists)),
       explain(::error('balance/2 должен преобразовывать несбалансированное дерево в сбалансированное.'))],
      (Unbalanced = tree(5, tree(3, tree(2, tree(1, nil, nil), nil), nil), tree(7, nil, nil)),
       tree_balancer::balance(Unbalanced, Balanced),
       tree_balancer::is_balanced(Balanced))).

   test(height_calculation_correct, true,
      [condition(success(height_method_exists)),
       explain(::error('height/2 должен корректно вычислять высоту дерева. Высота nil = 0, tree(_,L,R) = 1 + max(height(L), height(R))'))],
      (tree_balancer::height(nil, 0),
       tree_balancer::height(tree(1, nil, nil), 1),
       tree_balancer::height(tree(1, tree(2, nil, nil), nil), 2))).

   test(flatten_build_cycle_works, true,
      [condition(success(balance_method_exists)),
       explain(::error('Используйте алгоритм: flatten_tree -> sort -> build_balanced_tree.'))],
      (TestTree = tree(3, tree(1, nil, nil), tree(5, nil, nil)),
       tree_balancer::balance(TestTree, Balanced),
       tree_balancer::is_balanced(Balanced),
       tree_balancer::height(Balanced, Height),
       Height =< 2)).

:- end_object.


:- object(tests,
   extends(studyunit)).

   test_name('Практическая работа 3 - Параметрические объекты').
   test_type(problem_set).

   test(exercise_1_protocol, true,
      [explain(::error('Проверьте создание протокола для геометрических фигур'))],
      test_protocol::ok).

   test(exercise_2_geometric_figures, true,
      [explain(::error('Проверьте реализацию геометрических фигур'))],
      test_geometric_figures::ok).

   test(exercise_3_square_inheritance, true,
      [explain(::error('Проверьте наследование квадрата от прямоугольника'))],
      test_square_inheritance::ok).

   test(exercise_4_figures_collection, true,
      [explain(::error('Проверьте коллекцию фигур с динамическим управлением'))],
      test_figures_collection::ok).

   test(exercise_5_abstract_automaton, true,
      [explain(::error('Проверьте абстрактный конечный автомат'))],
      test_abstract_automaton::ok).

   test(exercise_6_float_automaton, true,
      [explain(::error('Проверьте автомат для чисел с плавающей запятой'))],
      test_float_automaton::ok).

   test(exercise_7_float_generator, true,
      [explain(::error('Проверьте генератор последовательностей'))],
      test_float_generator::ok).

   test(exercise_8_arithmetic_automaton, true,
      [explain(::error('Проверьте арифметический автомат-транслятор'))],
      test_arithmetic_automaton::ok).

   test(exercise_9_parser, true,
      [explain(::error('Проверьте синтаксический анализатор'))],
      test_parser::ok).

   test(exercise_10_lexicon, true,
      [explain(::error('Проверьте лексикон языка'))],
      test_lexicon::ok).

   test(exercise_11_sentence_generator, true,
      [explain(::error('Проверьте генератор предложений'))],
      test_sentence_generator::ok).

   test(exercise_12_tree_balancer, true,
      [explain(::error('Проверьте балансировщик двоичных деревьев'))],
      test_tree_balancer::ok).

   :- public(print_progress/0).
   print_progress :-
      self(Self),
      ::info("=== ПРОГРЕСС ВЫПОЛНЕНИЯ ==="),
      forall(
         ::test_result(Self, Name, Result),
         (   Result = success
         ->  ::info("✓ Упражнение завершено: ~w" + [Name])
         ;   Result = failure
         ->  ::info("✗ Требуется доработка: ~w" + [Name])
         ;   ::info("… Упражнение пропущено: ~w" + [Name])
         )
      ),
      ::test_count(SCount, FCount, SkCount),
      Total is SCount + FCount + SkCount,
      ::info("Выполнено: ~w из ~w упражнений, пропущено: ~w" + [SCount, Total, SkCount]),
      (   FCount = 0, SkCount = 0
      ->  ::info("🎉 Отлично! Все упражнения выполнены!")
      ;   ::info("💡 Совет: Читайте подсказки в сообщениях об ошибках для быстрого исправления.")
      ).

   run :-
      ^^run,
      ::print_progress.

:- end_object.
