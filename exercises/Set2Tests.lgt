:- object(hp_queries,
   extends(studyunit)).

   test_name('Задача 1 - Запросы к базе данных Harry Potter').
   test_type(problem).

   :- public(query/2).
   query(Name-fail, AnswerList) :-
      \+ hp_query::query(Name, AnswerList), !, fail.
   query(Name-fail, _AnswerList) :-
      ::error('Запрос ~q выполнен с ошибкой!' + [Name]), !, fail.
   query(Name, AnswerList) :-
      hp_query::query(Name, AnswerList), !.
   query(Name, _AnswerList) :-
      ::error('Запрос ~q выполнен с ошибкой!' + [Name]), !, fail.

   test(susan_bones_movies, true,
      [explain(::error('Susan Bones должна фигурировать в фильме Chamber of Secrets' + []))],
      (::query('Movie with Susan Bones',['Chamber of Secrets']))).

   test(movies_wo_susan_bones, true,
      [explain(::error('Susan Bones не фигурирует в Prisoner of Azkaban, но ваш запрос этого не показывает' + []))],
      (::query('Movie without Susan Bones',['Prisoner of Azkaban']))).

   test(same_series_characters, true,
      [explain(::error('Harry Potter и Hermione Granger снимались во всех фильмах вместе!' + []))],
      (::query('Character having common series',['Hermione Granger', 'Harry Potter']))).

   test(no_self_common_series, fail,
      [explain(::error('Персонаж не может иметь общие фильмы сам с собой!' + []))],
      (::query('Character having common series'-fail,['Harry Potter', 'Harry Potter']))).

   test(character_in_movie, true,
      [explain(::error('Harry Potter должен быть в фильме Half-Blood Prince' + []))],
      (::query('Movie having a character',['Half-Blood Prince', 'Harry Potter']))).

   test(single_movie_character, true,
      [explain(::error('Rolanda Hooch фигурирует только в одном фильме' + []))],
      (::query('Character appearing only in one Movie', ['Rolanda Hooch', 'Philosopher\'s Stone']))).

   test(character_filmed_wo_dudley, true,
      [explain(::error('Argus Filch был в Goblet of Fire, где не было Dudley Dursley' + []))],
      (::query('Character filmed in a Movie without Dudley Dursley',
          ['Argus Filch', 'Goblet of Fire']))).

   % Дополнительные тесты для полноты покрытия
   % test(multiple_answers_check, true,
   %    [explain(::error('Запрос должен возвращать несколько ответов при backtracking' + []))],
   %    (findall(Movie, ::query('Movie with Susan Bones', [Movie]), Movies),
   %     length(Movies, Count),
   %     Count >= 2)).

:- end_object.

:- object(test_hp_db_mod(_O_),
   extends(studyunit),
   imports(object_exists_and_predicates_c(_O_,
     [
       cast/2 - [protected, dynamic],
       add/2 - [public],
       remove/2 - [public]
     ]))).

   test_name('Задача 2 - Управление базой данных актеров').
   test_type(problem).

   test(A,B,C,D) :- ^^test(A,B,C,D).

   test(test_user_test, true,
       [condition(success(basic_predicates_defined))],
       ((hp_test::ok))).

   % Дополнительные тесты
   test(add_duplicate_character, true,
       [condition(success(basic_predicates_defined)),
        explain(::error('Добавление дубликата должно заменять существующую запись' + []))],
       (
           _O_::add('Harry Potter', 'Новый актер'),
           _O_<<cast('Harry Potter', 'Новый актер'),
           \+ _O_<<cast('Harry Potter', 'Daniel Radcliffe')
       )).

   test(remove_nonexistent, true,
       [condition(success(basic_predicates_defined)),
        explain(::error('Удаление несуществующей записи не должно вызывать ошибку' + []))],
       (
           _O_::remove('Несуществующий персонаж', _),
           true  % Должен завершиться без ошибки
       )).

:- end_object.

:- object(test_freq_dict(_O_),
   extends(studyunit),
   imports(object_exists_and_predicates_c(_O_,
     [
       add/1 - [public],
       list/2 - [public],
       print/0 - [public]
     ]))).

   test_name('Задача 3 - Частотный словарь').
   test_type(problem).

   test(A,B,C,D) :- ^^test(A,B,C,D).

   test(add_words,
      all(::mem(Word-Number, [
        alaska-10,
        california-31,
        alrosa-3
      ])),
      [condition(success(basic_predicates_defined)),
       each_test_name(add_word(Word, Number)),
       each_explain(
         ::error('Невозможно добавить слово ~q ~w раз' + [Word, Number]))],
      ((::checkall(between(1,Number, _), _O_::add(Word))))).

   test(check_words_in_dictionary,
      all(::mem(Word-Number, [
        alaska-10,
        california-31,
        alrosa-3
      ])),
      [condition(success(add_words)),
       each_test_name(check_word(Word, Number)),
       each_explain(
         ::error('Слово ~q должно встречаться ~w раз' + [Word, Number]))],
      ((_O_::list(Word, Number)))).

   test(check_printing,
      all(::mem(Word-Number, [
        alaska-10,
        california-31,
        alrosa-3
      ])),
      [condition(success(add_words)),
       each_test_name(check_word(Word, Number)),
       each_explain(
         ::error('Мы полагаем, что вывод словаря ~q строится на построении строк, где сначала выводится слово, а потом число... но не можем удоставериться в этом!? Как-то нестандартно реализован print/0, реализован ли вообще?' +
        [_O_]))],
      ((
        with_output_to(string(Out),
        _O_::print),
        ::check_output(Out, Word, Number)
      ))).

   test(case_sensitivity, true,
      [condition(success(basic_predicates_defined)),
       explain(::error('Регистр слов должен учитываться' + []))],
      (
          _O_::add('Test'),
          _O_::add('test'),
          _O_::list('Test', 1),
          _O_::list('test', 1)
      )).

   test(empty_word_handling, true,
      [condition(success(basic_predicates_defined)),
       explain(::error('Пустые строки должны обрабатываться корректно' + []))],
      (
          _O_::add(''),
          _O_::list('', 1)
      )).

   :- protected(check_output/3).
   check_output(O, Word, Number) :-
      format(string(N), '~w', Number),
      % format(O),
      sub_string(O, BW, LW, _, Word),
      sub_string(O, BN,_, _, N),
      BW + LW =< BN.

:- end_object.

:- object(test_fib(_O_, _Predicates_),
   extends(studyunit),
   imports(object_exists_and_predicates_c(_O_, _Predicates_))).

   test_name('Задача 4-5 - Числа Фибоначчи'(_O_)).
   test_type(problem).

   test(A,B,C,D) :- ^^test(A,B,C,D).

   test(generation_of_fibs,
      all((::mem(N-V, [
           1-1, 2-1, 3-2, 4-3, 5-5, 6-8, 11-89
         ]))),
       [each_test_name(gen_fib_number(N,V)),
        each_explain(::error('fib(~q) должен быть ~q' + [N,V])),
        condition(success(basic_predicates_defined))],
       ((_O_::calc(N, V)))).

   test(negative_input, fail,
      [condition(success(basic_predicates_defined)),
       explain(::error('Отрицательные числа не должны обрабатываться' + []))],
      (_O_::calc(-1, _))).

   test(zero_input, fail,
      [condition(success(basic_predicates_defined)),
       explain(::error('Ноль не должен обрабатываться' + []))],
      (_O_::calc(0, _))).

:- end_object.

:- object(test_fib_cache(_O_),
   extends(studyunit)).

   test_name('Задача 5 - Проверка кэша Фибоначчи'(_O_)).
   test_type(problem).

   test(cache_populated,
      all((::mem(N-V, [3-2, 4-3, 5-5, 6-8, 11-89]))),
       [each_test_name(cache_entry(N,V)),
        each_explain(::error('Кэш должен содержать запись для fib(~q)=~q' + [N,V]))],
       ((_O_<<cache(N, V)))).

   test(cache_usage, true,
      [condition(success(cache_populated)),
       explain(::error('Кэш должен ускорить повторные вычисления' + []))],
      (
          statistics(walltime, [Start|_]),
          _O_::calc(11, _),
          statistics(walltime, [End|_]),
          Duration is End - Start,
          Duration < 100  % Должно быть быстро благодаря кэшу
      )).

:- end_object.

% ДС

:- object(test_setup(_O_),
   extends(studyunit),
   imports(object_exists_and_predicates_c(_O_,
     [
       set_option/1 - [public],
       set_option/2 - [public],
       current_option/1 - [public],
       current_option/2 - [public]
     ]))).

   test_name('Задача 6 - Система настроек').
   test_type(problem).

   test(A,B,C,D) :- ^^test(A,B,C,D).

   test(set_option_name_value, true,
      [condition(success(basic_predicates_defined)),
       explain(::error('set_option/2 должен устанавливать опции в формате Name, Value' + []))],
      (
          _O_::set_option(test_name, test_value),
          _O_::current_option(test_name, test_value)
      )).

   test(set_option_equals, true,
      [condition(success(basic_predicates_defined)),
       explain(::error('set_option/1 должен поддерживать формат Name=Value' + []))],
      (
          _O_::set_option(test_name2=test_value2),
          _O_::current_option(test_name2, test_value2)
      )).

   test(set_option_dash, true,
      [condition(success(basic_predicates_defined)),
       explain(::error('set_option/1 должен поддерживать формат Name-Value' + []))],
      (
          _O_::set_option(test_name3-test_value3),
          _O_::current_option(test_name3, test_value3)
      )).

   test(current_option_equals, true,
      [condition(success(set_option_equals)),
       explain(::error('current_option/1 должен поддерживать формат Name=Value' + []))],
      (
          _O_::current_option(test_name2=test_value2)
      )).

   test(current_option_dash, true,
      [condition(success(set_option_dash)),
       explain(::error('current_option/1 должен поддерживать формат Name-Value' + []))],
      (
          _O_::current_option(test_name3-test_value3)
      )).

   test(option_overwrite, true,
      [condition(success(basic_predicates_defined)),
       explain(::error('Повторная установка опции должна заменять старое значение' + []))],
      (
          _O_::set_option(overwrite, value1),
          _O_::set_option(overwrite, value2),
          _O_::current_option(overwrite, value2),
          \+ _O_::current_option(overwrite, value1)
      )).

:- end_object.

% :- object(test_my_setup,
%    extends(studyunit)).

%    test_name('Задача 6 - Проверка статических настроек').
%    test_type(problem).

%    test(static_option1, true,
%       [explain(::error('Статическая опция prog_name1 должна быть доступна' + []))],
%       (my_setup::current_option(prog_name1, program_one))).

%    test(static_option2, true,
%       [explain(::error('Статическая опция prog_name2=program_two должна быть доступна' + []))],
%       (my_setup::current_option(prog_name2=program_two))).

%    test(static_option3, true,
%       [explain(::error('Статическая опция prog_name3-program_three должна быть доступна' + []))],
%       (my_setup::current_option(prog_name3-program_three))).

%    test(dynamic_override, true,
%       [explain(::error('Динамические опции должны переопределять статические' + []))],
%       (
%           my_setup::set_option(prog_name1, new_value),
%           my_setup::current_option(prog_name1, new_value)
%       )).

% :- end_object.

:- object(test_expert_system(_O_),
   extends(studyunit),
   imports(object_exists_and_predicates_c(_O_,
     [
       session/0 - [public],
       print/0 - [public]
     ]))).

   test_name('Задача 7 - Экспертная система').
   test_type(problem).

   test(A,B,C,D) :- ^^test(A,B,C,D).

   test(initial_knowledge, true,
      [condition(success(basic_predicates_defined)),
       explain(::error('Система должна знать начальный объект "Зайка"' + []))],
      (
          _O_<<root('Зайка'),
          _O_<<leave('Зайка')
      )).

   test(knowledge_base_structure, true,
      [condition(success(initial_knowledge)),
       explain(::error('База знаний должна содержать корректную структуру узлов' + []))],
      (
          % Проверяем, что можно получить сессию без ошибок
          catch(_O_::session, Error,
            (Error \= error(existence_error(procedure, _), !, fail)),
          true
      ))).

   test(learning_capability, true,
      [condition(success(knowledge_base_structure)),
       explain(::error('Система должна уметь обучаться через update/3' + []))],
      (
          % Проверяем, что update/3 определен и может быть вызван
          catch(_O_<<update('Зайка', 'Котик', 'пушистый'), Error,
            (Error = error(existence_error(procedure, update/3), !, fail)),
          true
      ))).

   test(print_method, true,
      [condition(success(basic_predicates_defined)),
       explain(::error('Метод print/0 должен выводить информацию о системе' + []))],
      (
          with_output_to(string(Output), _O_::print),
          sub_string(Output, _, _, _, 'Зайка')
      )).

:- end_object.

:- object(test_gcd(_O_),
   extends(studyunit),
   imports(object_exists_and_predicates_c(_O_,
     [
       run/0 - [public]
     ]))).

   test_name('Задача 8 - Алгоритм Евклида').
   test_type(problem).

   test(A,B,C,D) :- ^^test(A,B,C,D).

   test(number_predicate, true,
      [condition(success(basic_predicates_defined)),
       explain(::error('Должен быть объявлен protected dynamic number/1' + []))],
      (
          catch(_O_<<number(_), error(existence_error(procedure, _), _), fail)
      )).

   test(subtract_rule, true,
      [condition(success(number_predicate)),
       explain(::error('Должно быть реализовано правило subtract' + []))],
      (
          catch(_O_<<rule(subtract), error(existence_error(procedure, _), _), fail)
      )).

   test(print_rule, true,
      [condition(success(number_predicate)),
       explain(::error('Должно быть реализовано правило print' + []))],
      (
          catch(_O_<<rule(print), error(existence_error(procedure, _), _), fail)
      )).

   test(euclidean_algorithm, true,
      [condition(success(subtract_rule)),
       explain(::error('Алгоритм должен корректно вычислять НОД' + []))],
      (
          _O_::assertz(number(42)),
          _O_::assertz(number(56)),
          catch(_O_::run, Error,
            (Error \= error(existence_error(procedure, _), !, fail)),
          % После выполнения в базе должен остаться только НОД
          findall(N, _O_<<number(N), Numbers),
          length(Numbers, 1),
          member(14, Numbers)  % НОД(42,56)=14
      ))).

   test(single_number_case, true,
      [condition(success(euclidean_algorithm)),
       explain(::error('Для одного числа НОД должен быть само число' + []))],
      (
          _O_::retractall(number(_)),
          _O_::assertz(number(17)),
          catch(_O_::run, Error,
            (Error \= error(existence_error(procedure, _), !, fail)),
          _O_<<number(17)
      ))).

:- end_object.

:- object(test_prim_tree(_O_),
   extends(studyunit),
   imports(object_exists_and_predicates_c(_O_,
     [
       add/1 - [public],
       add/2 - [public],
       build/0 - [public],
       clear_db/0 - [public]
     ]))).

   test_name('Задача 10 - Алгоритм Прима').
   test_type(problem).

   test(A,B,C,D) :- ^^test(A,B,C,D).

   test(graph_inheritance, true,
      [explain(::error('Объект должен наследовать от graph' + []))],
      (extends_object(_O_, graph))).

   test(vertex_predicate, true,
      [condition(success(basic_predicates_defined)),
       explain(::error('Должен быть объявлен private dynamic vertex/1' + []))],
      (
          catch(_O_<<vertex(_), error(existence_error(procedure, _), _), fail)
      )).

   test(edge_predicate, true,
      [condition(success(basic_predicates_defined)),
       explain(::error('Должен быть объявлен protected dynamic edge/2' + []))],
      (
          catch(_O_<<edge(_, _), error(existence_error(procedure, _), _), fail)
      )).

   test(add_vertex, true,
      [condition(success(vertex_predicate)),
       explain(::error('Метод add/1 должен добавлять вершины' + []))],
      (
          _O_::add(test_vertex),
          _O_<<vertex(test_vertex)
      )).

   test(test_add_edge, true,
      [condition(success(edge_predicate)),
       explain(::error('Метод add/2 должен добавлять ребра' + []))],
      (
          _O_::add(v1, v2),
          _O_<<edge(v1, v2)
      )).

   test(test_clear_db, true,
      [condition(success(add_vertex)),
       explain(::error('Метод clear_db/0 должен очищать базу данных' + []))],
      (
          _O_::add(temp_vertex),
          _O_::clear_db,
          \+ _O_<<vertex(temp_vertex)
      )).

   test(test_build_spanning_tree, true,
      [condition(success(clear_db)),
       explain(::error('Метод build/0 должен строить остовое дерево' + []))],
      (
          _O_::clear_db,
          catch(_O_::build, Error,
            (Error \= error(existence_error(procedure, _), !, fail)),
          % После построения должны быть добавлены вершины и ребра
          findall(V, _O_<<vertex(V), Vertices),
          findall(E, _O_<<edge(E, _), Edges),
          length(Vertices, VCount),
          length(Edges, ECount),
          VCount >= 1,
          ECount >= 0
      ))).

   test(test_spanning_tree_properties, true,
      [condition(success(build_spanning_tree)),
       explain(::error('Остовое дерево должно охватывать все вершины графа' + []))],
      (
          % Граф имеет 7 вершин: a,b,c,d,e,f,g
          findall(V, _O_<<vertex(V), Vertices),
          length(Vertices, VCount),
          VCount =:= 7,
          % Остовое дерево должно иметь V-1 ребер
          findall(E, _O_<<edge(E, _), Edges),
          length(Edges, ECount),
          ECount =:= 6
      )).

:- end_object.


% Main test object

:- object(tests,
   extends(studyunit)).
   % debug_level(60).

   test_name('Тестовый набор по теме 2 - Динамические предикаты и состояние объектов').

   test_type(problem_set).

   test(hp_queries, true,
      [explain(::error('Не все запросы запрограммированы правильно!\nИспользуйте ?- hp_query::query(<название запроса>) для отладки запроса.' +
      []))],
      (hp_queries::ok)).

   test(hp_cast_database, true,
      [explain(::error('Проверьте управление базой данных актеров'))],
      ((test_hp_db_mod(harry_potter_movie)::ok))).

   test(freq_dict_test, true,
      [explain(::error('Проверьте работу частотного словаря'))],
      ((test_freq_dict(freq_dict)::ok))).

   test(fibonacci_correct, true,
      [explain(::error('Проверьте вычисление чисел Фибоначчи'))],
      (test_fib(fibonacci, [calc/2 - public])::ok)).

   test(fibonacci_cached_correct, true,
      [condition(success(fibonacci_correct)),
       explain(::error('Проверьте кэширование чисел Фибоначчи'))],
      (test_fib(fibonacci_cached, [cache/2-[protected, dynamic]])::ok)).

   test(fibonacci_cache_exists, true,
      [condition(success(fibonacci_cached_correct)),
       explain(::error('Проверьте заполнение кэша'))],
      (test_fib_cache(fibonacci_cached)::ok)).

   test(setup_system, true,
      [explain(::error('Проверьте систему настроек'))],
      (test_setup(setup)::ok)).

   % test(setup_static_options, true,
   %    [condition(success(setup_system)),
   %     explain(::error('Проверьте статические настройки'))],
   %    (test_my_setup::ok)).

   test(expert_system, true,
      [explain(::error('Проверьте экспертную систему'))],
      (test_expert_system(expert_system)::ok)).

   test(gcd_algorithm, true,
      [explain(::error('Проверьте алгоритм Евклида'))],
      (test_gcd(gcd)::ok)).

   test(prim_algorithm, true,
      [explain(::error('Проверьте алгоритм Прима'))],
      (test_prim_tree(prim_tree)::ok)).

   % Дополнительная статистика
   :- public(print_statistics/0).
   print_statistics :-
      ::info("=== СТАТИСТИКА ВЫПОЛНЕНИЯ ==="),
      (::test(Name, true, _, _), ::info("✓ ~w" + [Name]), fail; true),
      (::test(Name, fail, _, _), ::info("✗ ~w" + [Name]), fail; true),
      findall(_, ::test(_, true, _, _), Successes),
      findall(_, ::test(_, fail, _, _), Failures),
      length(Successes, SCount),
      length(Failures, FCount),
      Total is SCount + FCount,
      ::info("Успешно: ~w/~w тестов" + [SCount, Total]).

   run :-
      ^^run,
      ::print_statistics.

:- end_object.
