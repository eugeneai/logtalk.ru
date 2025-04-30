
:- object(hp_queries,
   extends(studyunit)).

   test_name('Задача 1').
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
      [
        explain(::error('Susan Bones фигурирует в фильме Chamber of Secrets, а у вас нет!' + []))
      ],
      (::query('Movie with Susan Bones',['Chamber of Secrets']))).

   test(movies_wo_susan_bones, true,
      [
        explain(::error('Susan Bones не фигурирует в фильме \'Prisoner of Azkaban\', а ваш запрос не показвыает это!' + []))
      ],
      (::query('Movie without Susan Bones',['Prisoner of Azkaban']))).

   test(same_series_characters, true,
      [
        explain(::error('Harry Potter и Hermione Granger снимались во всех фильмах! а у вас не так!' + []))
      ],
      (::query('Character having common series',['Hermione Granger', 'Harry Potter']))).

   test(same_series_characters, fail,
      [
        explain(::error('Ага, Harry Potter сам с собой снялся во всех фильмах! ;-)!' + []))
      ],
      (::query('Character having common series'-fail,['Harry Potter', 'Harry Potter']))).

   test(character_and_a_movie, true,
      [
        explain(::error('Harry Potter у вас не снялся в фильме Half-Blood Prince !, что странно.' + []))
      ],
      (::query('Movie having a character',['Half-Blood Prince', 'Harry Potter']))).

   test(single_movie_character, true,
      [
        explain(::error('Rolanda Hooch должна фигурировать только в одном фильме. У вас это не так.' + []))
      ],
      (::query('Character appearing only in one Movie', ['Rolanda Hooch', 'Philosopher\'s Stone']))).

   test(character_filmed_wo_dudley, true,
      [
        explain(::error('Argus Filch фигурировал в фильме Goblet of Fire, где не было Dudley!' + []))
      ],
      (::query('Character filmed in a Movie without Dudley Dursley',
          ['Argus Filch', 'Goblet of Fire']))).

:- end_object.

:- object(test_hp_db_mod(_O_),
   extends(studyunit),
   imports(object_exists_and_predicates_c(_O_,
     [
       cast/2 - [protected, dynamic],
       add/2 - [public],
       remove/2 - [public]
     ]))).

   test_name('Задача 2').
   test_type(problem).

   test(A,B,C,D) :- ^^test(A,B,C,D).

   test(test_user_test, true,
       [condition(success(basic_predicates_defined))],
       ((hp_test::ok))).

:- end_object.

:- object(test_freq_dict(_O_),
   extends(studyunit),
   imports(object_exists_and_predicates_c(_O_,
     [
       add/1 - [public],
       list/2 - [public],
       print/0 - [public]
     ]))).

   test_name('Задача 3, частотный словарь').
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
         ::error('Невозможно выполнить добавление (~w:add(~q)) слова ~q. Вероятно забыли реализовать add/2.' +
        [_O_, Word, Word]))],
      ((
        ::checkall(
          between(1,Number, _),
          _O_::add(Word))
      ))).

   test(check_words_in_dictionary,
      all(::mem(Word-Number, [
        alaska-10,
        california-31,
        alrosa-3
      ])),
      [condition(success(add_words)),
       each_test_name(check_word(Word, Number)),
       each_explain(
         ::error('Проверка словаря (~w:list(~q,~q)) для слова ~q неуспешна. Проверьте реализацию list/2, add/2, вашу систему хранения словаря и т.п.' +
        [_O_, Word, Number, Word]))],
      ((
        _O_::list(Word, Number)
      ))).

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

   test_name('Задача 4a, Фибоначчи'(_O_)).
   test_type(problem).

   test(A,B,C,D) :- ^^test(A,B,C,D).

   test(generation_of_fibs,
      all((::mem(N-V, [
           1-1,
           2-1,
           3-2,
           4-3,
           5-5,
           6-8,
           11-89
         ]))),
       [each_test_name(gen_fib_number(N,V)),
          each_explain(::error('Вычисление чисел Фибоначчи некорректно, должно быть, например, ~w::calc(~q,~q)!' +
          [_O_, N,V])),
          condition(success(basic_predicates_defined))
       ],
       ((_O_::calc(N, V)))
     ).

:- end_object.

:- object(test_fib_cache(_O_),
   extends(studyunit)).

   test_name('Задача 4b, Фибоначчи, проверка БД кэша.'(_O_)).
   test_type(problem).

   test(generation_of_fibs,
      all((::mem(N-V, [
           3-2,
           4-3,
           5-5,
           6-8,
           11-89
         ]))),
       [each_test_name(gen_fib_number(N,V)),
          each_explain(::error('Не обнаруживается наличие кэша БД ~w<<cache(~q,~q)!' +
          [_O_, N,V]))
       ],
       ((_O_<<cache(N, V)))
     ).

:- end_object.

:- object(tests,
   extends(studyunit)).
   % debug_level(60).

   test_name('Тестовый набор по теме 2, динамические предикаты.').

   test_type(problem_set).

   test(hp_queries, true,
      [explain(::error('Не все запросы запрограммированы правильно!\nИспользуйте ?- hp_query::query(<название запроса>) для отладки запроса.' +
      []))],
      (hp_queries::ok)).

   test(hp_cast_database, true,
      [],
      ((test_hp_db_mod(harry_potter_movie)::ok))).

   test(freq_dict_test, true,
      [],
      ((test_freq_dict(freq_dict)::ok))).

   test(fibonacci_correct, true,
      [],
      (test_fib(fibonacci, [calc/2 - public])::ok)).

   test(fibonacci_cached_correct, true,
      [condition(success(fibonacci_correct))],
      (test_fib(fibonacci_cached, [cache/2-[protected, dynamic]])::ok)).

   test(fibonacci_cache_exists, true,
      [condition(success(fibonacci_cached_correct))],
      (test_fib_cache(fibonacci_cached)::ok)).

   % Problem 6, Skipped.



:- end_object.
