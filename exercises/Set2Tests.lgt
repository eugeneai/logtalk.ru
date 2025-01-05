
:- object(hp_queries,
   extends(studyunit)).

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


:- object(tests,
   extends(studyunit)).
   debug(20).
   test_name('Тестовый набор по теме 2, динамические предикаты.').

   test_type(problem_set).

   test(hp_queries, true,
      [explain(::error('Не все запросы запрограммированы правильно!\nИспользуйте ?- hp_query::query(<название запроса>) для отладки запроса.' +
      []))],
      (hp_queries::ok)).

:- end_object.
