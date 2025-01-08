% Мир кубиков.

% -----------------------------------------------------
% Упражнение 1: Протокол решения задач (problem solving)
% Необходимо задачть протокол public-методов для определения
% допустимых переходов состояний next/2 (after зарезервирован
% Loktalk), и распознавания целевых состояний goal_state/1
% (goal - стандартный встроенный предикат, его мы использовать
% не можем). Протокол назоаем ssg_p (state space graph).

:- protocol(ssg_p).
   :- public(next/2).
   :- public(goal_state/1).
:- end_protocol.

% Второй протокол - протокол метода решения - solution_p.
% Протокол содержит один згидшс-метод solution/2, первый
% аргумент насальное состояние, второй - путь до одного
% из целевых.

:- protocol(solution_p).
   :- public(solution/2).
:- end_protocol.

% -----------------------------------------------------
% Упражнение 2: Управжнения будут представлять различные
% стратегии и методы поиска решения - путь из начальной
% вершины в одну из целевых. Путь с различными свойствами.

% Данный объект реализует простейший поиск в глубину,
% реализует solution_p.

:- object(depth_first(_SSG_),
   implements(solution_p)).

   % Если исходная вершина и есть целевая, никуда ни надо ходить.
   solution(X, []) :-
     _SSG_::goal_state(X).
   % Если исходная вершина не является целевой, то
   % сделать шаг в новую вершину, построить решающий
   % путь из новой вершины.
   solution(X, [X-Y|T]) :-
     \+ _SSG_::goal_state(X),
     _SSG_::next(X, Y),
     solution(Y, T).

:- end_object.

% -----------------------------------------------------
% Упражнение 3: Задайте объект со структурой лабиринта
% labyrinth. next/2 задает переходы между комнтатами.
% goal_state/1 задает одину целевую вершину.
% Переходы e/2 есть между комнатами:
% a-b, b-c, c-f, f-e, e-d, d-h, h-i, i-g, g-f.
% Двигаться можно в обоих направлениях через проходы.
% Целевое состояние - g.

:- object(labyrinth,
   implements(ssg_p)).

   :- protected(e/2).

   e(a,b).
   e(b,c).
   e(c,f).
   e(f,e).
   e(e,d).
   e(d,h).
   e(h,i).
   e(i,g).
   e(g,f).

   next(A,B) :- e(A,B).
   next(A,B) :- e(B,A).

   goal_state(g).

:- end_object.

% Будем использовать тест для проверки различных алгоритмов.
% Запускать:
%
% ?- test_alg(depth_first(labyrinth))::solution(<старт-вершина>, Solution).
%

:- object(test_alg(_Alg_),
   extends(studyunit)).

   test_type(problem).
   test_name('Тест алгоритмов поиска для '(_Alg_)).

   test(test_problem_solving_alg,
      all(::mem(Start-Solution,
        [
          i-[i-g],
          c-[c-f,f-g],
          a-[a-b,b-c,c-f,f-g]
        ])),
      [
         each_test_name(test_alg(_Alg_, Start)),
         each_explain(::error('Алгоритм ~q не нашел путь из ~q, который должен быть таким: ~q.' +
           [_Alg_, Start, Solution]))
      ],
      ((_Alg_::solution(Start, Solution)))).
      % Вариант для отладки.
      % ((_Alg_::solution(Start, GSolution),
      %   (Solution == GSolution -> true;
      %    ::info('Несовпадение результатов:\n~q (получено),\n~q должно быть\nАлгоритм:~q.' +
      %      [GSolution, Solution, _Alg_]))))).

:- end_object.

% -----------------------------------------------------
% Упражнение 4: Поиск в глубину с распознаванием циклов.

:- object(depth_first_wo_cycles(_SSG_),
   implements(solution_p)).

   :- private(solution/3).
   :- mode(solution(?atom, ?list(+compound), +list(+atom)), zero_or_more).
   :- info(solution/3, [
      comment is 'Построение решающего пути в глубину с распознаванием циклов.',
      argnames is ['StartState', 'Solution', 'ListOfState']
   ]).

   solution(X, Solution) :-
     solution(X, Solution, []).

   % Если исходная вершина и есть целевая, никуда ни надо ходить.
   solution(X, [], _) :-
     _SSG_::goal_state(X).
   % Если исходная вершина не является целевой, то
   % сделать шаг в новую вершину, построить решающий
   % путь из новой вершины.
   :- use_module(library(lists), [member/2]).
   solution(X, [X-Y|T], L) :-
     \+ _SSG_::goal_state(X),
     _SSG_::next(X, Y),
     \+ member(Y, L),
     solution(Y, T, [Y|L]).

:- end_object.


% -----------------------------------------------------
% Упражнение 5: Поиск в глубину с ограничением по глубине.

:- object(depth_first_contracted(_SSG_, _Depth_),
   implements(solution_p)).

   :- private(solution/3).
   :- mode(solution(?atom, ?list(+compound), +integer), zero_or_more).
   :- info(solution/3, [
      comment is 'Построение решающего пути, стратегия в глубину с ограничением по глубине.',
      argnames is ['StartState', 'Solution', 'Depth']
   ]).

   solution(X, Solution) :-
     solution(X, Solution, _Depth_).

   % Если исходная вершина и есть целевая, никуда ни надо ходить.
   solution(X, [], _) :-
     _SSG_::goal_state(X).
   % Если исходная вершина не является целевой, то
   % сделать шаг в новую вершину, построить решающий
   % путь из новой вершины.
   solution(X, [X-Y|T], N) :-
     N > 0,
     \+ _SSG_::goal_state(X),
     _SSG_::next(X, Y),
     N1 is N-1,
     solution(Y, T, N1).

:- end_object.

% -----------------------------------------------------
% Упражнение 6: Поиск в глубину с итеративным погружением.

:- object(depth_first_iterative(_SSG_, _MaxDepth_),
   implements(solution_p)).

   :- private(solution/3).
   :- mode(solution(?atom, ?list(+compound), +integer), zero_or_more).
   :- info(solution/3, [
      comment is 'Построение решающего пути, стратегия в глубину с ограничением по глубине с итеративным погружением.',
      argnames is ['StartState', 'Solution', 'Depth']
   ]).

   solution(X, Solution) :-
     solution(X, Solution, 0).

   solution(X, Solution, N) :-
     solution1(X, Solution, N).
   solution(X, Solution, N) :-
     N < _MaxDepth_, !,
     N1 is N+1,
     solution(X, Solution, N1).

   % Если исходная вершина и есть целевая, никуда ни надо ходить.
   solution1(X, [], _) :-
     _SSG_::goal_state(X).
   % Если исходная вершина не является целевой, то
   % сделать шаг в новую вершину, построить решающий
   % путь из новой вершины.
   solution1(X, [X-Y|T], N) :-
     N > 0,
     \+ _SSG_::goal_state(X),
     _SSG_::next(X, Y),
     N1 is N-1,
     solution(Y, T, N1).

:- end_object.

% -----------------------------------------------------
% Упражнение 7: Поиск в ширину. Задача - реализовать поиск
% решения, организовав поиск в ширину с сохранением
% путей-кандидатов в виде списков.

:- object(breadth_first(_SSG_),
    implements(solution_p)).

   :- protected(bf/2).

   :- use_module(library(lists), [append/3, member/2, reverse/2]).

   solution(X, []) :-
     % debugger::trace,
     _SSG_::goal_state(X), !.
   solution(X, Solution) :-
     bf([['*'-X]], Solution1),
     reverse(Solution1, ['*'-X|Solution]).

   bf([[X-Y|W]|_], [X-Y|W]) :-
     _SSG_::goal_state(Y),!.

   bf([[X-Y|W]|Ways], Solution) :-
     findall([Y-Z,X-Y|W],
        (_SSG_::next(Y,Z), \+ member(_-Z, [X-Y|W])), L),
     append(Ways, L, Solutions),
     bf(Solutions, Solution).

:- end_object.

% -----------------------------------------------------
% Упражнение 8: Теперь перейдем к эвристическим вариантам
% стратегий поиска. Понадобиться новые протоколы,
% алгоритмы (стратегии) и данные.

:- protocol(ssge_p). % Граф пространства состояний с данными
   :- public(next/3).  % расстояний между узлами.
   :- public(goal_state/1).
:- end_protocol.

% Потом понадобиться протокол для задания эвристических
% функций.

:- protocol(heuristic_p).
   :- public(h/2).
:- end_protocol.

% -----------------------------------------------------
% Упражнение 9: Зададим контрольный пример из Википедии.

:- object(ssge_test,
   implements(ssge_p)).

   :- protected(e/3).

   e(0,d, 2).
   e(d,e, 3).
   e(e,g, 2).
   e(c,g, 4).
   e(b,c, 3).
   e(a,b, 2).
   e(0,a, 1.5).

   next(A, B, C) :-
     e(A, B, C).
   next(B, A, C) :-
     e(A, B, C).

   goal_state(g).

:- end_object.

% -----------------------------------------------------
% Упражнение 10: Реализуем алгоритм UCS, Uniform Cost
% Search, поиск с равнозначной стоимостью (поиск в ширину).

:- object(ucs(_SSGE_),
   implements(solution_p)).

   solution(Start, Solution) :-
     ucs([0-['*'-Start]], RSol),
     reverse(RSol,[_|Solution]).

   :- use_module(library(lists), [append/3, member/2,
                                  reverse/2]).

   ucs([_-[X-Y|W]|_], [X-Y|W]) :-
     _SSGE_::goal_state(Y), !.

   ucs([G-[X-Y|W]|Ways], Solution) :-
     findall(GZ-[Y-Z,X-Y|W],
        ( _SSGE_::next(Y,Z, C),
          \+ member(_-Z, [X-Y|W]),
          GZ is G + C
        ), L),
     append(L, Ways, S),
     keysort(S, Solutions),
     ucs(Solutions, Solution).

:- end_object.

% Тестовый объект для эвристических алгоритмов поиска
% решающего пути.

:- object(test_halg(_Alg_),
   extends(studyunit)).

   test_type(problem).
   test_name('Тест алгоритмов поиска для '(_Alg_)).

   test(test_problem_solving_alg,
      all(::mem(Start-Solution,
        [
          0-[0-d,d-e,e-g]
          % , c-[c-f,f-g]
          % , a-[a-b,b-c,c-f,f-g]
        ])),
      [
         each_test_name(test_alg(_Alg_, Start)),
         each_explain(::error('Алгоритм ~q не нашел путь из ~q, который должен быть таким: ~q.' +
           [_Alg_, Start, Solution]))
      ],
      ((_Alg_::solution(Start, Solution)))).
%      Вариант для отладки.
      % ((_Alg_::solution(Start, GSolution),
      %   (Solution == GSolution -> true;
      %    ::info('Несовпадение результатов:\n~q (получено),\n~q должно быть\nАлгоритм:~q.' +
      %      [GSolution, Solution, _Alg_]))))).

:- end_object.
