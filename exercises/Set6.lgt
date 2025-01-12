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

:- protocol(ssgh_p). % Граф пространства состояний с данными
   :- public(next/3).  % расстояний между узлами.
   :- public(goal_state/1).
:- end_protocol.

% Потом понадобиться протокол для задания эвристических
% функций.

:- protocol(heuristic_p).
   :- public(h_value/2).
   :- mode(h_value(+list(+compound),-numeric), one).
   :- info(h_value/2, [
      comment is 'Значение эвристической функции, вычесленной из исходных данных, переданных в первом параметре (список)',
      argnames is ['ListOfInputValue', 'H_Value']
   ]).

:- end_protocol.

% -----------------------------------------------------
% Упражнение 9: Зададим контрольный пример из Википедии.

:- object(ssgh_test,
   implements([ssgh_p, heuristic_p])).

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

   h_value([Node], Value) :-
     h(Node, Value),!.

   h(0, 6).
   h(d, 4.5).
   h(a, 4).
   h(e, 2).
   h(c, 4).
   h(b, 2).
   h(g, 0).

:- end_object.

% -----------------------------------------------------
% Упражнение 10: Реализуем алгоритм UCS, Uniform Cost
% Search, поиск с равнозначной стоимостью (поиск в ширину).

:- object(ucs(_SSGH_),
   implements(solution_p)).

   solution(Start, Solution) :-
     ucs([0-['*'-Start]], RSol),
     reverse(RSol,[_|Solution]).

   :- use_module(library(lists), [append/3, member/2,
                                  reverse/2]).

   ucs([_-[X-Y|W]|_], [X-Y|W]) :-
     _SSGH_::goal_state(Y), !.

   ucs([G-[X-Y|W]|Ways], Solution) :-
     findall(GZ-[Y-Z,X-Y|W],
        ( _SSGH_::next(Y,Z, C),
          \+ member(_-Z, [X-Y|W]),
          GZ is G + C
        ), L),
     append(L, Ways, S),
     keysort(S, Solutions),
     ucs(Solutions, Solution).

:- end_object.

% Тестовый объект для эвристических алгоритмов поиска
% решающего пути.
%
% ?- test_halg(ucs(ssgh_test))::solution(<старт-вершина>, Solution).
%

:- object(test_halg(_Alg_),
   extends(studyunit)).

   test_type(problem).
   test_name('Тест эвристических алгоритмов поиска для '(_Alg_)).

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


% -----------------------------------------------------
% Упражнение 11: Реализация алгоритма A*. Алгоритм является
% обобщением ucs. Ключ сортировки - сумма G и значение
% эвристической функции, вычисленной по данным SSG/задачи

:- object(a_star(_SSGH_),
   implements([solution_p])).

   solution(Start, Solution) :-
     astar([0-q(0,['*'-Start])], RSol),
     reverse(RSol,[_|Solution]).

   :- use_module(library(lists), [append/3, member/2,
                                  reverse/2]).

   astar([_-q(_,[X-Y|W])|_], [X-Y|W]) :-
     _SSGH_::goal_state(Y), !.

   astar([_-q(G,[X-Y|W])|Ways], Solution) :-
     findall(FZ-q(GZ,[Y-Z,X-Y|W]),
        ( _SSGH_::next(Y,Z, C),
          \+ member(_-Z, [X-Y|W]),
          GZ is G + C,
          _SSGH_::h_value([Z], HZ),
          FZ is GZ + HZ
        ), L),
     append(L, Ways, S),
     keysort(S, Solutions),
     astar(Solutions, Solution).

:- end_object.

% Усложненный уровень раздела
% ===========================

% Используем разработанные алгоритмы для решения головоломки
% Игра 8 (игра 15 требует намного больших ресурсов)

% Будем поэтапно формулировать SSG для этой задачи.
% -----------------------------------------------------
% Упражнение 12: Сформулировать целевое состояние.
% Один из вариантов представления игрового поля:
% [ [1, 2, 3],
%   [8, 0, 4],
%   [7, 6, 5] ]

% Другой вариант представления игрового поля:
% [ 1, 2, 3,
%   8, 0, 4,
%   7, 6, 5 ]

% Еще вариант представления игрового поля:
%     1  2  3
%   [ 1, 2, 3,           % 1
%     8, 0, 4,           % 2
%     7, 6, 5 ] - 2/2    % 3

% Тесты вашего объекта будут независимы от структур
% данных представления игрового поля, поэтому мы
% не сможем детально контролировать процесс проектирования
% программы. ;-)

:- object(game8_ssgh,
   implements([ssgh_p, heuristic_p])).

   goal_state(
      [ 1, 2, 3,
        8, 0, 4,
        7, 6, 5 ] - 1/1).

% -----------------------------------------------------
% Упражнение 13: Теперь надо сформулировать метод next/3,
% очевидно, что все дуги будут одинаковой "длины".

   next(Begin, End, 0) :-
     move(Direction),
     make_move(Direction, Begin, End).

% Направления перемещения вверх, вниз, вправо влево.

   move(up).
   move(down).
   move(left).
   move(right).

% Далее реализуем перемещение пустого поля 0 согласно
% заданного направления.

  make_move(up, List-R/C, New - R1/C) :-
    R>0, !,
    R1 = R-1,
    find(R1,C, List, Num),
    swap(Num, List, New).

  make_move(down, List-R/C, New - R1/C) :-
    R<2, !,
    R1 = R+1,
    find(R1,C, List, Num),
    swap(Num, List, New).

  make_move(left, List-R/C, New - R/C1) :-
    C>0, !,
    C1 = C-1,
    find(R,C1, List, Num),
    swap(Num, List, New).

  make_move(right, List-R/C, New - R/C1) :-
    C<2, !,
    C1 = C+1,
    find(R,C1, List, Num),
    swap(Num, List, New).

  :- use_module(library(lists), [append/3,
                                 nth0/3,
                                 sum_list/2]).

  swap(Num, Begin, End) :-
    append(B, [Num|T], Begin),
    ( append(A1, [0|T1], B) ->
        append(A1, [Num|T1], B1),
        append(B1, [0|T1], End);
      append(A2, [0|T2], T) ->
        append(A2, [Num|T2], B2),
        append(B,[0|B2], End);
      format('Странно, других вариантов не должно быть!'), fail ).

  find(R,C, List, Num) :-
    var(Num),
    nonvar(List),
    nonvar(R),
    nonvar(C), !,
    Pos is R*3+C,
    nth0(Pos, List, Num).

  find(R,C, List, Num) :-
    nonvar(R),
    nonvar(C),
    nth0(Pos, List, Num),
    divmod(Pos, 3, R, C).  % Only SWI

% -----------------------------------------------------
% Упражнение 14: Теперь осталось сделать эвристическую
% функцию. Значение функции - сумма манхэттеновских
% расстояний местоположения фишек в текущей позиции до
% их позиций в целевом состоянии.

  h_value([Node-_/_], Value) :-
    ::goal_state(Target - _/_), !,
    findall(Diff,
      diff(Node, Target, Diff),
    L), !,
    sum_list(L, Value).

  diff(Curr, Target, Diff) :-
    find(R1, C1, Target, Num),
    Num>0,   % Не считать 0 фишкой.
    find(R2, C2, Curr, Num),
    Diff is abs(R1-R2)+abs(C1-C2).

% -----------------------------------------------------
% Упражнение 15: Для тестирования необходимо сделать
% генератор начального состояния из целевого при помощи
% перемещения 0 раз 100 в случайном направлении.
   :- public(gen_init/2).
   :- mode(gen_init(+integer, -compound), one_or_more).
   :- info(gen_init/2, [
      comment is 'Генерирует допустимое состояние',
      argnames is ['NumberOfMovements', 'PuzzleBoard']
     ]).

   :- use_module(library(random), [random_member/2,
                                   setrand/1]).

   gen_init(0, Target) :-
      ::goal_state(Target).
   gen_init(N, Result) :-
      N>0, !,
      N1 is N-1,
      gen_init(N1, Prev),
      random_member(Movement, [up, down, left, right]),
      make_move(Movement, Prev, Result).

   :- public(init_rand/1).
   :- mode(init_rand(+integer), one).
   :- info(init_rand/1, [
      comment is 'Устанавливает начальное значение генератора случайных чисел',
      argnames is ['InitialValue']
     ]).

   init_rand(Value) :-
      setrand(Value).

:- end_object.

% Тестовый объект для головоломки.
%
% ?- test_puzzle_solver(a_star(game8_ssgh), game8_ssgh)::run.
%

:- object(test_puzzle_solver(_Alg_, _SSGE_),
   extends(studyunit)).

   test_type(problem).
   test_name('Тест решателя головоломок '(_Alg_)).

   test(test_problem_solving_alg,
      all((between(1,10, _), _SSGE_::gen_init(10, Init))),
      [
         each_test_name(test_puzzle_solver(_Alg_, Start)),
         each_explain(::error('Решение не найдено для ~q.' +
           [Init]))
      ],
      ((_Alg_::solution(Init, Solution),
        ::info('Решение: ~q.\n' - [Solution])))).

:- end_object.
