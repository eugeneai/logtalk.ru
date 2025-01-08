% Точно категории.. на примерах ... Символьные вычисления.

% -----------------------------------------------------
% Упражнение 2: Задача состоит в разработке программы
% вычисления производных из формул. В этом упражнении
% разработаем категорию diff для основных операций:
% сложение, вычитание, умножение, деление, степень,
% суперпозиция функций g(f(E)) ...

% -----------------------------------------------------
% Упражнение 1: ... но сначала сформулируем протокол diff_p
% для задания public-метода вычисления производных d/3, где
% d(Формула, Переменная, Производная). Метод d вычисляет
% частную производную.

:- protocol(diff_p).
   :- public(d/3).
   :- mode( d(?any, ?any, ?any), zero_or_one).
   :- info(d/3, [
      comment is 'Вычисляет частную производную (третий аргумент) из первого аргумента по второму.',
      argnames is ['Formula','Variable','Formula']
   ]).
:- end_protocol.

% Продолжение управжнения 1. Предикат d - это функция, точнее
% оператор, функционал (отображает функции в функции).
% Пример формулы: x+y, производную можно брать по переменной
% x, y, ... - d(x+y, x, DE).

:- category(diff_c,
   implements(diff_p)).

   % Производная X по X = 1.
   d(X, X, 1):-!.
   % Производная константы (number/1) = 0.
   d(N, _, 0):- number(N),!.
   % Производная переменной, не X, = 0.
   d(Y, _, 0):- atom(Y),!.
   % Производная суммы.
   d(U+V, X, DU+DV) :-!,
     d(U, X, DU),
     d(V, X, DV).
   % Производная разности.
   d(U-V, X, DU-DV) :-!,
     d(U+V, X, DU+DV).
   % Производная произведения.
   d(U*V, X, DU*V+DV*U) :-!,
     d(U+V, X, DU+DV).
   % Производная отношения.
   d(U/V, X, (DUV-DVU)/(V*V)) :-!,
     d(U*V, X, DUV+DVU).
   % Произвдоная степенной функции E^N,
   % Не забываем про производную E.
   d(E^N, X, N*E^N1*DE) :-!,
     d(E, X, DE),
     N1 is N-1.
   % Производная суперпозиции функций,
   % Используйте univ/2: E =.. [F,A].
   % И пусть df(E, DE) - запрос БД
   % производных функций, реализованных
   % в объекте-приемнике или категории.
   % Не забудьте про производную A по X.
   d(E, X, DE*DA) :-
     E =.. [_,A],!,
     ::df(E,DE),
     d(A,X, DA).

:- end_category.

% -----------------------------------------------------
% Упражнение 3: Спроектировать протокол для объектов -
% баз данных производных функций.

:- protocol(f_diff_p).
   :- public(df/2).
   :- mode(df(?compound, ?compound), zero_or_one).
   :- info(df/2, [
      comment is 'База данных производных функций.',
      argnames is ['Formula', 'Formula']
   ]).

:- end_protocol.

% -----------------------------------------------------
% Упражнение 4: Создание объекта - базы данных производных
% функций.

:- category(f_diff_c,
   implements(f_diff_p)).

   df(ln(E), 1/E).
   df(sin(E), cos(E)).
   df(cos(E), -sin(E)).
   df(exp(E), exp(E)).
   df(tg(E), sec(E)^2).
   df(ctg(E), -(csc(E)^2)).
   df(sec(E), sec(E)*tg(E)).
   df(cosec(E), -(cosec(E)*ctg(E))).
   df(asin(E), 1/sqrt(1-E^2)).
   df(acos(E), -1/sqrt(1-E^2)).
   df(atan(E), 1/sqrt(1+E^2)).
   df(arcctg(E), -1/sqrt(1+E^2)).

:- end_category.

% -----------------------------------------------------
% Упражнение 5: Теперь надо сделать пару объектов -
% протокол reduce_p и категорию reduce_c, предназначенные
% для реализации сервиса сокращения выражений.

:- protocol(reduce_p).
   :- protected(r/2).
   :- public(reduce/2).
:- end_protocol.

% Материал для ознакомления:
% https://www.matznanie.ru/xbookM0001/index.html?go=part-006*page.htm
:- category(reduce_c,
   implements(reduce_p)).

   reduce(A, B) :-
     r(A, A1), !,  % Если удалось сделать шааг сокращения,
     reduce(A1, B).  % повторить операцию.
   reduce(A, A).

   % r/2 истинный, если удалось сделать шаг сокращения.
   % Например, если получилось то же самое, что
   % Сложение 0 с выражением
   r(0 + E, E).
   % Сложение выражения с 0
   r(E + 0, E).
   % Уножение 1 на выражение
   r(1 * E, E).
   % Умножение выражения на 1
   r(E * 1, E).
   % Выражение в степени 1
   r(E^1, E).
   % Сложение с -(E)
   r(A + (-(B)), A-B).
   % Сложение с (-1*E)
   r(A + (-1*B), A-B).
   r(-A, -1*A).   % Оставить
   % Разница двух одинаквых выражений
   r(A-A, 0).
   % Сложение двух одинаковх выражений
   r(A+A, 2*A).
   % Произведение двух одинаковых выражений
   r(A*A, A^2).
   r(A/1, A).
   r(A+N*A, (1+N)*A).  %
   r(A*N, N*A) :- number(N),!. %
   r(N*A*A, N*A^2) :- number(N),!. %
   r((N*A)*A, N*A^2) :- number(N),!. %
   r(A/(-1*B), -1*(A/B)).

   r(cos(E)^2-sin(E)^2, cos(2*E)) :- !. % оставить.
   r(sin(E)/cos(E), atan(E)) :- !. % оставить.
   % r(cos(E)/sin(E), atan(-E)+3.141516/2) :- !. % оставить.
   % r(A^2-B^2, (A+B)*(A-B)).

   % Попробовать просто вычислить выражение
   r(A, B) :-
        catch(
           B is A,
           _,
           fail  % не удается по разным причинам.
        ), B \= A. % Было ли сокращение?

   r((E^M)^N, E^(MN)) :-
     MN = M*N.

   r(sqrt(E^N), E^N1) :-
     integer(N), !,
     N1 = N/2.

   r(E^M/E^N, E^MN) :-
     MN = M-N.

   % Сокращение подвыражения.
   r(A, B) :-
     A =.. [O, A1, A2],
     ( reduce(A1, B1), A1\=B1 -> B =.. [O, B1, A2] ;
       reduce(A2, B2), A2\=B2, B =..[O, A1, B2]).

:- end_category.


% -----------------------------------------------------
% Упражнение 6: Собираем все вместе в один композиционный
% объект calculus, вычисляющий производные.

:- object(calculus,
   imports([diff_c, f_diff_c, reduce_c])).

   :- public(diff/3).
   :- mode(diff(+compound, +any, -compound), zero_or_one).
   :- info(diff/3, [
      comment is 'Вычислитель производных, а также сокращение получаемых выражений.',
      argnames is ['Formula','Variable/Symbol','Formula']
   ]).

   % diff/3 должен вычислить производную и сократить ее.
   diff(Formula, X, DFormula) :-
      ::d(Formula, X, DF),
      ::reduce(DF, DFormula).

:- end_object.

% Тестовый объект для calculus.
:- object(calculus_test,
   extends(studyunit)).

   test_name('Тест исчисления calculus').
   test_type(problem).

   test(calculus_test,
      all((::mem(c(D, V, DF), [
          c(x, x, 1)
        , c(y, x, 0)
        , c(1, x, 0)
        , c(x^2, x, 2*x)
        , c(x^3, x, 3*x^2)
        , c(sin(x) * cos(x), x, cos(2*x))
        % , c(sin(X) * cos(X), X, cos(2*X)) % Продвинутый уровень.
      ]))),
      [each_test_name(calculus_diff(D, DF)),
       each_explain(
        %::error('Производная из выражения ~q должна быть ~q (c учетом сокращения).'+
       %[D, DF])
       true
       )],
      (
       % debugger::trace,
       calculus::diff(D, V, DF1)
       , format('d~q/d~q = ~q (должно быть: ~q)\n', [D, V, DF1, DF])
       , DF1 == DF
       )).

:- end_object.

% -----------------------------------------------------
% Упражнение 7: Теперь попробуем реализовать метод Ньютона
% решения уавнения, используя синвольные вычисления.
%
%         f(x) = 0. % Собственно уравнение.
%
%  Метод Ньютона - последовательное уточнение решения:
%
%                  f(x_n)
%  x_{n+1} = x_n - ----------
%                  df(x_n)/dx
%
% Предлагается а) вычислить производную из формулы f.
% б) попробовать сократить выражение.

:- object(newton,
   extends(calculus)).

   :- public(calc/4).
   :- mode(calc(+compound, +number, -number, +number), zero_or_one).
   :- info(calc/4, [
      comment is 'Численно-аналитический метод Ньютона решения уравнения',
      argnames is ['Function', 'InitialValue', 'Result', 'Epsilon']
   ]).

   :- protected(iter/4).
   :- mode(iter(+compound, +number, -number, +number), zero_or_one).
   :- info(iter/4, [
      comment is 'Численно-аналитический метод Ньютона решения уравнения, основной цикл',
      argnames is ['Formula', 'InitialValue', 'Result', 'Epsilon']
   ]).

   calc(Fun, IniVal, Val, Eps) :-
      ::diff(Fun, x, DFun),
      ::reduce(Fun/DFun, Formula),
      format('Функция ~q, производная ~q, формула ~q\n.',
        [Fun, DFun, Formula]),
      % debugger::trace,
      iter(Formula, IniVal, Val, Eps).

   iter(Formula, V, Vr, Eps) :-
      substitute(Formula, V/x, F),
      % debugger::trace,
      Vn is F,
      ( abs(Vn)>Eps ->
        Vnn is V - Vn,
        format('~w\n',[Vnn]),
        iter(Formula, Vnn, Vr, Eps) ; Vr = V ).

   % :- private(c/3).

   substitute([], _, []) :-!.
   substitute([X|T], Sub, [X1|T1]) :- !,
      substitute(X, Sub, X1),
      substitute(T, Sub, T1).
   substitute(x, X/x, X) :-!.
   substitute(F, Sub, Fs) :-
      F =.. [O|T], !,
      substitute(T, Sub, T1),
      Fs =.. [O|T1].
   substitute(X, _, X).

:- end_object.

% Тест для метода Ньютона.

:- object(newton_test,
   extends(studyunit)).

   test(newton_test,
      all(::mem(c(Formula, X),[
           c(x, 0)
         , c(sin(x), 0)
         , c(cos(x), 3.1415926/2) % Зависит от начального приближения
      ])),
      [each_test_name(apply_newton_to(Formula)),
       each_explain(::error('Не удалось достичь необходимую точность!'+[]))],
      ( Eps = 0.001,
        newton::calc(Formula, 0.5, X1, Eps),
        format('Решение уравнения ~w=0: x=~w.\n', [Formula, X1]),
        abs(X1-X)=<Eps)).

:- end_object.

% -----------------------------------------------------
% Упражнение 8:
