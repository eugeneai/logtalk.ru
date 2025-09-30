% ПРАКТИЧЕСКАЯ РАБОТА 3: ПАРАМЕТРИЧЕСКИЕ ОБЪЕКТЫ В LOGTALK
% ======================================================

% В этой практической работе мы освоим мощный инструмент Logtalk -
% параметрические объекты. Вы научитесь создавать переиспользуемые
% компоненты, работающие с различными типами данных, проектировать
% конечные автоматы и синтаксические анализаторы.

% ЗАПУСК ТЕСТИРОВАНИЯ:
% swilgt -l Set3TestsLoader.lgt -g halt

% -----------------------------------------------------
% УПРАЖНЕНИЕ 1: Протокол для геометрических фигур
%
% ЗАДАЧА: Сформулируйте протокол parameter_p, который будет
% определять общий интерфейс для всех геометрических фигур.
%
% ТРЕБОВАНИЯ:
% - Протокол должен объявлять два публичных метода:
%   • area/1 - вычисление площади фигуры
%   • perimeter/1 - вычисление периметра фигуры
% - Протокол должен быть reusable (пригоден для повторного использования)
%
% ПЛАН РЕШЕНИЯ:
% 1. Создайте протокол с помощью :- protocol(...)
% 2. Объявите необходимые публичные методы
% 3. Убедитесь, что протокол можно имплементировать разными объектами

:- protocol(parameter_p).
   :- public(area/1).
   :- public(perimeter/1).
:- end_protocol.

% -----------------------------------------------------
% УПРАЖНЕНИЕ 2: Параметрические объекты для геометрических фигур
%
% ЗАДАЧА: Создайте параметрические объекты circle и rectangle,
% которые реализуют протокол parameter_p.
%
% ТРЕБОВАНИЯ:
% - circle(_Radius_) должен вычислять площадь и периметр круга
% - rectangle(_Width_, _Height_) должен вычислять площадь и периметр прямоугольника
% - Оба объекта должны имплементировать протокол parameter_p
% - Используйте математические формулы:
%   • Площадь круга: π × R²
%   • Периметр круга: 2 × π × R
%   • Площадь прямоугольника: Width × Height
%   • Периметр прямоугольника: 2 × (Width + Height)
%
% ПЛАН РЕШЕНИЯ:
% 1. Создайте объект circle с параметром радиуса
% 2. Реализуйте методы area/1 и perimeter/1
% 3. Создайте объект rectangle с параметрами ширины и высоты
% 4. Реализуйте соответствующие методы
% 5. Убедитесь, что оба объекта implement parameter_p

:- object(circle(_Radius_),
   implements(parameter_p)).

   area(Sq) :-
     Sq is _Radius_ * _Radius_ * 3.14.

   perimeter(Per) :-
     Per is _Radius_ * 2 * 3.14.

:- end_object.

:- object(rectangle(_Width_, _Height_),
   implements(parameter_p)).

   area(Sq) :-
     _Width_ * _Height_.

   perimeter(Per) :-
     Per is (_Width_ + _Height_) * 2.

:- end_object.

% Вот тестовый объект для фигур
% ?- test_fig_objects::run.

:- object(test_fig_objects,
   extends(studyunit)).

   test_name('Тест объектов - геометрических фигур').
   test_type(problem).

   test(figure_parameters,
      all((::object_list(L),::mem(Obj-q(Sq, Per)))),
       [],
       ((Eps = 0.001,
         Obj::area(Sq1),
         Obj::perimeter(Per1),
         abs(Sq1 - Sq) < Eps,
         abs(Per1 - Per) < Eps
         ))).

   :- protected(object_list/1).
   object_list([
          circle(2)-q(12.5663, 12.5663),
          rectangle(1,2)-q(2, 6)
        ]).

:- end_object.

% -----------------------------------------------------
% УПРАЖНЕНИЕ 3: Наследование параметрических объектов
%
% ЗАДАЧА: Создайте объект square, унаследовав его от rectangle.
% Квадрат - это частный случай прямоугольника с равными сторонами.
%
% ТРЕБОВАНИЯ:
% - square(_Side_) должен наследовать от rectangle
% - Параметры должны корректно передаваться в родительский объект
% - Не нужно переопределять методы area/1 и perimeter/1
%
% ПЛАН РЕШЕНИЯ:
% 1. Создайте объект square с одним параметром (сторона)
% 2. Унаследуйте от rectangle, передав параметр дважды
% 3. Убедитесь, что методы работают корректно

:- object(square(_Width_),
   extends(rectangle(_Width_, _Width_))).

:- end_object.

% ?- test_fig_objects_ex::run.

:- object(test_fig_objects_ex,
   extends(test_fig_objects)).

   object_list(
      [
        square(2) - a(4, 8)
      |L]) :-
      ::object_list(L).

:- end_object.

% -----------------------------------------------------
% УПРАЖНЕНИЕ 4: Коллекция фигур с динамическим управлением
%
% ЗАДАЧА: Создайте объект figures, который управляет коллекцией
% геометрических фигур и вычисляет суммарные площадь и периметр.
%
% ТРЕБОВАНИЯ:
% - Объект должен имплементировать parameter_p
% - Методы area/1 и perimeter/1 должны вычислять СУММЫ по всем фигурам
% - Используйте findall/3 для агрегации результатов
%
% ПЛАН РЕШЕНИЯ:
% 1. Объявите private dynamic предикаты для хранения фигур
% 3. В area/1 и perimeter/1 используйте findall для сбора результатов от всех фигур
% 4. Суммируйте результаты с помощью вспомогательного предиката sum_list/2

:- object(figures,
   implements(parameter_p)).

   :- private([circle/1, rectangle/2, square/1]).

   circle(2).
   circle(3).

   rectangle(2,4).
   rectangle(4,5).

   square(2).
   square(8).

   area(Total) :-
      findall(Area, (
          (circle(R), {circle(R)}::area(Area));
          (rectangle(W, H), {rectangle(W, H)}::area(Area));
          (square(S), {square(S)}::area(Area))
      ), Areas),
      sum_list(Areas, Total).

   perimeter(Total) :-
      findall(Per, (
          (circle(R), {circle(R)}::perimeter(Per));
          (rectangle(W, H), {rectangle(W, H)}::perimeter(Per));
          (square(S), {square(S)}::perimeter(Per))
      ), Pers),
      sum_list(Pers, Total).

   :- private(sum_list/2).
   sum_list([], 0).
   sum_list([X|T], Sum) :-
      sum_list(T, Rest),
      Sum is X + Rest.

:- end_object.

% -----------------------------------------------------
% УПРАЖНЕНИЕ 5: Абстрактный конечный автомат
%
% ЗАДАЧА: Создайте абстрактный параметрический объект automaton,
% который может быть основой для различных конечных автоматов.
%
% ТРЕБОВАНИЯ:
% - Параметр: начальное состояние
% - protected предикат q/4 для определения переходов
% - public предикат model/3 для моделирования работы автомата
% - model/3 должен быть ложным для непринимаемых последовательностей
%
% СПЕЦИФИКАЦИЯ ПРЕДИКАТОВ:
% q(CurrentState, InputSymbol, NextState, OutputSymbol)
% model(InputList, OutputList, FinalState)
%
% ПЛАН РЕШЕНИЯ:
% 1. Создайте параметрический объект automaton
% 2. Объявите protected q/4
% 3. Реализуйте model/3 с рекурсивной обработкой входной последовательности
% 4. Добавьте protected model/4 для рекурсивной реализации

:- object(automaton(_InitialState_)).
   :- protected(q/4).
   :- mode(q(+symbol, +symbol, +symbol, +symbol)).
   :- info(q/4, [
      comment is 'Задает переход из одного состояния, имея на входе символ, выдавая на выход другой символ',
      argnames is ['CurState', 'InputSymbol','NewState','OutputSymbol']
   ]).

   :- public(model/3).
   :- mode(model(?list[?symbol], ?list[?symbol], ?symbol)).
   :- info(model/2, [
      comment is 'Моделирует переход из состояния в состояние соглавно входному потоку и генерирует выходной',
      argnames  is ['InputSybolList', 'OutputSymbolList', 'TerminateState']
   ]).

   model(I, O, S) :-
     ::model(_InitialState_, I, O, S).

   :- protected(model/4).

   model(S, [], [], S). % Нет вхдоных символов.
   model(S, [X|T],[Y|L], Q) :-
      ::q(S,X, Y,Q1),
      model(Q1, T,L, Q).

:- end_object.

% -----------------------------------------------------
% УПРАЖНЕНИЕ 6: Автомат для чисел с плавающей запятой
%
% ЗАДАЧА: Создайте конкретный автомат, распознающий числа
% в инженерном формате: [+-]?[0-9]+(\.[0-9]*)?([eE][+-]?[0-9]+)?
%
% ТРЕБОВАНИЯ:
% - Наследуйте от automaton
% - Определите состояния: start, integer, fraction, exponent, exponent_sign, exponent_digit, accept
% - Реализуйте переходы между состояниями согласно формату
% - Автомат должен принимать корректные числа и отвергать некорректные
%
% ПЛАН РЕШЕНИЯ:
% 1. Создайте объект, наследующий от automaton
% 2. Определите все необходимые состояния
% 3. Реализуйте q/4 для каждого возможного перехода
% 4. Укажите конечные состояния (accept)

:- object(float_automaton(_Initial_),
   extends(automaton(_Initial_))).

   :- protected(q/4).

   % Состояния: start, integer, fraction, exponent, exponent_sign, exponent_digit, accept
   q(start, Char, integer, '') :-
      char_type(Char, digit).
   q(start, '+', start, '').
   q(start, '-', start, '').

   q(integer, Char, integer, Char) :-
      char_type(Char, digit).
   q(integer, '.', fraction, '.').
   q(integer, 'e', exponent, '').
   q(integer, 'E', exponent, '').

   q(fraction, Char, fraction, Char) :-
      char_type(Char, digit).
   q(fraction, 'e', exponent, '').
   q(fraction, 'E', exponent, '').

   q(exponent, '+', exponent_sign, '').
   q(exponent, '-', exponent_sign, '').
   q(exponent, Char, exponent_digit, Char) :-
      char_type(Char, digit).

   q(exponent_sign, Char, exponent_digit, Char) :-
      char_type(Char, digit).

   q(exponent_digit, Char, exponent_digit, Char) :-
      char_type(Char, digit).

   % Конечные состояния
   q(integer, end, accept, '').
   q(fraction, end, accept, '').
   q(exponent_digit, end, accept, '').

:- end_object.

% -----------------------------------------------------
% УПРАЖНЕНИЕ 7: Генератор валидных последовательностей
%
% ЗАДАЧА: Создайте генератор, который produces все валидные
% последовательности, принимаемые автоматом из упражнения 6.
%
% ТРЕБОВАНИЯ:
% - Наследуйте от float_automaton
% - public generate/1 - генерирует последовательности символов
% - public generate_valid/1 - генерирует только валидные числа (как атомы)
% - generate_valid/1 должен проверять, что последовательность действительно является числом
%
% ПЛАН РЕШЕНИЯ:
% 1. Создайте объект, наследующий от float_automaton
% 2. Используйте model/3 для генерации последовательностей
% 3. В generate_valid/1 преобразуйте в атом и проверьте через atom_number/2
% 4. Используйте catch/3 для обработки ошибок преобразования

:- object(float_generator(_Initial_),
   extends(float_automaton(_Initial_))).

   :- public(generate/1).
   :- mode(generate(-list), zero_or_more).

   generate(Sequence) :-
      ::model(Sequence, _, accept).

   :- public(generate_valid/1).
   generate_valid(Atom) :-
      generate(Sequence),
      atom_chars(Atom, Sequence),
      catch(atom_number(Atom, _), _, fail). % Проверяем, что это валидное число

:- end_object.

% -----------------------------------------------------
% УПРАЖНЕНИЕ 8: Арифметический автомат-транслятор
%
% ЗАДАЧА: Создайте автомат, который не только распознает, но и
% вычисляет простые арифметические выражения без скобок.
%
% ТРЕБОВАНИЯ:
% - Распознавать выражения вида: "3+4*2"
% - Вычислять результат выражения
% - Поддерживать операции: +, -, *, /
% - public evaluate/2 должен возвращать числовой результат
%
% ПЛАН РЕШЕНИЯ:
% 1. Наследуйте от automaton
% 2. Определите состояния для чисел и операторов
% 3. Реализуйте evaluate/2, который преобразует вход в число и вычисляет
% 4. Создайте вспомогательный предикат для разбора выражения

:- object(arithmetic_automaton(_Initial_),
   extends(automaton(_Initial_))).

   :- protected(q/4).

   % Состояния: start, number, operator, accept
   q(start, Char, number, Char) :-
      char_type(Char, digit).

   q(number, Char, number, Char) :-
      char_type(Char, digit).
   q(number, Op, operator, Op) :-
      member(Op, ['+', '-', '*', '/']).
   q(number, end, accept, '').

   q(operator, Char, number, Char) :-
      char_type(Char, digit).

   :- public(evaluate/2).
   evaluate(Input, Result) :-
      ::model(InputChars, _, accept),
      atom_chars(InputAtom, InputChars),
      parse_expression(InputAtom, Expression),
      Result is Expression.

   :- private(parse_expression/2).
   parse_expression(Atom, Result) :-
      atom_chars(Atom, Chars),
      parse_expression(Chars, 0, Result).

   parse_expression([], Acc, Acc).
   parse_expression([Op, Num|Rest], Acc, Result) :-
      char_type(Num, digit),
      number_chars(Number, [Num]),
      (   Op = '+' -> NewAcc is Acc + Number
      ;   Op = '-' -> NewAcc is Acc - Number
      ;   Op = '*' -> NewAcc is Acc * Number
      ;   Op = '/' -> NewAcc is Acc / Number
      ),
      parse_expression(Rest, NewAcc, Result).
   parse_expression([Num|Rest], Acc, Result) :-
      char_type(Num, digit),
      number_chars(Number, [Num]),
      parse_expression(Rest, Number, Result).

:- end_object.

% -----------------------------------------------------
% УПРАЖНЕНИЕ 9: Синтаксический анализатор английского языка
%
% ЗАДАЧА: Создайте параметрический синтаксический анализатор
% для простого подмножества английского языка.
%
% ГРАММАТИКА:
% <sentence> ::= <noun_group> <verb_group>
% <noun_group> ::= <determinant> <noun>
% <verb_group> ::= <verb> <noun_group>
% <determinant> ::= a | an | the | my
% <noun> ::= cow | tail
% <verb> ::= walks | shakes
%
% ТРЕБОВАНИЯ:
% - Параметр: лексикон языка
% - Предикаты разбора должны быть трехаргументными (Input, Output, Structure)
% - public parse/2 должен преобразовывать предложение в синтаксическое дерево
% - Дерево должно отражать структуру: sent(noun_group, verb_group)
%
% ПЛАН РЕШЕНИЯ:
% 1. Создайте параметрический объект parser
% 2. Реализуйте предикаты для каждого правила грамматики
% 3. Используйте DCG-подобный стиль с двумя дополнительными аргументами
% 4. Дерево разбора должно быть вложенной структурой

:- object(parser(_Lexic_)).
   :- protected(sentence/3).    % <предложение>
   :- protected(nonun_group/3). % <группа существительного>
   :- protected(verb_group/3).  % <глагольная группа>
   :- protected(determinant/3).
   :- protected(noun/3).
   :- protected(verb/3).
   :- public(parse/2).   % транслирует Sentence в структуру

   parse(Sentence, Struct) :-
      ::sentence(Sentence, [], Struct).

   sentence(I, O, sent(NG, VG)) :-
      ::noun_group(I, R, NG),
      ::verb_group(R, O, VG).

   noun_group(I, O, ng(D, N)) :-
      ::determinant(I, R, D),
      ::noun(R, O, N).

   verb_group(I, O, vg(V, NG)) :-
      ::verb(I, R, V),
      ::noun_group(R, O, NG).

   noun([Word | O], O, noun(Word)) :-
      _Lexic_::noun(Word).

   verb([Word | O], O, verb(Word)) :-
      _Lexic_::verb(Word).

   determinant([Word | O], O, det(Word)) :-
      _Lexic_::determinant(Word).

:- end_object.


% -----------------------------------------------------
% УПРАЖНЕНИЕ 10: Лексикон языка
%
% ЗАДАЧА: Создайте иерархию объектов, определяющих лексику
% для синтаксического анализатора.
%
% ТРЕБОВАНИЯ:
% - Базовый объект gen_lexic с общими детерминантами
% - Объект cow_lexic с конкретными существительными и глаголами
% - Все слова должны быть размечены частями речи
%
% ПЛАН РЕШЕНИЯ:
% 1. Создайте gen_lexic с детерминантами
% 2. Создайте cow_lexic, наследующий от gen_lexic
% 3. Добавьте существительные и глаголы для "cow"-языка

% Лексика, общая для всех "английских" языков
:- object(gen_lexic).
   :- public(determinant/1).  % Артикли, местоименя
   :- public(noun/1).   % существительные
   :- public(verb/1).   % глаголы

   determinant(a).
   determinant(an).
   determinant(the).
   determinant(my).
   determinant(your).

:- end_object.

% Лексический состав 'cow'-языка
:- object(cow_lexic,
   extends(gen_lexic)).

   noun(cow).
   noun(tail).

   verb(shakes).
   verb(walks).

:- end_object.

% Тестирование cow-языка.

:- object(test_cow_lang,
   extends(studyunit)).

   test_name('Тест cow-языка').
   test_type(problem).

   test(test_cow_shakes_tail, true,
      [],
      ((parser(cow_lexic)::translate([a, cow, shakes, the, tail], Struct),
         format('Sent:~q\n', [Struct])))).
% Добавить сравнение с результатом.
:- end_object.


% -----------------------------------------------------
% УПРАЖНЕНИЕ 11: Генератор предложений
%
% ЗАДАЧА: Используйте синтаксический анализатор как генератор
% корректных предложений языка.
%
% ТРЕБОВАНИЯ:
% - Наследуйте от parser(cow_lexic)
% - public generate/1 должен генерировать все возможные предложения
% - Предложения должны соответствовать грамматике
%
% ПЛАН РЕШЕНИЯ:
% 1. Создайте объект, наследующий от parser с конкретным лексиконом
% 2. Используйте sentence/3 в режиме генерации
% 3. generate/1 должен находить все возможные предложения

:- object(cow_lang,
   extends(parser(cow_lexic))).
   :- public(generate/1).

   generate(Sentence) :-
      ::translate(Sentence, _).

:- end_object.

% ?- cow_lang::generate(Sentence).

% -----------------------------------------------------
% УПРАЖНЕНИЕ 12: Балансировка двоичного дерева
%
% ЗАДАЧА: Создайте объект для балансировки двоичных деревьев поиска.
%
% ТРЕБОВАНИЯ:
% - public balance/2 преобразует несбалансированное дерево в сбалансированное
% - public is_balanced/1 проверяет сбалансированность дерева
% - public height/2 вычисляет высоту дерева
% - Представление: tree(Value, Left, Right) или nil
%
% АЛГОРИТМ:
% 1. Преобразовать дерево в отсортированный список (in-order)
% 2. Построить сбалансированное дерево из отсортированного списка
% 3. Дерево сбалансировано, если разница высот поддеревьев ≤ 1
%
% ПЛАН РЕШЕНИЯ:
% 1. Реализуйте flatten_tree/2 для преобразования в список
% 2. Реализуйте build_balanced_tree/2 для построения из отсортированного списка
% 3. Реализуйте height/2 и is_balanced/1

:- object(tree_balancer).

   :- public(balance/2).
   :- mode(balance(+term, -term), one).

   % Представление дерева: tree(Value, Left, Right) или nil
   balance(nil, nil).
   balance(tree(Value, Left, Right), Balanced) :-
      flatten_tree(tree(Value, Left, Right), List),
      sort(List, Sorted),
      build_balanced_tree(Sorted, Balanced).

   :- private(flatten_tree/2).
   flatten_tree(nil, []).
   flatten_tree(tree(Value, Left, Right), List) :-
      flatten_tree(Left, LeftList),
      flatten_tree(Right, RightList),
      append(LeftList, [Value|RightList], List).

   :- private(build_balanced_tree/2).
   build_balanced_tree([], nil).
   build_balanced_tree(List, tree(Middle, Left, Right)) :-
      length(List, Len),
      Half is Len // 2,
      length(LeftList, Half),
      append(LeftList, [Middle|RightList], List),
      build_balanced_tree(LeftList, Left),
      build_balanced_tree(RightList, Right).

   :- public(height/2).
   height(nil, 0).
   height(tree(_, Left, Right), Height) :-
      height(Left, H1),
      height(Right, H2),
      Height is max(H1, H2) + 1.

   :- public(is_balanced/1).
   is_balanced(nil).
   is_balanced(tree(_, Left, Right)) :-
      height(Left, H1),
      height(Right, H2),
      abs(H1 - H2) =< 1,
      is_balanced(Left),
      is_balanced(Right).

:- end_object.
