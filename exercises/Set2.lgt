% В этом наборе задач будем изучать возможность изменять состояние
% объектов, примерно как в обычных языках программирования.
% Состояние объекта меняется при помощи динамических предикатов
% и стандартны для языка программирования Prolog.

% Запуск тестирования:
%
% swilgt -l Set2TestLoader.lgt -g halt
%

% -----------------------------------------------------
% Упражнение 1: Задача состоит в том, чтобы создать
% объект fibonacci, в котором реализовать public-метод
% calc/2, истинный, если второй аргумент - n-е число
% Фибоначчи. Первый аргумент - это n. Т.е.
%
% ?- fibonacci::calc(5,5).
% true.
%
% Можно использовать и рекурсивный и итеративный алгоритм,
% а также вспомогательные предикаты.
% Ряд Фибоначчи: 1, 1, 2, 3, 5, 8, ... A, B,   A+B, ...
%             n: 1, 2, 3, 4, 5, 6, ... n, n+1, n+2, ...


:- object(fibonacci).
  :- public(calc/2).

  calc(1, 1) :- !.
  calc(2, 1) :- !.
  calc(N, V) :-
    N1 is N-1,
    N2 is N-2,
    ::calc(N1, V1),
    ::calc(N2, V2),
    V is V1+V2.

:- end_object.

% -----------------------------------------------------
% Упражнение 2: Теперь надо реализовать "кэширование"
% результата - ранее вычисленные значения будем записывать
% в локальную базу данных объекта. Теперь перед тем, как
% запускать вычисление очередного числа, надо проверить
% наличие числа в базе данных.
% Кэширование реализовать при помощи динамического
% protected-метода cache/2.

:- object(fibonacci_cached,
  extends(fibonacci)).

  :- protected(cache/2).
  :- dynamic(cache/2).

  calc(N, M) :-
     cache(N, M), !.
  calc(N, M) :-
     ^^calc(N, M),
     assertz(cache(N, M)).

:- end_object.

% -----------------------------------------------------
% Упражнение 3: Следующий объект 'setup', при помощи
% которого будем в объекта-наследниках хранить настройки
% для программы/системы и т.п.
% Требования следующие:
%   * опции (настройки) задаются или внутри
%     объекта-наследника (статические, неизменяемые
%     настройки)
%       option(+ <Name>, +<Value>).
%       option(+ <Name>-<Value>).
%       option(+ <Name>=<Value>).
%     или зпросами "снаружи"
%       setup::set_option(+ <Name>, +<Value>).
%       setup::set_option(+ <Name>-<Value>).
%       setup::set_option(+ <Name>=<Value>).
%     Все запросы равносильны, все запросы должны
%     устанавлитвать ТОЛЬКО ОДНО соответствие
%     Name->Value.
%   * получение значений настроек - предикаты
%     current_option/1, current_option/2.
%     Также должны кооректно выполняться запросы
%       setup::current_option(? <Name>, ?<Value>).
%       setup::current_option(? <Name>-<Value>).
%       setup::current_option(? <Name>=<Value>).
%
% Усложнение задания: Сделать, чтобы поддерживался
% современный стандарт <Name>(<Value>) без изменения
% формата хранения настроек в локальной базе данных.

:- object(setup).
%   :- protected([option/2, option/1]).
%   :- public([current_option/2, current_option/1]).
%   :- protected([option_/1]).

%   set_option(Name=Value) :-
%     retractall(Name=Value),
%     assertz(option_(Name=Value)).
%   set_option(Name-Value) :-
%     set_option(Name=Value).
%   set_option(Name, Value) :-
%     ::option(Name=Value).

%   current_option(Name=Value) :-
%     option_(Name=Value).
%   current_option(Name=Value) :-
%     option(Name=Value).
%   current_option(Name=Value) :-
%     option(Name-Value).
%   current_option(Name=Value) :-
%     option(Name,Value).
%   current_option(Name-Value) :-
%     current_option(Name=Value).
%   current_option(Name, Value).

:- end_object.

% Примеры использования.
% Замечание: Примеры не менять, используются во время
% тестирования.

:- object(my_setup,
  extends(setup)).

  % option(prog_name1, program_one).
  % option(prog_name2=program_two).
  % option(prog_name3-program_three).

:- end_object.


:- object(my_setup_ext).
  % option(prog_name(program)).
:- end_object.

% -----------------------------------------------------
% Упражнение 4: Сейчас разработаем программу - самообучающуюся
% "экспертную" систему ЭС.
% 1. В самом начале экспертная система знает только "Зайка".
% 2. Пользователь загадывает предмет/объект/героя мультика
%    в том числе и Зайку.
% 3. Эксперная система анализирует дерево знаний и задает
%    вопросы. В зависимости от ответа (да/нет) выбирает
%    поддерево и т.д.
% 4. Дойдя до листовой вершины, ЭС пишет на экран ответ
%    (значение в листовой вершине).
% 5. Если ситема угадала загаданнй объект, то сеанс закончен.
% 6. Если не угадала, то запрашивает у пользователя ответы на два вопроса:
%    а) что звгадал пользователь.
%    б) чем отличается загаданный объект от того, который
%       был предложен ЭС.
% Ваша задача реализовать предикаты обновления дерева.

:- object(expert_system).
  :- public(session/0).
  :- protected(session/1).

  :- private(node/3).
  :- dynamic(node/3).

  % node(нет-поддерево, вопрос, да-поддерево).

  :- private(leave/1).
  :- dynamic(leave/1).

  % leave(ответ_ЭС).

  :- private(root/1).
  :- dynamic(root/1).

  % root(корневой вопрос или ответ).

  root('Зайка').
  leave('Зайка').

  session :-
     ::root(Name),
     session(Name).

  session(Name) :-
     ::leave(Name), !,
     format("Это ~w? (да/нет):", [Name]),
     get_yn_answer(Answer),
     update(Answer, Name).

  session(Question) :-
     node(No, Question, Yes),
     format('~w (да/нет):', [Question]),
     get_yn_answer(Answer),
     (
        Answer == yes -> session(Yes) ;
        Answer == no -> session(No) ;
        format('Непонятный ответ: \'~w\'.\n', [Answer]),
        session(Question)).

  :- use_module(library(readutil), [read_line_to_string/2]).
  :- use_module(library(lists), [member/2]).

  get_yn_answer(Answer) :-
     get_answer(A),
     (
       member(A, [y, yes, yay, 'д', 'да']) -> Answer = yes;
       member(A, [n, no, nay, 'н', 'нет']) -> Answer = no ;
       format("Ответ должен быть да или нет. \n"),
       get_yn_answer(Answer)
     ).

  get_answer(Answer) :-
     read_line_to_string(user_input, S),
     atom_string(Answer,S).


  % Реализуйте update здесь.
  update(yes, _).
  update(no, Name) :-
     format("Тогда что это?: ", []),
     get_answer(What),
     format("Чем отличается ~w от ~w?: ", [What, Name]),
     get_answer(Question),
     update(Name, What, Question).

  update(Name, What, Question) :-
     (root(Name) ->
       retractall(root(Name)),
       assertz(root(Question)) ; true),
     (node(Name, PQ1, Yes) ->
        retractall(node(Name, PQ1, Yes)),
        assertz(node(Question, PQ1, Yes));
      node(No, PQ2, Name) ->
        retractall(node(No, PQ2, Name)),
        assertz(node(No, PQ2, Question));
      true),
     assertz(node(Name, Question, What)),
     assertz(leave(What)).

% forall(expert_system<<node(N, Q, Y), format("node(~q,~q,~q).\n", [N, Q, Y])).

:- end_object.




% -----------------------------------------------------
% Упражнение 5: Создайте объект second, унаследовав объект

% -----------------------------------------------------
% Упражнение 6: Создайте объект second, унаследовав объект

% -----------------------------------------------------
% Упражнение 7: Создайте объект second, унаследовав объект

% -----------------------------------------------------
% Упражнение 8: Создайте объект second, унаследовав объект

% -----------------------------------------------------
% Упражнение 9: Создайте объект second, унаследовав объект

% -----------------------------------------------------
% Упражнение 10: Создайте объект second, унаследовав объект

% -----------------------------------------------------
% Упражнение 11: Создайте объект second, унаследовав объект
