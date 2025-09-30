% В этом наборе задач будем изучать возможность изменять состояние
% объектов, примерно как в обычных языках программирования.
% Состояние объекта меняется при помощи динамических предикатов
% и стандартны для языка программирования Prolog.

% Запуск тестирования:
%
% swilgt -l Set2TestsLoader.lgt -g halt
%

% -----------------------------------------------------
% Упражнение 1: Datalog - язык запросов к базе данных,
% синтаксис которого унаследован с Prolog.
% Задана инкапсулированная в объект 'hp_db' база данных:

:- object(hp_db).
   :- protected([movie/3, char/2]).
   :- dynamic([movie/3, char/2]).

   movie('Philosopher\'s Stone', fs, 2001).
   movie('Chamber of Secrets', cos, 2002).
   movie('Prisoner of Azkaban', poa, 2004).
   movie('Goblet of Fire', gof, 2005).
   movie('Order of the Phoenix', ootf, 2007).
   movie('Half-Blood Prince', hbp, 2009).
   movie('Deathly Hallows – Part 1', dhp1, 2010).
   movie('Deathly Hallows – Part 2', dhp2, 2011).

   char('Katie Bell',       [fs, cos,            hbp, dhp1, dhp2]).
   char('Vincent Crabble',  [fs, cos, pos, gof, ootf, hbp]).
   char('Susan Bones',      [fs, cos]).
   char('Dudley Dursley',   [fs, cos, pos,      ootf,      dhp1]).
   char('Vernon Dursley',   [fs, cos, pos,      ootf,      dhp1]).
   char('March Dursley',             [pos]).
   char('Petunia Dursley',  [fs, cos, pos,      ootf,      dhp1, dhp2]).
   char('Argus Filch',      [fs, cos, pos, gof, ootf, hbp,       dhp2]).
   char('Hermione Granger', [fs, cos, pos, gof, ootf, hbp, dhp1, dhp2]).
   char('Harry Potter',     [fs, cos, pos, gof, ootf, hbp, dhp1, dhp2]).
   char('Rolanda Hooch',    [fs]).
   char('Ernie Prang',               [pos]).

:- end_object.

% Необходимо реализовать следующие запросы (public-предикат):

:- object(hp_query,
   extends(hp_db)).
   :- public(query/2).
   :- mode(query(+symbol, -list(-symbol)), zero_or_more).
   :- info(query/2, [
      comment is 'Поименнованный запрос к базе данных о фильнах вселенной Дж.К. Роулинг',
      argnames is ['Название запроса', 'Ответ']
   ]).

   :- use_module(library(lists), [member/2]).

   % В окончательных вариантах реализации запросов строку
   % to_be_done(....). надо убрать.

   query('Movie with Susan Bones', [MovieName]) :-
      % ВАША РЕАЛИЗАЦИЯ ЗДЕСЬ
      % Найти фильм, в котором снималась Susan Bones
      to_be_done('Movie with Susan Bones').

   query('Movie without Susan Bones', [MovieName]) :-
      % ВАША РЕАЛИЗАЦИЯ ЗДЕСЬ
      % Найти фильм, в котором НЕ снималась Susan Bones
      to_be_done('Movie without Susan Bones').

   query('Character having common series', [Char1, Char2]):-
      % ВАША РЕАЛИЗАЦИЯ ЗДЕСЬ
      % Найти двух персонажей, которые снимались в одних и тех же фильмах
      to_be_done('Персонажи, появляющиеся в одних и тех же фильмах').

   query('Movie having a character', [MovieName, Char]):-
      % ВАША РЕАЛИЗАЦИЯ ЗДЕСЬ
      % Найти фильм и персонажа, который в нем снимался
      to_be_done('В Фильме присутствует Персонаж').

   query('Character appearing only in one Movie', [Char, MovieName]) :-
      % ВАША РЕАЛИЗАЦИЯ ЗДЕСЬ
      % Найти персонажа, который снимался только в одном фильме
      to_be_done('Персонаж, присутствующий только в одном фильме').

   query('Character filmed in a Movie without Dudley Dursley', [Char, MovieName]) :-
      % ВАША РЕАЛИЗАЦИЯ ЗДЕСЬ
      % Найти персонажа и фильм, в котором НЕ снимался Dudley Dursley
      to_be_done('Персонаж Фильма, в котором не появляется Dudley Dursley').

   % Вспомогательный метод для вывода/отладки запросов
   :- public(print/1).
   print(Name) :-
      forall(
         ::query(Name, Row),
         format('~w\n', [Row])).

   to_be_done(Name):-
      format('Требуется реализовать запрос \'~w\'!\n', [Name]).
:- end_object.

% -----------------------------------------------------
% Упражнение 2: В объекте ниже надо объявить protected-
% динамический метод cast/2 и реализовать public-методы
% add/2 и remove/2, таким образом, чтобы сработал тест
% (второй объект).
% Методы add/2 и remove/2 используются для управления
% базой данных актеров. add(Character, Actor) должен
% добавлять или заменять существующую запись.
% remove(Character, Actor) должен удалять конкретную пару
% герой-актер."

:- object(harry_potter_movie).

   % ВАША РЕАЛИЗАЦИЯ ЗДЕСЬ
   % Объявите protected dynamic cast/2
   % Добавьте начальные факты про Harry Potter и Dudley Dursley
   % Реализуйте add/2 для добавления новых ролей
   % Реализуйте remove/2 для удаления ролей

:- end_object.

% Не вносите изменения в тест. Тест запускается из системы
% тестирования.
% Запуск теста из командной строки logtalk:
%
% ?- hp_test::run.
%

:- object(hp_test,
   extends(studyunit)).

   % debug_level(6).    % Включить отладочные сообщения
   % debug_level(60).

   test_name('Harry Potter Cast test').
   test_type(problem).

   test(hermione_first_does_not_exist, fail,
      [explain(::error("Объект '~w' не должен содержать данные о '~w'" +
       [harry_potter_movie, 'Hermione Granger']))],
      harry_potter_movie<<cast('Hermione Granger', 'Emma Whatson')).

   test(dudley_first_exist, fail,
      [explain(::error("Объект '~w' должен содержать данные о '~w'" +
       [harry_potter_movie, 'Dudley Dursley']))],
      harry_potter_movie<<cast('Hermione Granger', 'Emma Whatson')).

   test(add_hermione, true,
      [explain(::error("Объект '~w' не позволяет добавлять новые роли (cast/2)" +
       [harry_potter_movie])),
       condition(success(hermione_first_does_not_exist))],
      (
       harry_potter_movie::add('Hermione Granger', 'Emma Whatson'),
       harry_potter_movie<<cast('Hermione Granger', 'Emma Whatson'))).

   test(remove_dudley, true,
      [explain(::error("Объект '~w' не позволяет удалять роли (cast/2)" +
       [harry_potter_movie])),
       condition(success(dudley_first_exist))],
      (
       harry_potter_movie::remove('Dudley Dursley', _),
       \+ harry_potter_movie<<cast('Dudley Dursley', _))).

:- end_object.

% -----------------------------------------------------
% Упражнение 3: Частотный словарь.
% Разработать объект freq_dict, подсчитывающий количество
% слов, с тремя public-методами:
% add/1 - получает слово (например, car) и добавляет его
% в частотный словарь, т.е., увеличивает на 1 количество
% слов 'car' в словаре.
% list(Word, Number) - показывает какое количество соответствует
% слову Word.
% print/0 - печатает словарь на экран.
% Порядок следования слов в базе данных не важно, как
% не важно и как вы хранить будете данные о словах.

:- object(freq_dict).
   :- public([add/1, list/2, print/0]).

   % ВАША РЕАЛИЗАЦИЯ ЗДЕСЬ
   % Объявите private dynamic word/2
   % Реализуйте add/1 - добавляет слово или увеличивает счетчик
   % Реализуйте list/2 - показывает частоту слова
   % Реализуйте print/0 - выводит весь словарь

:- end_object.


% -----------------------------------------------------
% Упражнение 4: Задача состоит в том, чтобы создать
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

   % ВАША РЕАЛИЗАЦИЯ ЗДЕСЬ
   % Реализуйте рекурсивный алгоритм вычисления чисел Фибоначчи

:- end_object.

% -----------------------------------------------------
% Упражнение 5: Теперь надо реализовать "кэширование"
% результата - ранее вычисленные значения будем записывать
% в локальную базу данных объекта. Теперь перед тем, как
% запускать вычисление очередного числа, надо проверить
% наличие числа в базе данных.
% Кэширование реализовать при помощи динамического
% protected-метода cache/2.

:- object(fibonacci_cached,
  extends(fibonacci)).

   % ВАША РЕАЛИЗАЦИЯ ЗДЕСЬ
   % Объявите protected dynamic cache/2
   % Переопределите calc/2 для использования кэша

:- end_object.

% -----------------------------------------------------
% Упражнение 6: Следующий объект 'setup', при помощи
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
% Требования:
% * опции задаются внутри объекта-наследника или через set_option
% * получение значений настроек - предикаты current_option/1, current_option/2
% * поддерживать форматы: Name(Value), Name=Value, Name-Value

:- object(setup).
   :- public([set_option/1, set_option/2, current_option/1, current_option/2]).

   % ВАША РЕАЛИЗАЦИЯ ЗДЕСЬ
   % Объявите protected dynamic option_/1
   % Реализуйте set_option для различных форматов
   % Реализуйте current_option для различных форматов

:- end_object.

% Примеры использования.
% Замечание: Примеры не менять, используются во время
% тестирования.

:- object(my_setup,
  extends(setup)).

  option(prog_name1, program_one).
  option(prog_name2=program_two).
  option(prog_name3-program_three).

:- end_object.


:- object(my_setup_ext).
  option(prog_name(program)).
:- end_object.

% -----------------------------------------------------
% Упражнение 7: Разработаем программу - самообучающуюся
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

   :- private([node/3, leave/1, root/1]).
   :- dynamic([node/3, leave/1, root/1]).

  % node(нет-поддерево, вопрос, да-поддерево).

  % leave(ответ_ЭС).

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

  update(yes, _).
  update(no, Name) :-
     format("Тогда что это?: ", []),
     get_answer(What),
     format("Чем отличается ~w от ~w?: ", [What, Name]),
     get_answer(Question),
     update(Name, What, Question).

  % ВАША РЕАЛИЗАЦИЯ ЗДЕСЬ
  % Реализуйте update/3 для обучения системы
  % Name - это имя листовго узла, предположение ЭС
  % What - это имя нового листового узла,
  %        нечто, задуманное пользователем.
  % Question - это утверждение отличающее What от
  %        Name.

  update(Name, What, Question) :-
      % 1. Перенаправить существующий узел
      % 2. Создать лист для What
      % 3. Создать узел объединяющий Name, Question, What
      % 4. Обновить root если нужно
      format('Требуется реализовать обучение системы!~n').

  :- public(print/0).
  :- info(print/0, [
     comment is 'Печатает правила ЭС'
  ]).

  print :-
    ( root(R) -> format('Начальная вершина root(~q).\n', [R]) ;
                 format('Куда-то делась начальная вершина. А должна быть!\n') ),
    format('Список узлов\n------------\n'),
    forall( node(N, Q, Y),
      format('node(~q,~q,~q).\n', [N, Q, Y]) ),
    format('Список листовых вершин\n----------------------\n'),
    forall( leave(Name),
      format('leave(~q).\n',[Name]) ).

% Распечатка списка узлов ЭС из командной строки logtalk
% forall(expert_system<<node(N, Q, Y), format("node(~q,~q,~q).\n", [N, Q, Y])).

:- end_object.

% -----------------------------------------------------
% Упражнение 8: Программирование при помощи типовых
% конфигураций. Задача вычисления наибольшего общего делителя.
% Алгоритм Евклида:
% 1. Найти два числа X и Y, таких, что X > Y.
% 2. Заменить большее на разность большего и меньшего.
% В противном случае распечатать число.
% Перед началом работы необходимо инициализировать базу чисел:
% gcd::assertz(number(42)), gcd::assertz(number(56)).
% Реализуйте правило subtract, которое находит два различных числа
% и заменяет большее на их разность.

:- object(gcd).
   :- protected(number/1).
   :- dynamic(number/1).
   :- protected(rule/1).

   % ВАША РЕАЛИЗАЦИЯ ЗДЕСЬ
   % Реализуйте правило subtract для вычитания
   % Реализуйте правило print для вывода результата

   :- public(run/0).
   run :-
      rule(Name), !,
      (Name == print ->
       true ;
       run).

:- end_object.

% -----------------------------------------------------
% Упражнение 9: Редуктор простых выражений
% (задание будет добавлено в следующей версии)

:- object(expression_reducer).
   :- public(reduce/2).

   % ЗАДАНИЕ БУДЕТ ДОБАВЛЕНО

:- end_object.

% -----------------------------------------------------
% Упражнение 10: Построение остового дерева минимальной
% стоимости. Алгоритм Прима.
%   На вход алгоритма подаётся связный неориентированный
% граф. Для каждого ребра задаётся его стоимость.
%   Сначала берётся произвольная вершина и находится ребро,
% инцидентное данной вершине и обладающее наименьшей
% стоимостью. Найденное ребро и соединяемые им две
% вершины образуют дерево. Затем, рассматриваются рёбра
% графа, один конец которых — уже принадлежащая дереву
% вершина, а другой — нет; из этих рёбер выбирается
% ребро наименьшей стоимости. Выбираемое на каждом шаге
% ребро присоединяется к дереву. Рост дерева происходит
% до тех пор, пока не будут исчерпаны все вершины
% исходного графа.
%   Результатом работы алгоритма является остовное дерево
% минимальной стоимости.
%
% Подсказка - полезные предикаты: встроенные findall/3,
% setof/3, bagof/3, keysort/2.

:- object(graph).
   % Множество ребер
   :- private(edge/3).
   % :- dynamic(edge/3).
   :- mode(edge(?atom, ?atom, ?atom), zero_or_more).
   :- info(edge/3, [
      comment is 'Ребро графа с заданной стоимостью',
      argnames is ['Vertex', 'Vertex', 'Cost']
   ]).

   % Множество вершин не будем определять, т.к. граф связный.

   % Учитывайте при реализации, что граф *неориентированный*!
   edge(a, b, 7).
   edge(a, c, 8).
   edge(c, e, 5).
   edge(e, g, 9).
   edge(g, f, 11).
   edge(f, d, 6).
   edge(d, a, 5).
   edge(d, b, 9).
   edge(b, e, 7).
   edge(e, f, 8).
   edge(d, e, 15).

   :- public(arc/3).
   arc(A,B,C) :-
      edge(A,B,C).
   arc(A,B,C) :-
      edge(B,A,C).

:- end_object.

:- object(prim_tree,
   extends(graph)).

   :- protected(edge/2).
   :- dynamic(edge/2).

   :- public([add/2, add/1, build/0, clear_db/0]).

   % ВАША РЕАЛИЗАЦИЯ ЗДЕСЬ
   % Реализуйте add/2 для добавления ребра в дерево
   % Реализуйте add/1 для добавления вершины
   % Реализуйте build/0 для построения остового дерева
   % Реализуйте clear_db/0 для очистки базы данных

   :- private(vertex/1).
   :- dynamic(vertex/1).

:- end_object.
