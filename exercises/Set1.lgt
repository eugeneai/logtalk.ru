

% Добро пожаловать в первый набор тестов Logtalk!
% Редактируйте этот файл в соотвестствии с инструкциями и
% чтобы проверить, правильно ли вы выпонили задание,
% запускайте
%
% swilgt -l Set1TestLoader.lgt -g halt
%
% Также можно после проверки задания перейти в командную
% строку Logtalk (top-level) и позадавать запросы.
%
% swilgt -l Set1TestLoader.lgt
%
% Данный набор тестов содержит задачи по темам
%
%   - определение объектов
%   - задание предикатов-методов
%   - определение области видимости
%   - наследование объектов

% -----------------------------------------------------
% Упражнение 1: Задайте объект с названием first,
% содержащий два факта, что butsy и flash - кошка (cat) и
% собака, соответственно (dog).
% используйте инструкцию
%
%   :- public([cat/1,dog/1]).
%
% чтобы система тестирования могла получить доступ к
% к вашим животным.

% :- object(first).
%      :- public([dog/1,cat/1]).

%      cat(butsy).
%      dog(flash).

% :- end_object.


% -----------------------------------------------------
% Упражнение 2: Создайте объект second, унаследовав объект
% first. В новом объекте создайте правило animal/1 (животное),
% определяющее, что все кошки и собаки - это животные.
% используйте инструкцию public/1, чтобы указать Logtalk,
% что метод animal/1 - публичный.
% Напоминание: dog/1 и cat/1 - матоды, вызов своего метода
% реализуется оператором ::/1.

:- object(second,
     extends(first)).
     :- public(animal/1).

     animal(X) :- ::dog(X).
     animal(X) :- ::cat(X).

:- end_object.


% -----------------------------------------------------
% Упражнение 3: Реализуйте объект third, скопировав объект
% работоспособный и отлаженный объект first. В данном задании
% необходимо указать, что dog/1 и cat/1 - защищенные методы.

:- object(third).
   :- protected([dog/1,cat/1]).

   cat(butsy).
   dog(flash).

:- end_object.

% -----------------------------------------------------
% Упражнение 4: Реализуйте объект fourth, скопировав объект
% работоспособный и отлаженный объект second.
% Объект должен наследовать third.
% Здесь мы проверим, сможет ли animal/1 получить доступ к
% унаследованным dog/1 и cat/1.

:- object(fourth,
     extends(third)).
     :- public(animal/1).

   animal(X) :- dog(X).
   animal(X) :- ::cat(X).

:- end_object.

% -----------------------------------------------------
% Упражнение 5: Реализуйте объект fifth, унаслевдовав
% объект fourth. В новом объекте задайте
% факт, что star и iron - это лошади (horse).
% Область видимости horse/1 - объекты-наследники.
% задайте правило, что и лошади тоже животные.
% Задайте правило, что кошки и собаки - это
% домашние животные (pet).
% Напоминание: Вызов унаследованного метода осуществляется
% оператором ^^/1.

:- object(fifth,
    extends(fourth)).
    :- protected(horse/1).

    horse(star).
    horse(iron).
    animal(X):- ^^animal(X).
    animal(X):- ::horse(X).

    :- public(pet/1).

    pet(X):- ::dog(X).
    pet(X):- ::cat(X).
:- end_object.

% -----------------------------------------------------
% Упражнение 6: Реализуйте объект sixth, унаслевдовав
% объект fifth. Добавьте в него публичный предикат
% owner/2 (владеет), реализующий следующие высказывания.
% Kate (kate) владеет (owner(kate,...)...) всеми домашними
% животными, а Bob (bob) только лошадью star.

:- object(sixth,
    extends(fifth)).
    :- public(owner/2).

    owner(kate, X):- ::pet(X).
    owner(bob, star):- ::horse(star).
:- end_object.

% -----------------------------------------------------
% Упражнение 6: Реализуйте объект table_animal_printer.
% Теперь нам надо вывести на стандартный вывод (экран)
% таблицу владельцев животных в фромате:
% <имя владельца>:<имя животного>.
% Вышеуказанный вывод должен реализовываться методом
% print/1, единственный параметр которого - это объект
% типа sixth.
% Примечание 1: Для получения всех ответов предиката
% Можно использовать кострукции с fail или forall/2.
% Примечание 2: Печать на экран - смотритее описание format/2.
% Примечание 3: Вызов метода объекта реализуется
% оператором ::/2. Пример - red_hat::walk_to_forest(F).

:- object(table_animal_printer).
   :- public(print/1).

   print(World):-
        forall(World::owner(Owner, Animal),
            format("~w:~w\n",[Owner, Animal])).

:- end_object.


% -----------------------------------------------------
% Упражнение 7: Реализуйте объекты dog_object, cat_object.
% Объекты должны задавать два предиката:
%   - say/1, защищенный (protected)
%   - say_something/0, публичный.
% аргументом say/1 у них должны быть строки "meow" и
% "wow", соответственно.
% say_something/0 должен выводить аргимент say/1 на экран,
% печатать сразу после слов "!!!" и переводить строку (!).
% Примечание: при реализации можно создавать дополнительные
% объекты (или категории), использовать наследование
% (и/или композицию), можно использовать объекты-протоколы.
%
% Пример использования:
% | ?- dog_object::say_something.
% wow!!!
% true

:- object(animal_object).
   :- public(say_something/0).
   say_something:-
        ::say(Something),
        format('~w!!!\n',[Something]).
:- end_object.

:- protocol(say_p).
   :- private(say/1).
:- end_protocol.

:- object(dog_object,
   extends(animal_object),
   implements(say_p)).

   say("wow").

:- end_object.

:- object(cat_object,
   extends(animal_object),
   implements(say_p)).

   say("meow").

:- end_object.

% -----------------------------------------------------
% Упражнение 8: Реализуйте объект taylor инкапсулирующий
% приватный fact/2, истинный, если второй аргумент является
% факториялом первого;
% приватный sqr/2, истинный, если второй аргумент - квадрат
% первого
% публичный exp/3 - разложение функции exp(x) (e^x)
% в ряд Тейлора вокруг точки
%   - x (первый аргумент);
%   - второй аргумент, целое число - количество членов
%     разложения, начиная с 1;
%   - третий аргумент - результат разложения.
% exp(x) = 1 + x/1! + x^2/2! + x^3/3! ...
%          1   2      3        4      ... (количество членов)
% Примечание 1: все методы должны быть детерминированными.
% Примечание 2: exp(_, 0, 0.0) положим истинным.
% Примечание 3: вызов приватных предикатов при помощи ::/1
% порождает замечание (Warning), поэтому не используйте ::/1
% в реализации exp/3.

:- object(taylor).
   :- private([fact/2, sqr/2]).

   fact(0, 1):-!.
   fact(1, 1):-!.
   fact(N, M):-
           N1 is N-1,
           fact(N1, M1), !,
           M is M1 * N.

   sqr(A, B):- B is A*A.

   :- public(exp/3).
   exp(_X, 0, 0.0):-!.
   exp(_X, 1, 1):-!.
   exp(X, N, Y):-
           N>1,!,
           N1 is N-1,
           exp(X, N1, Y1), !,
           fact(N, FN),
           sqr(N, NS),
           D is NS/FN,
           Y is Y1 + D.

:- end_object.

% -----------------------------------------------------
% Упражнение 9: Реализуйте объект evaluator, который
% содержит предикат eval/2, получающий в качестве
% первого параметра выражение вида
%    and(or(t,f),not(and(t,t)))
% второй параметр - вычисленное логическое значение
%    t или f.
% Только eval/2 должен быть публичным и детерминированным.

:- object(evaluator).
   :- public(eval/2).

   eval(t,t):-!.
   eval(f,f):-!.

   eval(and(A, _), f):-
       eval(A, f), !.
   eval(and(_, B), f):-
       eval(B, f), !.
   eval(and(_, _), t):-!.

   eval(or(A, _), t):-
       eval(A, t), !.
   eval(or(_, B), t):-
       eval(B, t), !.
   eval(or(_, _), f):-!.

   eval(not(A), f):-
       eval(A, t), !.
   eval(not(_), t):-!.

:- end_object.

% -----------------------------------------------------
% Упражнение 10: Реализуйте объект door_lock такой, чтобы
% при наследовании от него можно было добавлять новые пароли.
% Публичный метод check/1, который истинный, если
% в списке паролей password/1 имеется пароль, совпадающий с
% аргументом.
% Пароли не должны быть доступны снаружи.
% Примечание: тип данных строк паролей - символы, например
% 'password', 'award_ed', '42', т.е. символы в одинарных
% кавычках.

:- object(door_lock).
   % ....
   :- public(check/1).
   check(X) :- ::password(X).
   :- protected(password/1).
   % ....
   password('default_password').
:- end_object.

:- object(my_door_lock,
   extends(door_lock)).

   password('my-password').
   password('mudmervidzar').
   % password(X) :- ^^password(X).

:- end_object.
