% Набор упражнений 10: DCG и экспертные системы.
%
% Запуск проверки:
%
%   swilgt -l Set10TestsLoader.lgt -g halt

% -----------------------------------------------------
% Упражнение 1: грамматика приветствия.
%
% Требования:
% - greeting//1 должно разбирать список [hello, Name] и связывать Name.
% - используйте DCG-правило greeting(Name) --> [hello, Name].

:- object(greeting_parser).

    :- public(greeting//1).

    greeting(Name) --> [hello, Name].

:- end_object.

% -----------------------------------------------------
% Упражнение 2: арифметический разбор.
%
% Требования:
% - number//1 разбирает одно число;
% - sum//1 разбирает последовательность number '+' number и вычисляет сумму.

:- object(arithmetic_parser).

    :- public(number//1).
    :- public(sum//1).

    number(N) --> [N], { number(N) }.

    sum(N) -->
        number(A),
        ['+'],
        number(B),
        { N is A + B }.

:- end_object.

% -----------------------------------------------------
% Упражнение 3: простая экспертная система.
%
% Факты о животных даны. Реализуйте solve/2:
% - cat, если у животного есть шерсть и оно мурлычет;
% - dog, если у животного есть шерсть и оно лает;
% - bird, если у животного есть перья;
% - mammal, если у животного есть шерсть (если не cat и не dog).

:- object(animal_expert).

    :- public(solve/2).

    has(fluffy, hair).
    has(fluffy, purrs).
    has(rex, hair).
    has(rex, barks).
    has(tweety, feathers).
    has(tweety, flies).

    solve(Animal, cat) :-
        has(Animal, hair),
        has(Animal, purrs), !.

    solve(Animal, dog) :-
        has(Animal, hair),
        has(Animal, barks), !.

    solve(Animal, bird) :-
        has(Animal, feathers), !.

    solve(Animal, mammal) :-
        has(Animal, hair).

:- end_object.
