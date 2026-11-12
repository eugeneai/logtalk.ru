% Набор упражнений 5: классы и экземпляры.
%
% Запуск проверки:
%
%   swilgt -l Set5TestsLoader.lgt -g halt
%
% В этом наборе уже заданы корневые классы и две категории для хранения
% состояния экземпляров. Ваша задача — реализовать классы и объекты,
% отмеченные комментарием "ВАША РЕАЛИЗАЦИЯ".

:- set_logtalk_flag(unknown_entities, silent).

% -----------------------------------------------------
% Корневые объекты иерархии классов.
% Это заготовка, менять её не нужно.

:- object(object,
    instantiates(class)).
:- end_object.

:- object(abstract_class,
    instantiates(class),
    specializes(object)).
:- end_object.

:- object(class,
    instantiates(class),
    specializes(abstract_class)).

    % Создаёт динамический экземпляр класса, отправившего сообщение.
    :- public(new/1).
    new(Instance) :-
        self(Class),
        create_object(Instance, [instantiates(Class)], [], []).

:- end_object.

% -----------------------------------------------------
% Категория для хранения счётчика в каждом экземпляре отдельно.
% Обратите внимание на вызовы ::/1: они работают в контексте
% объекта, импортировавшего категорию.

:- category(count_state).

    :- public(set_count/1).
    :- public(get_count/1).

    :- private(count_/1).
    :- dynamic(count_/1).

    set_count(Count) :-
        ::retractall(count_(_)),
        ::assertz(count_(Count)).

    get_count(Count) :-
        ::count_(Count).

:- end_category.

% -----------------------------------------------------
% Категория для хранения имени экземпляра.

:- category(name_state).

    :- public(set_name/1).
    :- public(get_name/1).

    :- private(name_/1).
    :- dynamic(name_/1).

    set_name(Name) :-
        ::retractall(name_(_)),
        ::assertz(name_(Name)).

    get_name(Name) :-
        ::name_(Name).

:- end_category.

% -----------------------------------------------------
% Упражнение 1: класс counter.
%
% Требования:
% - counter должен быть классом: instantiates(class);
% - counter должен специализировать object;
% - counter импортирует count_state;
% - public value/1 возвращает текущее значение счётчика;
% - public increment/0 увеличивает счётчик на единицу;
% - начальное значение счётчика — 0, если значение ещё не задано.

:- object(counter,
    instantiates(class),
    specializes(object),
    imports(count_state)).

    :- public(value/1).
    value(Value) :-
        (   ::get_count(Value)
        ->  true
        ;   Value = 0
        ).

    :- public(increment/0).
    increment :-
        (   ::get_count(Count)
        ->  true
        ;   Count = 0
        ),
        Next is Count + 1,
        ::set_count(Next).

:- end_object.

% -----------------------------------------------------
% Упражнение 2: класс step_counter.
%
% Требования:
% - step_counter должен быть классом;
% - step_counter специализирует counter;
% - public increment_by/1 увеличивает счётчик на заданное
%   неотрицательное число, используя унаследованный increment/0;
% - increment_by(0) ничего не меняет.

:- object(step_counter,
    instantiates(class),
    specializes(counter)).

    :- public(increment_by/1).
    increment_by(0) :- !.
    increment_by(Count) :-
        Count > 0,
        ::increment,
        Rest is Count - 1,
        increment_by(Rest).

:- end_object.

% -----------------------------------------------------
% Упражнение 3: класс person.
%
% Требования:
% - person должен быть классом;
% - person специализирует object;
% - person импортирует name_state;
% - public name/1 возвращает имя экземпляра;
% - public greet/1 связывает с аргументом приветствие вида
%   'Hello, <имя>!'.

:- object(person,
    instantiates(class),
    specializes(object),
    imports(name_state)).

    :- public(name/1).
    name(Name) :-
        ::get_name(Name).

    :- public(greet/1).
    greet(Greeting) :-
        ::name(Name),
        format(atom(Greeting), 'Hello, ~w!', [Name]).

:- end_object.

% -----------------------------------------------------
% Упражнение 4: статический экземпляр alice класса person.
%
% Требования:
% - alice должен быть экземпляром person;
% - при загрузке файла имя alice должно быть установлено в alice;
% - после загрузки должны работать запросы alice::name(Name) и
%   alice::greet(Greeting).

:- object(alice,
    instantiates(person)).

    :- initialization(::set_name(alice)).

:- end_object.

% -----------------------------------------------------
% Упражнение 5: объект registry — реестр экземпляров.
%
% Требования:
% - registry хранит экземпляры в динамическом предикате person_/1;
% - public add/1 добавляет экземпляр в реестр, не создавая дубликатов;
% - public all/1 возвращает список всех экземпляров;
% - public remove/1 удаляет экземпляр из реестра;
% - public count/1 возвращает количество экземпляров;
% - clear/0 уже реализован и очищает реестр.

:- object(registry).

    :- public(add/1).
    :- public(all/1).
    :- public(remove/1).
    :- public(count/1).

    :- public(clear/0).
    clear :-
        retractall(person_(_)).

    :- private(person_/1).
    :- dynamic(person_/1).

    add(Person) :-
        (   person_(Person)
        ->  true
        ;   assertz(person_(Person))
        ).

    all(Persons) :-
        findall(Person, person_(Person), Persons).

    remove(Person) :-
        retract(person_(Person)).

    count(Count) :-
        findall(_, person_(_), Persons),
        length(Persons, Count).

:- end_object.

% -----------------------------------------------------
% Упражнение 6: фабрика динамических экземпляров.
%
% Требования:
% - public spawn/2 создаёт экземпляр заданного класса с заданным именем;
% - public destroy/1 уничтожает динамический экземпляр;
% - spawn/2 должен использовать create_object/4;
% - destroy/1 должен использовать abolish_object/1.

:- object(instance_factory).

    :- public(spawn/2).
    :- public(destroy/1).

    spawn(Class, Instance) :-
        create_object(Instance, [instantiates(Class)], [], []).

    destroy(Instance) :-
        abolish_object(Instance).

:- end_object.
