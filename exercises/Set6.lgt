% Набор упражнений 6: саморефлексия и генераторы экземпляров.
%
% Запуск проверки:
%
%   swilgt -l Set6TestsLoader.lgt -g halt

:- set_logtalk_flag(unknown_entities, silent).

% -----------------------------------------------------
% Минимальная иерархия классов (дана).

:- object(class,
    instantiates(class)).

    :- public(new/1).
    new(Instance) :-
        self(Class),
        create_object(Instance, [instantiates(Class)], [], []).

:- end_object.

:- object(item,
    instantiates(class),
    specializes(class)).

    :- public(id/1).
    id(Id) :-
        self(Id).

:- end_object.

:- object(prototype_item).

    :- public(kind/1).
    kind(prototype).

:- end_object.

% -----------------------------------------------------
% Упражнение 1: инспектор объектов.
%
% Требования:
% - all_objects/1 возвращает список всех загруженных объектов;
% - public_predicates/2 возвращает список public-предикатов объекта
%   (пустой список, если публичных предикатов нет);
% - object_kind/2 возвращает prototype, class или instance:
%   * class — объект специализирует другой объект или имеет экземпляры;
%   * instance — объект является экземпляром, но не классом;
%   * prototype — остальные объекты.

:- object(inspector).

    :- public(all_objects/1).
    :- public(public_predicates/2).
    :- public(object_kind/2).

    % ВАША РЕАЛИЗАЦИЯ ЗДЕСЬ

:- end_object.

% -----------------------------------------------------
% Упражнение 2: фабрика динамических экземпляров.
%
% Требования:
% - spawn/2 создаёт динамический экземпляр указанного класса;
% - destroy/1 уничтожает динамический экземпляр;
% - используйте create_object/4 и abolish_object/1.

:- object(object_factory).

    :- public(spawn/2).
    :- public(destroy/1).

    % ВАША РЕАЛИЗАЦИЯ ЗДЕСЬ

:- end_object.

% -----------------------------------------------------
% Упражнение 3: генератор экземпляров.
%
% Требования:
% - make_items/2 создаёт N динамических экземпляров класса item;
% - имена экземпляров: generated_item_1, generated_item_2, ...;
% - второй аргумент — список созданных объектов в порядке номеров.

:- object(generator).

    :- public(make_items/2).

    % ВАША РЕАЛИЗАЦИЯ ЗДЕСЬ

:- end_object.

% -----------------------------------------------------
% Упражнение 4: управление флагами компилятора.
%
% Требования:
% - flag/2 возвращает текущее значение флага Logtalk;
% - set_flag/2 устанавливает новое значение флага.

:- object(compiler).

    :- public(flag/2).
    :- public(set_flag/2).

    % ВАША РЕАЛИЗАЦИЯ ЗДЕСЬ

:- end_object.
