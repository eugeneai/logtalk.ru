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

    all_objects(Objects) :-
        findall(Object, current_object(Object), Objects).

    public_predicates(Object, Predicates) :-
        (   object_property(Object, public(Predicates))
        ->  true
        ;   Predicates = []
        ).

    object_kind(Object, class) :-
        (   specializes_class(Object, _)
        ;   has_instances(Object)
        ), !.
    object_kind(Object, instance) :-
        instantiates_class(Object, _), !.
    object_kind(_, prototype).

    :- private(has_instances/1).
    has_instances(Object) :-
        setof(Instance, instantiates_class(Instance, Object), Instances),
        Instances \= [].

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

    spawn(Class, Instance) :-
        create_object(Instance, [instantiates(Class)], [], []).

    destroy(Instance) :-
        abolish_object(Instance).

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

    make_items(N, Items) :-
        findall(Item,
            (   between(1, N, Index),
                atomic_list_concat([generated_item_, Index], Item),
                item::new(Item)
            ),
            Items).

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

    flag(Name, Value) :-
        current_logtalk_flag(Name, Value).

    set_flag(Name, Value) :-
        set_logtalk_flag(Name, Value).

:- end_object.
