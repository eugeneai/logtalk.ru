% Набор упражнений 8: компоненты и адаптеры.
%
% Запуск проверки:
%
%   swilgt -l Set8TestsLoader.lgt -g halt

:- set_logtalk_flag(unknown_entities, silent).

% -----------------------------------------------------
% Упражнение 1: протокол компонента.
%
% Требования:
% - протокол text_p должен объявлять public text/1.

:- protocol(text_p).

    :- public(text/1).

:- end_protocol.

% -----------------------------------------------------
% Упражнение 2: простой компонент.
%
% Требования:
% - plain_text(_Text_) реализует text_p;
% - text/1 возвращает текст, переданный параметром.

:- object(plain_text(_Text_),
    implements(text_p)).

    text(_Text_).

:- end_object.

% -----------------------------------------------------
% Упражнение 3: адаптер, меняющий регистр.
%
% Требования:
% - uppercase_adapter(_Source_) реализует text_p;
% - text/1 получает текст у _Source_ и переводит его в верхний регистр
%   через upcase_atom/2.

:- object(uppercase_adapter(_Source_),
    implements(text_p)).

    text(Upper) :-
        _Source_::text(Text),
        upcase_atom(Text, Upper).

:- end_object.

% -----------------------------------------------------
% Упражнение 4: прокси-адаптер с префиксом.
%
% Требования:
% - prefix_adapter(_Prefix_, _Source_) реализует text_p;
% - text/1 возвращает текст источника с добавленным префиксом;
% - используйте atomic_list_concat/2.

:- object(prefix_adapter(_Prefix_, _Source_),
    implements(text_p)).

    text(Result) :-
        _Source_::text(Text),
        atomic_list_concat([_Prefix_, Text], Result).

:- end_object.

% -----------------------------------------------------
% Упражнение 5: мультиадаптер.
%
% Требования:
% - composite_text(_Components_) реализует text_p;
% - text/1 собирает тексты всех компонентов и склеивает их;
% - порядок компонентов сохраняется.

:- object(composite_text(_Components_),
    implements(text_p)).

    text(Result) :-
        findall(Text,
            ( lists:member(Component, _Components_),
              Component::text(Text)
            ),
            Texts),
        atomic_list_concat(Texts, Result).

:- end_object.

% -----------------------------------------------------
% Упражнение 6: реестр компонентов.
%
% Требования:
% - register/2 добавляет компонент по имени без дубликатов;
% - get/2 возвращает компонент по имени;
% - names/1 возвращает список имён зарегистрированных компонентов;
% - clear/0 уже реализован и очищает реестр.

:- object(component_registry).

    :- public(register/2).
    :- public(get/2).
    :- public(names/1).

    :- public(clear/0).
    clear :-
        retractall(component_(_, _)).

    :- private(component_/2).
    :- dynamic(component_/2).

    register(Name, Component) :-
        (   component_(Name, _)
        ->  true
        ;   assertz(component_(Name, Component))
        ).

    get(Name, Component) :-
        component_(Name, Component).

    names(Names) :-
        findall(Name, component_(Name, _), Names).

:- end_object.
