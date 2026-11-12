% Набор упражнений 9: события, мониторы и перехват сообщений.
%
% Запуск проверки:
%
%   swilgt -l Set9TestsLoader.lgt -g halt

:- set_logtalk_flag(events, allow).

% -----------------------------------------------------
% Объект, за сообщениями которого мы будем наблюдать.
% Этот объект дан и менять его не нужно.

:- object(greeter).

    :- public(hello/1).
    hello(Name) :-
        format('Hello, ~w!~n', [Name]).

    :- public(noop/0).
    noop.

:- end_object.

% -----------------------------------------------------
% Упражнение 1: монитор-трассировщик.
%
% Требования:
% - tracer реализует протокол monitoring;
% - before/3 записывает событие event_(before, Object, Message);
% - after/3 записывает событие event_(after, Object, Message);
% - events/1 возвращает список записанных событий;
% - clear/0 очищает журнал.

:- object(tracer,
    implements(monitoring)).

    :- public(events/1).
    :- public(clear/0).

    :- private(event_/3).
    :- dynamic(event_/3).

    % ВАША РЕАЛИЗАЦИЯ ЗДЕСЬ

:- end_object.

% -----------------------------------------------------
% Упражнение 2: управление подпиской.
%
% Требования:
% - watch/1 подписывает монитор на все события;
% - unwatch/1 отписывает монитор;
% - используйте define_events/5 и abolish_events/5.

:- object(event_control).

    :- public(watch/1).
    :- public(unwatch/1).

    % ВАША РЕАЛИЗАЦИЯ ЗДЕСЬ

:- end_object.

% -----------------------------------------------------
% Упражнение 3: счётчик сообщений.
%
% Требования:
% - counter_monitor реализует monitoring;
% - before/3 увеличивает счётчик на единицу;
% - after/3 ничего не делает;
% - count/1 возвращает текущее значение, 0 если счётчик пуст;
% - reset/0 сбрасывает счётчик.

:- object(counter_monitor,
    implements(monitoring)).

    :- public(count/1).
    :- public(reset/0).

    :- private(count_/1).
    :- dynamic(count_/1).

    % ВАША РЕАЛИЗАЦИЯ ЗДЕСЬ

:- end_object.
