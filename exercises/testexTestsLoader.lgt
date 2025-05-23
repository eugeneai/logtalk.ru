
:- initialization((
    % Минимизируем объем вывода ошибок, ограничимся только самыми важными
    % (ошибки, предупреждения)
    set_logtalk_flag(report, warnings),
    set_logtalk_flag(events, allow),
    set_logtalk_flag(debug, on),
    set_prolog_flag(verbose_load, true),
    % Загрузка библитоек, требуемых приложению; например,
    %%% logtalk_load(basic_types(loader)),
    % Загрузка библиотеки тестирования
    logtalk_load(tutor(loader)),
    logtalk_load(tools(loader)),  % debugging, tracing, trace
    logtalk_load(debugger(loader)),  % debugging
    % logtalk_load(lgtunit(loader)),
%	logtalk_load(basic_types(loader)),
%	logtalk_load(os(loader)),
%	logtalk_load(arbitrary(loader)),
	logtalk_load(options(loader)),
	% define a flag to allow the logtalk_tester script to pass the
	% option to suppress the test file and directory path prefix
	create_logtalk_flag(suppress_path_prefix, '', [type(atom), keep(true)]),
	logtalk_load([%library(lists),
                  'testlib/testing'
                  %lgtunit(lgtunit_messages),
                  %'study_messages'
                  ],
         [optimize(on)]),
    % logtalk_load('study_messages', [source_data(on), debug(on)]),
    logtalk_load('studyunit', [source_data(on), debug(on)]),  % Библиотека средств тестирования
    % Загрузка файлов основной программы (например, "source.lgt"), при этом разрешаем
    % оценивать покрытие (code coverage), что требует от компилятора работать в режиме
    % поддержки отладки (debug mode) и сбора данных об исходном коде;
    % Если не требуется оценивать покрытие, опцию "debug(on)" надо убрать,
    % трансляция будет идти быстрее.
    logtalk_load('testex', [source_data(on), debug(on)]),
    % Теперь скомпилируем библиотеку тестирования модулей (unit tests), при этом
    % внесем в нее расширения, исплоьзуя объект-хук (hook) "lgtunit"
    % для осуществления предварительной обработки (preprocess) тестов;
    % если есть тесты, которые не срабатывают (failing tests), можно добавлять
    % режим debug(on) чтобы иметь возможность их отладки (см. "tools/lgtunit/NOTES.md",
    % где изложены разные рекомендации по отладке); тесты следует загружать после
    % исходного кода тестируемых программ, это решает проблемы с предупреждениями,
    % например, "отсутствие объекта", на который ссылается тест.
    logtalk_load('testexTests'),
    % Теперь осуществляем запуск всех тестов; здесь предполагается, что
    % в файле tests.lgt определен объект "tests".
    %tests::runexp,
    %tests::print
    extest(a)::run,
    extest(b)::run,
    extest(c)::run
)).
