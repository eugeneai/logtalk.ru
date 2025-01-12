
:- initialization((
    % Минимизируем объем вывода ошибок, ограничимся только самыми важными
    % (ошибки, предупреждения)
    set_logtalk_flag(report, warnings),
    % Загрузка библитоек, требуемых приложению; например,
    %%% logtalk_load(basic_types(loader)),
    % Загрузка библиотеки тестирования
    logtalk_load(lgtunit(loader)),
    % Загрузка файлов основной программы (например, "source.lgt"), при этом разрешаем
    % оценивать покрытие (code coverage), что требует от компилятора работать в режиме
    % поддержки отладки (debug mode) и сбора данных об исходном коде;
    % Если не требуется оценивать покрытие, опцию "debug(on)" надо убрать,
    % трансляция будет идти быстрее.
    logtalk_load(source, [source_data(on), debug(on)]),
    % Теперь скомпилируем библиотеку тестирования модулей (unit tests), при этом
    % внесем в нее расширения, исплоьзуя объект-хук (hook) "lgtunit"
    % для осуществления предварительной обработки (preprocess) тестов;
    % если есть тесты, которые не срабатывают (failing tests), можно добавлять
    % режим debug(on) чтобы иметь возможность их отладки (см. "tools/lgtunit/NOTES.md",
    % где изложены разные рекомендации по отладке); тесты следует загружать после
    % исходного кода тестируемых программ, это решает проблемы с предупреждениями,
    % например, "отсутствие объекта", на который ссылается тест.
    logtalk_load(tests, [hook(lgtunit)]),
    % Теперь осуществляем запуск всех тестов; здесь предполагается, что
    % в файле tests.lgt определен объект "tests".
    tests::run
)).