:- initialization((
    set_logtalk_flag(report, warnings),
    set_logtalk_flag(debug, on),
    set_prolog_flag(verbose_load, true),
    logtalk_load(tutor(loader)),
    logtalk_load(tools(loader)),
    logtalk_load(debugger(loader)),
    logtalk_load(options(loader)),
    logtalk_load('testlib/testing', [source_data(on), debug(on)]),
    logtalk_load('studyunit', [source_data(on), debug(on)]),
    logtalk_load('Set8', [source_data(on), debug(on)]),
    logtalk_load('Set8Tests', [context_switching_calls(allow), source_data(on), debug(on)]),
    tests::run
)).
