
:- initialization((
    set_logtalk_flag(report, warnings),
    set_logtalk_flag(events, allow),
    set_logtalk_flag(debug, on),
    set_prolog_flag(verbose_load, true),
    logtalk_load(tutor(loader)),
    logtalk_load(tools(loader)),  % debugging, tracing, trace
    logtalk_load(debugger(loader)),  % debugging
    logtalk_load(options(loader)),
    create_logtalk_flag(suppress_path_prefix, '', [type(atom), keep(true)]),
    logtalk_load(['testlib/testing'], [source_data(on), debug(on)]),
    logtalk_load('studyunit', [source_data(on), debug(on)]),
    logtalk_load('Set12', [source_data(on), debug(on)]),
    logtalk_load('Set12Tests', [context_switching_calls(allow), source_data(on),debug(on)]),
    tests::run
)).
