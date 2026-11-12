:- object(test_set9,
   extends(studyunit)).

   test_name('Набор 9 - События и перехват сообщений').
   test_type(problem_set).

   test(A, B, C, D) :- ^^test(A, B, C, D).

   test(tracer_api, true,
      [ explain(::error('tracer должен реализовывать monitoring и иметь public events/1, clear/0.')) ],
      ( current_object(tracer),
        implements_protocol(tracer, monitoring),
        object_property(tracer, public(Predicates)),
        lists:member(events/1, Predicates),
        lists:member(clear/0, Predicates) )).

   test(event_control_api, true,
      [ explain(::error('event_control должен иметь public watch/1 и unwatch/1.')) ],
      ( current_object(event_control),
        object_property(event_control, public(Predicates)),
        lists:member(watch/1, Predicates),
        lists:member(unwatch/1, Predicates) )).

   test(tracer_records_events, true,
      [ condition(success(tracer_api)),
        condition(success(event_control_api)),
        explain(::error('tracer должен записывать before и after события для greeter::hello/1.')) ],
      ( catch(event_control::unwatch(tracer), _, true),
        catch(tracer::clear, _, true),
        event_control::watch(tracer),
        greeter::hello(world),
        tracer::events(Events),
        lists:member(event(before, greeter, hello(world)), Events),
        lists:member(event(after, greeter, hello(world)), Events),
        event_control::unwatch(tracer),
        catch(tracer::clear, _, true) )).

   test(unwatch_stops_events, true,
      [ condition(success(tracer_records_events)),
        explain(::error('После unwatch/1 новые события не должны записываться.')) ],
      ( catch(tracer::clear, _, true),
        greeter::hello(again),
        tracer::events(Events),
        Events == [] )).

   test(counter_monitor_api, true,
      [ explain(::error('counter_monitor должен реализовывать monitoring и иметь public count/1, reset/0.')) ],
      ( current_object(counter_monitor),
        implements_protocol(counter_monitor, monitoring),
        object_property(counter_monitor, public(Predicates)),
        lists:member(count/1, Predicates),
        lists:member(reset/0, Predicates) )).

   test(counter_monitor_counts, true,
      [ condition(success(counter_monitor_api)),
        condition(success(event_control_api)),
        explain(::error('counter_monitor должен считать сообщения, отправленные greeter.')) ],
      ( catch(event_control::unwatch(tracer), _, true),
        counter_monitor::reset,
        event_control::watch(counter_monitor),
        greeter::hello(one),
        greeter::hello(two),
        greeter::noop,
        counter_monitor::count(3),
        event_control::unwatch(counter_monitor),
        counter_monitor::reset )).

:- end_object.

:- object(tests,
   extends(studyunit)).

   test_name('Практическая работа 9 - События и перехват сообщений').
   test_type(problem_set).

   run :-
       test_set9::run.

:- end_object.
