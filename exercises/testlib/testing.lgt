:- protocol(testing_p).
   :- protected(test/4).
   :- mode(test(+atom, +atom, +list(+callable), +callable), zero_or_more).
   :- info(test/4, [
       comment is 'Test definition.',
       argnames is ['Identifier', 'Outcome', 'Options', 'TestGoal']
   ]).

   :- public(run_tests/1).
   :- mode(run_tests(+list(atom)), one).
   :- info(run_tests/1, [
      comment is 'Runs specific tests by name',
      argnames is ['TestNames']
   :- meta_predicate(test(*,*,*,0)).

   :- public(run/0).
   :- mode(run, one).
   :- info(run/0, [
      comment is 'Runs the unit tests, succeeds always and set total success flag.'
   ]).

   :- public(test_count/3).
   :- mode(test_count(-integer, -integer, -integer), one).
   :- info(test_count/1, [
      comment is 'Returns counts of success, failure, skipped tests',
      argnames is ['Success', 'Failure', 'Skipped']
   ).

   :- public(test_name/1).
   :- mode(test_name(-atom), one).
   :- info(test_name/1, [
      comment is 'Defines name for object relating it to a problem.',
       argnames is ['Name']
   ]).

:- end_protocol.

:- object(basicunit,
   imports(options),
   implements(testing_p)).
   :- info([
        version is 24:12:22,
        autor is 'Evgeny Cherkashin',
        date is 2024-12-27,
        comment is 'A student test case unit test framework inspired by Paulo Mora lgtunit'
      ]).


   run :-
       ::clear_results,
       ::test_name(TestName),
       ::debug(5,'Начало выполнения набора тестов \'~w\'' + [TestName]),
       forall(::test(Name, Outcome, Options, Goal),
           process_env(Name, Outcome, Options, Goal)
       ),
       ::debug(5,'Завершение набора тестов \'~w\'' + [TestName]).

   :- public(run_tests/1).
   run_tests(TestNames) :-
       ::clear_results,
       ::test_name(TestName),
       ::debug(5,'Начало выполнения выбранных тестов \'~w\'' + [TestName]),
       forall(
           (::test(Name, Outcome, Options, Goal), member(Name, TestNames)),
           process_env(Name, Outcome, Options, Goal)
       ),
       ::debug(5,'Завершение выбранных тестов \'~w\'' + [TestName]).

   :- public(test_count/3).
   test_count(Success, Failure, Skipped) :-
       self(Self),
       findall(_, ::test_result(Self, _, success), SuccessList),
       findall(_, ::test_result(Self, _, failure), FailureList),
       findall(_, ::test_result(Self, _, skipped), SkippedList),
       length(SuccessList, Success),
       length(FailureList, Failure),
       length(SkippedList, Skipped).

   :- protected(validate_test_options/2).
   validate_test_options(Name, Options) :-
       ^^option(each_test_name(_), Options), !,
       (   ^^option(each_explain(_), Options)
       ;   ^^option(each_condition(_), Options)
       ->  true
       ;   ::error("Тест '~w' с each_test_name требует each_explain или each_condition" + [Name])
       ).

   validate_test_options(_, _).

    :- protected(process_env/4).
   process_env(Name-debug, Outcome, Options, Goal) :-
      debugger::trace,
      process_env(Name, Outcome, Options, Goal).

   % Модифицировать process_env для валидации
   process_env(Name, Outcome, Options, Goal) :-
      ::validate_test_options(Name, Options),
      self(Self),
      ( ::debug(10,'Начало теста \'~w\'' + [Name]),
           (^^option(trace(start), Options) -> ::info('debug\n'), debugger::trace ; true),
        options_before(Options, Name)
        ->
              ( ^^option(trace(process), Options) -> debugger::trace ; true ),
              process(Name, Outcome, Options, Goal, Result), !,
              (^^option(trace(result), Options) -> debugger::trace ;true ),
           (^^option(parent_test(ParentName), Options),
              Result \= success,
              (::test_result(Self, ParentName, PrevResult) -> true ; PrevResult = success)
            ->
              ::worst_result(Result, PrevResult, CurResult), !,
              retractall(test_result(Self, ParentName, _)),
              assertz(test_result(Self, ParentName, CurResult))
            ;
              true),
           options_after(Options, Result), !,
           ::debug(10,'Результат теста \'~w\': ~w' + [Name, Result])
        ;
           ::info('Тест \'~w\' пропущен' + [Name]),
           assertz(test_result(Self, Name, skipped))
      ),
      options_final(Options, Result).

   :- protected(worst_result/3).
   worst_result(failure, _, failure).
   worst_result(_, failure, failure).
   worst_result(skipped, _, skipped).
   worst_result(_, skipped, skipped).
   worst_result(_, _, success).
   
:- protected(print_statistics/0).
   print_statistics :-
       ::test_count(Success, Failure, Skipped),
       Total is Success + Failure + Skipped,
       ::info("Статистика: ~w успешно, ~w провалено, ~w пропущено, всего ~w" +
              [Success, Failure, Skipped, Total]).


   :- protected(process/5).
   :- meta_predicate(process(*, *, *, 0, *)).
   process(Name, true, _Options, Goal, Result) :-
      self(Self),
      (call(Goal) -> Result = success; Result = failure ),
      assertz(test_result(Self, Name, Result)).

   process(Name, fail, _Options, Goal, Result) :-
      debugger::debug,
      self(Self),
      ( call(Goal) -> Result = failure; Result = success ),
      assertz(test_result(Self, Name, Result)).

   process(Name, all(AllGoal), Options, Goal, Result) :-
      self(Self),
      ( call(AllGoal),
        ::each_to_one(Options, Opts),
        (
           ^^option(each_test_name(EachName), Opts) -> true ;
           ::error("Забыта опция each_test_name(...) в all-тесте '~w' в объекте '~w'" + [Name, Self]),
           halt
        ),
        ::process_env(EachName, true, [parent_test(Name)| Opts], Goal), fail; true ),
      ( ::test_result(Self, Name, failure) -> Result = failure;
        Result = success ),
        assertz(test_result(Self, Name, Result)).

   process(Name, none(AllGoal), Options, Goal, Result) :-
      self(Self),
      ( call(AllGoal),
        ::each_to_one(Options, Opts),
        (
           ^^option(each_test_name(EachName), Opts) -> true ;
           ::error("Забыта опция each_test_name(...) в all-тесте '~w' в объекте '~w'" + [Name, Self]),
           halt
        ),
        ::process_env(EachName, fail, [parent_test(Name)| Opts], Goal), fail; true ),
      ( ::test_result(Self, Name, failure) -> Result = failure;
        Result = success ),
        assertz(test_result(Self, Name, Result)).

   process(Name, Type, Options, Goal, _) :- !,
      ::error("Не знакомый тип '~w' теста '~w' с опциями ~w и целью '~w'" +
        [Type, Name, Options, Goal]), halt.

   :- protected(each_to_one/2).
   :- use_module(library(lists), [select/3]).

   each_to_one([], []).
   each_to_one([X|T], T1) :-
      ::each_to_one_subst(X, none), !,
      each_to_one(T, T1),!.
   each_to_one([X|T], [R|T1]) :-
      ::each_to_one_subst(X, R), !,
      each_to_one(T, T1),!.
   each_to_one([X|T], [X|T1]) :- !,
      each_to_one(T, T1),!.

   :- protected(each_to_one_subst/2).
   each_to_one_subst(condition(_), none).
   each_to_one_subst(explain(_), none).
   each_to_one_subst(each_condition(Goal), condition(Goal)).
   each_to_one_subst(each_explain(Goal), explain(Goal)).

   :- protected(options_before/2).
   :- protected(options_after/2).
   :- protected(options_final/3).

   options_before(Options, _):-
      ^^option(skip, Options),!,fail.

   options_before(Options, _):-
      ^^option(skipped, Options),!,fail.

   options_before(Options, _):-
      ^^option(condition(success(Name)), Options),
      self(Self),
      ::test_result(Self, Name, success), !.
   options_before(Options, _):-
      ^^option(condition(failure(Name)), Options),
      self(Self),
      ::test_result(Self, Name, failure), !.
   options_before(Options, _):-
      ^^option(condition(skipped(Name)), Options),
      self(Self),
      ::test_result(Self, Name, skipped), !.

   options_before(Options, _):-
      ^^option(condition(Atom), Options),
      Atom =.. [Term, Name],
      AllTestResults = [success, failure, skipped],
      member(Term, AllTestResults), !,
      ( \+ ::test_result(Self, Name, _) ->
        self(Self),
        ::error('Результат теста \'~w\' в объекте \'~w\' еще не известен!\nВот что известно:\n'+[Name, Self]),
        ::print_test_results,
        halt; false ).

   options_before(Options, _):-
      ^^option(condition(Goal), Options),
      call(Goal), !.

   options_before(_, _).

   options_after(Options, failure) :-
      ^^option(explain(Goal), Options), !,
      ( call(Goal); true ).

   options_after(_, _).
   
% Расширить options_final для вывода статистики
   options_final(Options, _):-
       ^^option(statistics, Options), !,
       ::print_statistics.

   options_final(Options, _):-
      ^^option(note(Atom), Options),
      ::info("Note ~w"+[Atom]),!.

   options_final(_, _).

   :- public(print_test_results/0).
   print_test_results :-
      self(Self), !,
      forall(::test_result(Self, Name, Result),
         ::info('test_result(~p,~p,~p).\n' - [Self, Name, Result])),
      !.

   :- protected(error/1).
   error(Template+List):-!,
      ::fmt(error, Template, List, String),
      format(String, []).
   error(Template-List):-!,
      ::fmtl(error, Template, List, String),
      format(String, []).
   error(Message):-!,
      ::fmt(error, Message, String),
      format(String, []).

   :- protected(info/1).
   info(Template+List):-!,
      ::fmt(info, Template, List, String),
      format(String, []).
   info(Template-List):-!,
      ::fmtl(info, Template, List, String),
      format(String, []).
   info(Message):-!,
      ::fmt(info, Message, String),
      format(String, []).

   :- protected(debug/2).
   debug(Level,Template+List):-
      ::debug_level(DLevel),
      Level =< DLevel, !,
      ::fmt(debug, Template, List, String),
      format(String, []).
   debug(Level,Template-List):-
      ::debug_level(DLevel),
      Level =< DLevel, !,
      ::fmtl(debug, Template, List, String),
      format(String, []).
   debug(_Level,_Template+_List).
   debug(_Level,_Template-_List).

   :- protected(fmt/3).
   fmt(Level, Message, String) :-
       fmt_decor(Level, _Symbol, _Name, Color), !,
       fmt_color(Color, ColorCode),
       fmt_color(black, BlackCode),
       format(atom(String), '~w~w~w', [ColorCode, Message, BlackCode]).

   fmt(Template, List, String) :-
      format(atom(String), Template, List).

   :- protected(fmt/4).
   fmt(Level, Template, List, String) :-
      ::fmt(Template, List, S), !,
      ::test_name(Name), !,
      % debugger::trace,
      ::fmt_decor(Level, Sign, LevelName, Color), !,
      fmt_color(Color, ColorCode), !,
      fmt_color(black, BlackCode), !,
      format(atom(String), "~w~w~w:\n~w~w\n%>>>>> тест:~w\n~w",
          [ColorCode, Sign, LevelName, Sign, S, Name, BlackCode]),!.

   :- protected(fmtl/4).
   fmtl(Level, Template, List, String) :-
      ::fmt(Template, List, S), !,
      ::fmt_decor(Level, _Sign, _LevelName, Color), !,
      fmt_color(Color, ColorCode), !,
      fmt_color(black, BlackCode), !,
      format(atom(String), "~w~w~w",
          [ColorCode, S, BlackCode]),!.

   :- protected(fmt_decor/4).
   fmt_decor(error, '!', 'ERROR', red).
   fmt_decor(info, '*', 'INFO', blue).
   fmt_decor(debug, '+', 'DEBUG', black).

   :- protected(fmt_color/2).
   fmt_color(black, '\033[0;30m').
   fmt_color(green, '\033[0;32m').
   fmt_color(red, '\033[1;31m').
   fmt_color(blue, '\033[0;34m').
   fmt_color(A, A).

   :- protected(test_result/3).
   :- dynamic(test_result/3).
   :- protected(clear_results/0).
   clear_results :-
      self(Self),
      retractall(test_result(Self, _, _)).

   :- public(has_failed/0).
   has_failed :-
      self(Self),
      ::test_result(Self, _, failure),!.

   :- public(has_skipped/0).
   has_skipped :-
      self(Self),
      ::test_result(Self, _, skipped),!.

   :- public(all_succeeded/0).
   all_succeeded :-
      \+ has_skipped,
      \+ has_failed.

   :- public(partially_succeeded/0).
   partially_succeeded :-
      \+ has_failed.

   test_name(Self) :-
      self(Self).

   :- protected(mem/2).
   :- use_module(library(lists), [member/2]).

   mem(_,[]):-!, fail.
   mem(C,[X|T]) :- !,
      member(A,[X|T]),
      mem(C,A).
   mem(A,A).

   :- protected(checkall/2).
   :- meta_predicate(checkall(0,0)).
   checkall(Test, Goal):-
      call(Test),
      (call(Goal) -> fail;
       true),
       !, fail.
   checkall(_,_).

   :- protected(output_substring/2).
   output_substring(Out, SubString) :-
      check_string(Out),
      atomics_to_string([SubString], SS), !,
      sub_string(Out, _, _, _, SS).

   check_string(S) :-
      string(S),!.
   check_string(_) :-
      ::error("Argument is not a string" + []),
      halt.

   :- protected(debug_level/1).
   debug_level(0).

:- end_object.
