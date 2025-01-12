%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% define a flag to allow the logtalk_tester script to pass the
% option to suppress the test file and directory path prefix
:- initialization(
	create_logtalk_flag(suppress_path_prefix, '', [type(atom), keep(true)])
).


:- category(study_messages).

	:- info([
		version is 0:1:0,
		author is 'Evgeny Cherkashin',
		date is 2024-12-22,
		comment is 'Logtalk unit test framework default message usage in student testcase testing.'
	]).

	:- set_logtalk_flag(debug, off).

	% structured message printing predicates;
	% the main reason to not write directly to an output stream is to allows
	% other tools such as IDEs to intercept and handle unit test results

	:- multifile(logtalk::message_prefix_stream/4).
	:- dynamic(logtalk::message_prefix_stream/4).

	logtalk::message_prefix_stream(Kind, lgtunit, Prefix, Stream) :-
		message_prefix_stream(Kind, Prefix, Stream).

	% Quintus Prolog based prefixes (also used in SICStus Prolog):
	message_prefix_stream(information, '% ',     user_output).
	message_prefix_stream(warning,     '*     ', user_output).
	message_prefix_stream(error,       '!     ', user_output).

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(Message, lgtunit) -->
		{numbervars(Message, 0, _)},
		message_tokens(Message).

	% messages for tests handling

	message_tokens(tests_started) -->
		[].

	message_tokens(tests_ended) -->
		[].

	message_tokens(tests_start_date_time(Year, Month, Day, Hours, Minutes, Seconds)) -->
		{	integer_to_padded_atom(Month, MonthAtom),
			integer_to_padded_atom(Day, DayAtom),
			integer_to_padded_atom(Hours, HoursAtom),
			integer_to_padded_atom(Minutes, MinutesAtom),
			integer_to_padded_atom(Seconds, SecondsAtom),
			Args = [Year, MonthAtom, DayAtom, HoursAtom, MinutesAtom, SecondsAtom]
		},
		[nl, 'тестирование начато в ~w-~w-~w, ~w:~w:~w'-Args, nl, nl].

	message_tokens(tests_end_date_time(Year, Month, Day, Hours, Minutes, Seconds)) -->
		{	integer_to_padded_atom(Month, MonthAtom),
			integer_to_padded_atom(Day, DayAtom),
			integer_to_padded_atom(Hours, HoursAtom),
			integer_to_padded_atom(Minutes, MinutesAtom),
			integer_to_padded_atom(Seconds, SecondsAtom),
			Args = [Year, MonthAtom, DayAtom, HoursAtom, MinutesAtom, SecondsAtom]
		},
		['тестирование закончено в ~w-~w-~w, ~w:~w:~w'-Args, nl, nl].

	message_tokens(running_tests_from_object_file(Object, File)) -->
		['выполнение тестов из объекта ~q:'-[Object], nl, 'файл: ~w'-[File], nl, nl].

	message_tokens(number_of_tests(Total)) -->
		['количество тестов: ~q'-[Total], nl].

	message_tokens(completed_tests_from_object(Object)) -->
		['заверненных тестов в объекте ~q:'-[Object], nl, nl].

	message_tokens(tests_skipped(Object, _File, Note)) -->
		(	{Note == ''} ->
			['tests skipped @ ~q'-[Object], nl]
		;	['tests skipped @ ~q (~w)'-[Object, Note], nl]
		).

	message_tokens(tests_run_differ_from_tests_total(Run, Total)) -->
		['количесто запущенных тестов: ~d, но количество определений тестов: ~d'-[Run, Total], nl, nl].

	% messages for test results

	message_tokens(tests_results_summary(_Object, Total, Skipped, Passed, Failed, Flaky, Note)) -->
		(	{Note == ''} ->
			[nl, '~d тестов, из них: ~d пропущено, ~d успешны, ~d неуспешны (~d халявных)'-[Total, Skipped, Passed, Failed, Flaky], nl]
		;	[nl, '~d тестов, из них: ~d пропущено, ~d успешны, ~d неуспешны (~d халавных; ~w)'-[Total, Skipped, Passed, Failed, Flaky, Note], nl]
		).

	:- if(\+ current_logtalk_flag(prolog_dialect, ji)).

		message_tokens(tests_runtime(_Object, CPUTime, WallTime)) -->
			['время исполенния: ~9f/~9f ЦП/всего секунд'-[CPUTime, WallTime], nl].

		message_tokens(passed_test(Object, Test, File, Position, Note, CPUTime, WallTime)) -->
            {Object::test_passed(Test, [file(File), position(Position),
                                        note(Note), cpu(CPUTime), wall(WallTime)]) ; true},
			(	{Note == ''} ->
				['~q: успешно (in ~9f/~9f  ЦП/всего секунд)'-[Test, CPUTime, WallTime], nl]
			;	['~q: успешно (~w) (за ~9f/~9f  ЦП/всего секунд)'-[Test, Note, CPUTime, WallTime], nl]
			).

		message_tokens(failed_test(Object, Test, File, Position, Reason, Flaky, Note, CPUTime, WallTime)) -->
            {Object::test_failed(Test, [file(File), position(Position), reason(Reason), flaky(Flaky),
                                        note(Note), cpu(CPUTime), wall(WallTime)]) ; true},
			(	{Note == ''} ->
				['~q: неуспех'-[Test]], flaky(Flaky), ['(за ~9f/~9f ЦП/всего секунд)'-[CPUTime, WallTime], nl]
			;	['~q: неуспех (~w)'-[Test, Note]], flaky(Flaky), ['(за ~9f/~9f ЦП/всего секунд)'-[CPUTime, WallTime], nl]
			),
			failed_test_reason(Reason),
			message_context(File, Position),
			[nl].

	:- else.

		message_tokens(tests_runtime(_Object, CPUTime, WallTime)) -->
			['время тсполнения: ~w/~w секунд'-[CPUTime, WallTime], nl].

		message_tokens(passed_test(Object, Test, _File, Position, Note, CPUTime, WallTime)) -->
            {Object::test_passed(Test, [file(File), position(Position),
                                        note(Note), cpu(CPUTime), wall(WallTime)]) ; true},
			(	{Note == ''} ->
				['~q: успех (in ~w/~w ЦП/всего секунд)'-[Test, CPUTime, WallTime], nl]
			;	['~q: успех (~w) (in ~w/~w ЦП/всего секунд)'-[Test, Note, CPUTime, WallTime], nl]
			).

		message_tokens(failed_test(Object, Test, File, Position, Reason, Flaky, Note, CPUTime, WallTime)) -->
            {Object::test_failed(Test, [file(File), position(Position), reason(Reason), flaky(Flaky),
                                        note(Note), cpu(CPUTime), wall(WallTime)]) ; true},
			(	{Note == ''} ->
				['~q: неуспех'-[Test]], flaky(Flaky), ['(за ~w/~w ЦП/всего секунд)'-[CPUTime, WallTime], nl]
			;	['~q: неуспех (~w)'-[Test, Note]], flaky(Flaky), ['(за ~w/~w ЦП/всего секунд)'-[CPUTime, WallTime], nl]
			),
			failed_test_reason(Reason),
			message_context(File, Position),
			[nl].

	:- endif.

	message_tokens(skipped_test(_Object, Test, _File, _Position, Note)) -->
		(	{Note == ''} ->
			['~q: пропущено'-[Test], nl]
		;	['~q: пропущено (~w)'-[Test, Note], nl]
		).

	message_tokens(verbose_quick_check_test(passed, Goal)) -->
		['Успешно:    ~q'-[Goal], nl].
	message_tokens(verbose_quick_check_test(discarded, Goal)) -->
		['Отменено:   ~q'-[Goal], nl].
	message_tokens(verbose_quick_check_test(failure, Goal)) -->
		['Неуспешно:  ~q'-[Goal], nl].
	message_tokens(verbose_quick_check_test(error, Goal)) -->
		['Ошибочных:  ~q'-[Goal], nl].
	message_tokens(verbose_quick_check_test(shrinked, Goal)) -->
		['Согращений: ~q'-[Goal], nl].

	message_tokens(quick_check_progress_bar_start) -->
		[].
	message_tokens(quick_check_progress_bar_tick(Test)) -->
		[at_same_line, '...~d'-[Test], flush].
	message_tokens(quick_check_progress_bar_end) -->
		[at_same_line, nl].

	message_tokens(quick_check_passed(NumberOfTests, Seed, Discarded, Labels)) -->
		['~w случайных тестов, выполненных удачно, ~w discarded'-[NumberOfTests, Discarded], nl],
		['Начальное случайное зерно: ~w'-[Seed], nl],
		quick_check_labels(Labels, NumberOfTests).

	message_tokens(quick_check_failed(Goal, Test, Shrinks, Seed, TestSeed)) -->
		(	{Shrinks == 1} ->
			['quick check - тест неудачен (тест \'~w\' после ~w сокращений):'-[Test, Shrinks], nl, '  ~q'-[Goal], nl]
		;	['quick check - тест неудачен  (тест \'~w\' после ~w сокращений):'-[Test, Shrinks], nl, '  ~q'-[Goal], nl]
		),
		['начальное случайное зерно: ~q'-[Seed], nl],
		['зерно тестирования:        ~q'-[TestSeed], nl].

	message_tokens(quick_check_error(error(Error,_), Goal, Test, Seed, TestSeed)) -->
		message_tokens(quick_check_error(Error, Goal, Test, Seed, TestSeed)).
	message_tokens(quick_check_error(Error, _Goal, Test, Seed, TestSeed)) -->
		['quick check - тест исполнен с ошибкой (тест \'~w\'):'-[Test], nl, '  ~q'-[Error], nl],
		['начальное случайное зерно: ~q'-[Seed], nl],
		['зерно тестирования:        ~q'-[TestSeed], nl].

	message_tokens(quick_check_broken(generate_test_error(Template), Error)) -->
		['порождение quick check - теста выполнено с ошибкой для шаблона: ~q'-[Template], nl, '  ~q'-[Error], nl].
	message_tokens(quick_check_broken(generate_test_failure(Template), Type)) -->
		['порождение quick check - теста выполнено неудачно для шаблона: ~q'-[Template], nl, '  ~q'-[Type], nl].

	message_tokens(quick_check_broken(label_goal_error(Label), Error)) -->
		['ошибка использования замыкания в quick check - тесте: ~q'-[Label], nl, '  ~q'-[Error], nl].
	message_tokens(quick_check_broken(label_goal_failure(Label))) -->
		['неудача в label-замыкании в quick check - тесте: ~q'-[Label], nl].

	message_tokens(quick_check_broken(pre_condition_error(Condition), Error)) -->
		['ошибка использования замыкания типа \'предусловие\' в quick check - тесте: ~q'-[Condition], nl, '  ~q'-[Error], nl].
	message_tokens(quick_check_broken(pre_condition_always_fails(Condition))) -->
		['неудача в замыкании типа \'предусловие\' в quick check - тесте: ~q'-[Condition], nl].

	message_tokens(quick_check_broken(template_error(Template), Error)) -->
		['Ошибка использования шаблона в quick check - тесте: ~q'-[Template], nl, '  ~q'-[Error], nl].

	message_tokens(quick_check_broken(option_error(Option), Error)) -->
		['параметр quick check - теста ошибочен: ~q'-[Option], nl, '  ~q'-[Error], nl].

	message_tokens(failed_cleanup(_Object, Test, File, Position, Reason)) -->
		failed_cleanup_reason(Reason, _Object, Test),
		message_context(File, Position).

	message_tokens(broken_step(Step, Object, Error)) -->
		['нефункционирующая цель \'~w\' для объекта-теста \'~q\': ~q'-[Step, Object, Error], nl].

	message_tokens(failed_step(Step, Object)) -->
		['запуск цели \'~w\' объекта-теста \'~q\' привел к неудаче'-[Step, Object], nl].

	% messages for test's clause coverage

	message_tokens(declared_entities_and_clause_numbers(Entities, Clauses)) -->
		entity_tokens(Entities),
		[' декларирован покрытием, содержащим '-[]],
		clause_tokens(Clauses),
		[nl].

	message_tokens(covered_entities_numbers(Covered, Total, Percentage)) -->
		['~d из всего '-[Covered]],
		entity_tokens(Total),
		[' покрыто, ~f% порытием сущностей'-[Percentage], nl].

	message_tokens(covered_clause_numbers(Covered, Total, Percentage)) -->
		['~d out of '-[Covered]],
		clause_tokens(Total),
		[' covered, ~f% покрытием высказываний'-[Percentage], nl, nl].

	message_tokens(code_coverage_header) -->
		[nl, 'отношение покрытий высказываний и покрытие предикатов для каждой сущности'-[], nl, nl].

	message_tokens(entity_coverage_starts(_Entity)) -->
		[].

	message_tokens(entity_predicate_coverage(Entity, Predicate, Covered, Total, _Percentage, Clauses)) -->
		(	{Covered =:= Total} ->
			% all clause are covered
			['~q: ~q - ~w - ~w'-[Entity, Predicate, Covered/Total, '(all)'], nl]
		;	['~q: ~q - ~w - ~w'-[Entity, Predicate, Covered/Total, Clauses], nl]
		).

	message_tokens(entity_coverage(Entity, Covered, Total, Percentage)) -->
		['~q: ~d out of '-[Entity, Covered]],
		clause_tokens(Total),
		[' покрыто, ~f% покрытий'-[Percentage], nl, nl].

	message_tokens(entity_coverage_ends(_Entity)) -->
		[].

	message_tokens(no_code_coverage_information_collected) -->
		['не удалось собрать информацию о покрытии кода'-[], nl].

	message_tokens(no_code_coverage_for_protocols(Entity)) -->
		['покрытие кода запрошено для протокола: ~q'-[Entity], nl].

	message_tokens(unknown_entity_declared_covered(Entity)) -->
		['количество неизвестных сущностей, для которых декларировано покрытие: ~q'-[Entity], nl].

	% messages for test identifier errors (compile-time)

	message_tokens(non_instantiated_test_identifier(_Object, File, Position, Type, Entity)) -->
		['найден идентификатор теста, содержащий свободную переменную'-[], nl],
		message_context(File, Position, Type, Entity).

	message_tokens(non_callable_test_identifier(_Object, Test, File, Position, Type, Entity)) -->
		['найден non-callable - идентификатор теста: ~q'-[Test], nl],
		message_context(File, Position, Type, Entity).

	message_tokens(non_ground_test_identifier(_Object, Test, File, Position, Type, Entity)) -->
		['найден неосновной идентификатор теста: ~q'-[Test], nl],
		message_context(File, Position, Type, Entity).

	message_tokens(repeated_test_identifier(_Object, Test, File, Position, Type, Entity)) -->
		['найден повторный идентификатор теста: ~q'-[Test], nl],
		message_context(File, Position, Type, Entity).

	% messages for test identifier errors (runtime)

	message_tokens(non_instantiated_test_identifier) -->
		['идентификатор теста, содержащий свободную переменную'-[], nl].

	message_tokens(non_callable_test_identifier(Test)) -->
		['non-callable - идентификатор теста: ~q'-[Test], nl].

	message_tokens(non_ground_test_identifier(Test)) -->
		['неосновной идентификатор теста: ~q: ~q'-[Test], nl].

	message_tokens(unknown_test(Test)) -->
		['неизвестный тест: ~q'-[Test], nl].

	message_tokens(partial_list_of_tests(Tests)) -->
		['частичный список тестов: ~q'-[Tests], nl].

	% messages for test outcome errors

	message_tokens(non_instantiated_test_outcome(Test, File, Position, Type, Entity)) -->
		['найден результат теста, содержащий свободную переменную: ~q'-[Test], nl],
		message_context(File, Position, Type, Entity).

	message_tokens(invalid_test_outcome(Test, Outcome, File, Position, Type, Entity)) -->
		['результат теста \'~q\' ошибочен: ~q'-[Test, Outcome], nl],
		message_context(File, Position, Type, Entity).

	% messages for invalid test specifications

	message_tokens(non_instantiated_test_option(Test, File, Position, Type, Entity)) -->
		['найден параметр теста, содержащий свободную переменную: ~q'-[Test], nl],
		message_context(File, Position, Type, Entity).

	message_tokens(invalid_test_option(Test, Option, File, Position, Type, Entity)) -->
		['параметр теста \'~q\' неправильный: ~q'-[Test, Option], nl],
		message_context(File, Position, Type, Entity).

	% linter warnings

	message_tokens(assertion_uses_unification(Test, Assertion, File, Position, Type, Entity)) -->
		['assert-выражение теста \'~q\' использует цель с унификацией: ~q'-[Test, Assertion], nl],
		message_context(File, Position, Type, Entity).

	% auxiliary grammar rules

	failed_test_reason(success_instead_of_failure) -->
		['  тест успешен, но должен быть неуспешным'-[], nl].
	failed_test_reason(success_instead_of_error(ExpectedError)) -->
		['  тест успешен, но должен был сгенерировать ошибку:'-[], nl],
		['    должно быть \'~q\''-[ExpectedError], nl].
	failed_test_reason(failure_instead_of_success) -->
		['  тест неуспешен, но должен быть успешным'-[], nl].
	failed_test_reason(failure_instead_of_error(ExpectedError)) -->
		['  тест неуспешен, но должен был сгенерировать ошибку:'-[], nl],
		['    expected ~q'-[ExpectedError], nl].
	failed_test_reason(non_deterministic_success) -->
		['  тест успешен, но выдает много результатов'-[], nl].
	failed_test_reason(error_instead_of_failure(Error)) -->
		['  тест генерирует ошибку, но должен быть неуспешным: ~q'-[Error], nl].
	failed_test_reason(error_instead_of_success(assertion_error(Assertion, error(Error,_)))) -->
		['  assert-выражение теста генерирует ошибку: ~q - ~q'-[Assertion, Error], nl].
	failed_test_reason(error_instead_of_success(assertion_error(Assertion, Error))) -->
		['  assert-выражение теста генерирует ошибку: ~q - ~q'-[Assertion, Error], nl].
	failed_test_reason(error_instead_of_success(assertion_failure(Assertion))) -->
		['  assert-выражение теста неуспешно: ~q'-[Assertion], nl].
	failed_test_reason(error_instead_of_success(Error)) -->
		['  тест генерирует ошибку, но дложен быть успешным: ~q'-[Error], nl].
	failed_test_reason(wrong_error(ExpectedError, Error)) -->
		['  тест генерирует ошибку, отличную от той, что должна быть сгенерирована:'-[], nl],
		['    должно быть \'~q\''-[ExpectedError], nl],
		['    сгенерировано \'~q\''-[Error], nl].

	failed_test_reason(quick_check_failed(Goal, Test, Shrinks, Seed, TestSeed)) -->
		(	{Shrinks == 1} ->
			['  quick check - тест неудачен (тест \'~w\' после выполнения ~w сокращений, при начальном зерне ~q и тестовом зерне ~q): ~q'-[Test, Shrinks, Seed, TestSeed, Goal], nl]
		;	['  quick check - тест неудачен (тест \'~w\' после выполнения ~w сокращений, при начальном зерне ~q и тестовом зерне ~q): ~q'-[Test, Shrinks, Seed, TestSeed, Goal], nl]
		).

	failed_test_reason(quick_check_error(error(Error,_), Goal, Test, Seed, TestSeed)) -->
		failed_test_reason(quick_check_error(Error, Goal, Test, Seed, TestSeed)).
	failed_test_reason(quick_check_error(Error, _Goal, Test, Seed, TestSeed)) -->
		['  quick check - тест выполнился с ошибкой (тест \'~w\' после выполнения ~w сокращений, при начальном зерне ~q и тестовом зерне ~q): ~q'-[Test, Seed, TestSeed, Error], nl].
	failed_test_reason(quick_check_error(Error, Culprit)) -->
		['  quick check - тест выполнился с ошибкой (источник проблем - ~q): ~q'-[Error, Culprit], nl].

	failed_test_reason(step_error(Step, Error)) -->
		['  ~w генерирует ошибку, но должен был выполниться удачно: ~q'-[Step, Error], nl].
	failed_test_reason(step_failure(Step)) -->
		['  ~w неудачный, но должен быть удачным'-[Step], nl].

	failed_test_reason(Unexpected) -->
		['  Непредвиденный отказ (вероятно проблема в backend-е): ~q'-[Unexpected], nl].

	failed_cleanup_reason(error(Error), _Object, Test) -->
		['  cleanup-стадия теста ~q генерирует ошибку, но должна была выполниться удачно: ~q'-[Test, Error], nl].
	failed_cleanup_reason(failure, _Object, Test) -->
		['  cleanup-стадия теста ~q выполнилась неудачно, но должна была выполниться удачно'-[Test], nl].

	flaky(true) -->
		[' [халявный] '-[]].
	flaky(false) -->
		[' '-[]].

	message_context(Path, Lines, Type, Entity) -->
		{suppress_path_prefix(Path, ShortPath)},
		['  при компилировании ~w ~q'-[Type, Entity], nl],
		(	{Lines == 0-0} ->
			['  во вспомогательном определении, сгенерированном в файл ~w'-[ShortPath], nl, nl]
		;	{Lines == 1-1} ->
			['  в файле ~w, строка 1'-[ShortPath], nl, nl]
		;	{Lines = Line-Line} ->
			['  в файле ~w, строка ~d или раньше'-[ShortPath, Line], nl, nl]
		;	['  в файле ~w между строками ~w'-[ShortPath, Lines], nl, nl]
		).

	message_context(Path, Lines) -->
		{suppress_path_prefix(Path, ShortPath)},
		['  while compiling file'-[], nl],
		(	{Lines == 0-0} ->
			['  во вспомогательном определении, сгенерированном в файл ~w'-[ShortPath], nl, nl]
		;	{Lines == 1-1} ->
			['  в файле ~w, строка 1'-[ShortPath], nl, nl]
		;	{Lines = Line-Line} ->
			['  в файле ~w, строка ~d или раньше'-[ShortPath, Line], nl, nl]
		;	['  in file ~w между строками ~w'-[ShortPath, Lines], nl, nl]
		).

	suppress_path_prefix(Path, ShortPath) :-
		{current_logtalk_flag(suppress_path_prefix, Prefix)},
		(	atom_concat(Prefix, ShortPath, Path) ->
			true
		;	ShortPath = Path
		).

	entity_tokens(Entities) -->
		(	{Entities =:= 1} ->
			['~d сущность'-[Entities]]
		;	['~d сущностей'-[Entities]]
		).

	clause_tokens(Clauses) -->
		(	{Clauses =:= 1} ->
			['~d фраза'-[Clauses]]
		;	['~d фраз'-[Clauses]]
		).

	quick_check_labels([], _) -->
		[].
	quick_check_labels([Label-N| Labels], NumberOfTests) -->
		(	{N > 0} ->
			{Percentage is N / NumberOfTests * 100}
		;	{Percentage is 0.0}
		),
		['~w: ~d/~d (~f%)'-[Label, N, NumberOfTests, Percentage], nl],
		quick_check_labels(Labels, NumberOfTests).

	integer_to_padded_atom(Integer, Atom) :-
		number_codes(Integer, Codes),
		(	Integer < 10 ->
			char_code('0', ZeroCode),
			atom_codes(Atom, [ZeroCode| Codes])
		;	atom_codes(Atom, Codes)
		).

:- end_category.