:- object(test_set10,
   extends(studyunit)).

   test_name('Набор 10 - DCG и экспертные системы').
   test_type(problem_set).

   test(A, B, C, D) :- ^^test(A, B, C, D).

   test(greeting_parser, true,
      [ explain(::error('greeting//1 должно разбирать [hello, Name].')) ],
      ( greeting_parser::greeting(Name, [hello, world], []),
        Name == world,
        \+ greeting_parser::greeting(_, [hi, world], []) )).

   test(number_parser, true,
      [ explain(::error('number//1 должно разбирать одно число.')) ],
      ( arithmetic_parser::number(N, [7], []),
        N == 7 )).

   test(sum_parser, true,
      [ condition(success(number_parser)),
        explain(::error('sum//1 должно разбирать число + число и вычислять сумму.')) ],
      ( arithmetic_parser::sum(N, [2, '+', 3], []),
        N == 5 )).

   test(expert_system_api, true,
      [ explain(::error('animal_expert должен иметь public solve/2.')) ],
      ( current_object(animal_expert),
        object_property(animal_expert, public(Predicates)),
        lists:member(solve/2, Predicates) )).

   test(expert_system_classification, true,
      [ condition(success(expert_system_api)),
        explain(::error('solve/2 должен определять cat, dog, bird и mammal по фактам has/2.')) ],
      ( animal_expert::solve(fluffy, cat),
        animal_expert::solve(rex, dog),
        animal_expert::solve(tweety, bird) )).

:- end_object.

:- object(tests,
   extends(studyunit)).

   test_name('Практическая работа 10 - DCG и экспертные системы').
   test_type(problem_set).

   run :-
       test_set10::run.

:- end_object.
