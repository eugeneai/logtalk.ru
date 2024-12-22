

:- object(extest,
   extends(studyunit)).

   number(1).
   count(0).
   % note(note +_A_-_B_).

   succeeds(object_exists(a)) :-
       current_object(a).
   fails(object_exists(b)) :-
       current_object(b).

   :- public(r/0).
   r :- ::run_tests.

:- end_object.
