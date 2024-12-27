

:- object(extest(_O_),
   extends(studyunit)).

   number(1).
   count(0).
   % note(note +_A_-_B_).

   test(object_exists(_O_), true,
       [explain(::error("We need to create object: ~w"+[_O_]))],
       (current_object(_O_))).

:- end_object.
