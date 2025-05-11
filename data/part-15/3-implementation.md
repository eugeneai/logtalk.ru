---
path: '/part-15/3-implementation'
title: 'Реализация генератора документов'
hidden: false
---


## Процедура генерации документа: общий сценарий

Чтобы сгенерировать документ, надо сначала открыть выходной поток, сгенерироавть преамбулу, общую для всех документов.  В конце, в обратном порядка, генерируется заключитальеная часть, и закрывается выходной поток.  Реализовать этот сценарий можно в виде объекта или категории.

```logtalk
:- object(documents(_Renderer_)).
   :- protected(start/0).
   start:-
        _Renderer_::openStream,
        _Renderer_::preamble.

   :- protected(end/0).
   end:-
        _Renderer_::postamble,
        _Renderer_::closeStream.
:- end_object.
```

### Процедура генерации документов для группы студентов

Генерируется два документа для каждого студента - справка полученных оценок и справка о том, что студент действительно учится в университете на конкретной специальности.  Все документы всех студентов группы помещаются в один файл.

```logtalk
:- object(potaninDocuments(_Renderer_, _Notes_, _StudentGroup_, _Curriculum_, _SignPerson_),
   extends(documents(_Renderer_)),
   imports(notesc)).

   :- public(gen/0).
   gen:-
        ::start,
        forall((_Notes_::notes(Person, Notes),
                _StudentGroup_::element(Person, student(PersonName, Gender, PersonAge))),
               (
                _Renderer_::newpage,
                notesDocument(_Renderer_, Notes,
                    student(Person, PersonName, Gender, PersonAge),
                    _Curriculum_, _SignPerson_
                )::gen,
                _Renderer_::newpage,
                referenceDocument(_Renderer_,
                    student(Person, PersonName, Gender, PersonAge),
                    _Curriculum_, _SignPerson_
                )::gen
               )
        ),
        ::end,
        format('% Files "~w" created.\n', [_Renderer_]).
:- end_object.
```

## Документы

### Справка о принадлежности студента институту

Документ генерируется объектом-отрисовщиком, передаваемым в параметр ```_Renderer_``` для конкретного студента ```_Student_```, учебного плана и подписывается уполномоченным лицом ```_SignPerson_```.  Данный объект только воспроизводит документ, не обращая внимание на кто, как он будет выгладеть.

```logtalk
:- object(referenceDocument(_Renderer_,_Student_, _Curriculum_, _SignPerson_),
   imports([notesc, russianc, optionc,
            signaturec(_Renderer_, _Curriculum_, _SignPerson_)]),
   implements(documentp)).

   :- use_module(library(lists), [member/2]).

   gen:-
        R = _Renderer_,
        self(Self),
        % вариант представления организации, левая колонка таблицы
        R::initAffiliation([right=Self::right]),
        R::begin(center),
        R::run('Справка дана для предъявления {\\bfseries по месту требования}'),
        R::end(center),
        ::signature(
            (
             R::par,
             R::vspace('2em')
            ),
            (
             R::par,
             R::emptyLine,
             R::cuttingLine,
             R::vfill,
             R::strut
            )
        ),
        true.

   :- public(right/0).
   % правыая колонка документа
   right:-
        ::subjectDefinition.

   subjectDefinition:-
        _Renderer_ = R,
        R::run('{'),
        R::cmd([bfseries,sffamily,centering]), R::run('СПРАВКА'), R::par,
        R::run('}'),
        R::benv,
        R::cmd('hspace{\\parindent}'),
        R::run('Дана '),
        _Student_ = student(Alias, PersonNames, Gender, Birth),
        ::option(dat(PersonName), PersonNames),
        R::run('\\textbf{~w}',[PersonName]),
        R::date(Birth),
        R::run(' г.р., '),
        R::run(' в том, что '),
        ::choice(Gender, [f='она', m='он'], Pronoun),
        R::run(Pronoun),
        R::run(' действительно является студентом '),
        C = _Curriculum_,
        C::currentStudyYear(StudyYear),
        R::run('{\\bfseries ~w} курса магистратуры ', [StudyYear]),
        R::benv,
        R::cmd(bfseries),
        (C::institute(InstituteName)->
          R::run(' Института '),
          R::run(InstituteName); true),
        (C::faculty(FacultyName)->
          R::run(' факультета '),
          R::run(FacultyName); true),
        R::run('ФГБОУ ВС <<ИГУ>>'),
        R::eenv,
        R::run(' очной формы обучения (за счет бюджетных ассигнований федерального бюджета) '),
        R::run(' по основной образовательной программе по направлению '),
        C::specialty(Code, Name),
        R::run(' {\\bfseries ~w~~~w } ', [Code, Name]),
        %R::run('(уровень магистратуры)'),
        R::run(' с '),
        C::startDate(StartDate),
        R::date(StartDate),
        C::enrollOrder(OrderNumber, EnrollDate),
        R::date(EnrollDate, EnrollDateString),
        R::run(' (Приказ на зачисление ~w от ~w)', [OrderNumber, EnrollDateString]),
        C::endDate(EndDate),
        R::date(EndDate, EndDateString),
        R::run(' Предполагаемый срок окончания обучения: ~w.  ', [EndDateString]),
        R::eenv,
        true.

:- end_object.
```
Основной метод - ```gen/0```.

### Справка с оценками

Структура объекта та же самая, за исключением названия объекта.

```logtalk
:- object(notesDocument(_Renderer_, _Notes_, _Student_, _Curriculum_, _SignPerson_),
   imports([notesc, russianc, optionc,
            signaturec(_Renderer_, _Curriculum_, _SignPerson_)]),
   implements(documentp)).

   :- use_module(library(lists), [member/2]).

   gen:-
        R = _Renderer_,
        R::initAffiliationFlat,
        R::begin(center),
        ::subjectDefinition,
        R::end(center),
        R::emptyLine,
        ::tableHeader,
        forall(member(Note, _Notes_), ::tableRow(Note)),
        ::tableFooter,
        ::statistics,
        ::signature,
        true.

   :- protected(statistics/0).
   statistics:-
        R = _Renderer_,
        ::statistics([5,4,ok], Total),
        ::statistics([5], Fives),
        ::statistics([4], Goods),
        R::cmd(noindent),
        R::run('Общее количество оценок -- ~w', [Total]), R::nl,
        R::run('Количество оценок "Отлично" -- ~w', [Fives]), R::nl,
        R::run('Количество оценок "Хорошо" -- ~w', [Goods]),
        true.

   :- protected(statistics/2).
   statistics([], 0).
   statistics([Var|T], Sum):-
        findall(1, member(_-Var,_Notes_), L),
        length(L, Sum1),
        statistics(T, Sum2),
        Sum is Sum1 + Sum2.

   subjectDefinition:-
        _Renderer_ = R,
        R::run('{'),
        R::cmd([large,bfseries,sffamily]), R::run('Справка об успеваемости за весь период обучения'),
        R::run('}'), R::nl('1em'),
        R::run('Выдана '),
        _Student_ = student(Alias, PersonNames, Gender, _),
        ::option(dat(PersonName), PersonNames),
        R::run('\\textbf{~w}',[PersonName]), % R::nl,
        R::run(' в том, что '),
        ::choice(Gender, [f='она', m='он'], Pronoun),
        R::run(Pronoun),
        R::run(' является студентом '),
        C = _Curriculum_,
        C::currentStudyYear(StudyYear),
        R::run(' {~w} ', [StudyYear]),
        R::run(' курса магистратуры очной формы обучения, обучается по направлению подготовки (специальности) высшего образования '),
        C::specialty(Code, Name),
        R::run(' {\\bfseries ~w~~~w  ', [Code, Name]),
        (C::institute(InstituteName)->
          R::run(' Института '),
          R::run(InstituteName); true),
        (C::faculty(FacultyName)->
          R::run(' факультета '),
          R::run(FacultyName); true),
        R::eenv,
        R::run('{\\bfseries ФГБОУ ВС <<ИГУ>>}'),
        true.

   :- protected(tableHeader/0).
   tableHeader:-
        _Renderer_ = R,
        R::cmd('setcounter{mytableline}{0}'),
        ::longtblrStyle(default),
        R::begin(longtblr,[
            '[caption={}]',
            '{',
            'width=1\\linewidth,rowhead=1,colspec={|X|X[15]|X[3]|}, row{1} = {c}, column{1} = {r}, column{3} = {c}, hlines}'
            ]),
        R::run(' \\textbf{№} '), R::tab,
        R::run(' \\textbf{Дисциплина} '), R::tab,
        R::run(' \\textbf{Оценка} '), R::nl,
        ::resetLineNumber.

   :- protected(tableFooter/0).
   tableFooter:-
        _Renderer_ = R,
        R::end(longtblr).

   :- protected(tableRow/1).
   tableRow(Subject-Note) :-
        ::translate(Note, NoteName),
        _Renderer_ = R,
        ::incNumber(LineNumber),
        R::run(LineNumber), R::tab,
        _Curriculum_::course(Subject, course(SubjectName), _, _, _, _, _),
        R::run(SubjectName), R::tab,
        R::run(NoteName), R::nl.

   :- protected(longtblrStyle/1).
   longtblrStyle(_):-
        R = _Renderer_,
        R::cmd('DefTblrTemplate{contfoot-text}{default}{}'),
        R::cmd('DefTblrTemplate{conthead-text}{default}{}'),
        R::cmd('DefTblrTemplate{caption-text}{default}{}'),
        R::cmd('DefTblrTemplate{caption-tag}{default}{}'),
        R::cmd('DefTblrTemplate{caption-sep}{default}{}'),
        true.

   :- protected(resetLineNumber/0).
   resetLineNumber:-
        retractall(lineNumber(_)),
        assertz(lineNumber(0)).

   :- private(lineNumber/1).
   :- dynamic(lineNumber/1).

   :- protected(incNumber/1).
   incNumber(Number):-
        lineNumber(Num),
        Number is Num + 1,
        retractall(lineNumber(_)),
        assertz(lineNumber(Number)).

:- end_object.
```

## Реализация объекта-отрисовщика

Правильным вариантом реализации было б использование компонентного подхода, где необходимо продумать заренее и определить протокол.  Здесь на этапе исследования предметной области реализация достигнута, действуя в обратном направлении, *ad hoc*-подход.  Создадим сразу объект.  Делать из этого объекта категорию не следует, пусть его большое количество методов располагаются в отдельном пространстве имен.  Параметр объекта - это выхдоной поток.

```logtalk
:- object(latexRenderer(_FileName_),
   imports(optionc)).
   :- use_module(library(lists), [member/2]).

   :- public(preamble/0).
   preamble:-
        ::run('\\documentclass[12pt]{scrreprt}'),
        ::cmd('pagestyle{empty}'),
        forall(::requirePackage(Package), ::run('\\usepackage{~w}', [Package])),
        forall(::requirePackage(Options, Package), ::run('\\usepackage~w{~w}', [Options, Package])),
        ::styleConfig,
        ::auxPreamble,
        ::begin(document).

   :- public(postamble/0).
   postamble:-
        ::end(document).

   :- public(newpage/0).
   newpage:-
        ::cmd(newpage).

   :- public(cmd/1).
   cmd([]).
   cmd([Cmd|T]):-
        cmd(Cmd),
        cmd(T).
   cmd(Cmd):-
        ::run('\\~w ', [Cmd]).

   :- public(run/1).
   run(String):-
        ::outputStream(O),
        format(O, '~w\n', [String]).

   :- public(run/2).
   run(FormatString, Args):-
        format(atom(S), FormatString, Args),
        run(S).

   :- public(runLn/1).
   runLn(String):-
        ::outputStream(O),
        format(O, '~w\\\\\n', [String]).

   :- public(runsLn/1).
   runsLn([]).
   runsLn([S|T]):-
        forall(member(L, [S|T]), runLn(L)).

   :- public(runs/1).
   runs([]).
   runs([S|T]):-
        forall(member(L, [S|T]), run(L)).

   :- public(runLn/2).
   runLn(FormatString, Args):-
        format(atom(S), FormatString, Args),
        runLn(S).

   :- public(begin/1).
   begin(Environment):-
        format(atom(S), 'begin{~w}', [Environment]),
        ::cmd(S).
   :- public(begin/2).
   begin(Environment,Args):-
        format(atom(S), 'begin{~w}', [Environment]),
        ::cmd(S),
        ::runs(Args).
   :- public(end/1).
   end(Environment):-
        format(atom(S), 'end{~w}', [Environment]),
        ::cmd(S).

   :- public(nl/0).
   nl:-
    runLn('').

   :- public(nl/1).
   nl(Size):-
        ::outputStream(O),
        format(O,'\\\\[~w]\n',[Size]).

   :- public(par/0).
   par:-
        ::cmd(par).

   :- public(vspace/1).
   vspace(Size):-
        ::outputStream(O),
        format(O,'\\vspace{~w}\n',[Size]).

   :- public(emptyLine/0).
   emptyLine:-
        vspace('1em').

   :- public(date/1).
   date(Date):-
        ::date(Date, String),
        ::run(String).
   :- public(date/2).
   date(YYYY-MM-DD, Output):-
        ::twodig(DD, D),
        ::twodig(MM, M),
        format(atom(Output), '~w.~w.~w',[D, M, YYYY]).

   :- protected(twodig/2).
   twodig(D, Output):-D>=10,!,
        format(atom(Output), '~w', [D]).
   twodig(D, Output):-
        format(atom(Output), '0~w', [D]).


   :- public(underscoreFill/1).
   underscoreFill(Size):-
        ::run('\\makebox[~w]{\\hrulefill}',[Size]).

   :- public(tab/0).
   tab:-
        run('&').

   :- public(hfill/0).
   hfill:-
        ::cmd(hfill).

   :- public(vfill/0).
   vfill:-
        ::cmd(vfill).

   :- public(strut/0).
   strut:-
        ::cmd(null).

   :- public(benv/0).
   benv:-
        ::run('{').
   :- public(eenv/0).
   eenv:-
        ::run('}').

   :- public(cuttingLine/0).
   cuttingLine:-
        ::cmd([noindent, dotfill]).

   :- public(initAffiliationFlat/0).
   initAffiliationFlat:-
        ::cmd(noindent),
        ::begin(center),
        ::isuLogo('width=17mm'),
        ::runsLn(['МИНОБРНАУКИ РОССИИ',
                   'Федеральное государственное бюджетное образовательное учреждение высшего образования',
                   '«Иркутский государственный университет»',
                   '(ФГБОУ ВО «ИГУ»)',
                   'Институт математики и информационных технологий',
                   'б.~Гагарина, д.~20, Иркутск, 664003,',
                   'Тел.: (3952) 24-22-14, Факс: (3952) 24-39-63',
                   'ОКПО 02068226, ОГРН 1033801008218, ИНН/КПП 3808013278/380801001',
                   '\\texttt{http://www.math.isu.ru}, e-mail: \\texttt{ime@math.isu.ru}']),
        ::vspace('0.7em'),
        ::letterRegistration('25ex','12ex'),
        ::end(center).

   :- public(initAffiliation/0).
   initAffiliation:-
        ::initAffiliation([]).

   :- public(initAffiliation/1).
   initAffiliation(Options):-
        ::cmd(noindent),
        ::begin(tblr, ['{width=\\linewidth, colspec={X[6]X[5]}, column{1}={c}}']),
            ::run('{'),
        ::cmd(footnotesize),
        ::isuLogo('width=13mm'),
        ::runsLn(['МИНОБРНАУКИ РОССИИ',
                   'Федеральное государственное бюджетное',
                   'образовательное учреждение',
                   'высшего образования',
                   '«Иркутский государственный университет»',
                   '(ФГБОУ ВО «ИГУ»)',
                   'Институт математики и информационных технологий',
                   'б.~Гагарина, д.~20, Иркутск, 664003,',
                   'Тел.: (3952) 24-22-14, Факс: (3952) 24-39-63',
                   'ОКПО 02068226, ОГРН 1033801008218,',
                   'ИНН/КПП 3808013278/380801001',
                   '\\texttt{http://www.math.isu.ru},',
                   'e-mail: \\texttt{ime@math.isu.ru}']),
        ::vspace('0.7em'),
        ::letterRegistration('25ex','12ex'),
            ::run('}'),
        ::tab,
            (::option(right(Right), Options) -> call(Right); true),
        ::end(tblr).

   :- public(isuLogo/1).
   isuLogo(Option):-
        runLn('\\includegraphics[~w]{~w}',[Option, 'isu-logo.png']).

   :- public(styleConfig/0).
   styleConfig:-
        ::cmd('defaultfontfeatures{Ligatures={TeX,Required},Scale=MatchLowercase}'),
        ::cmd('geometry{paper=a4paper,includehead, left=1.5cm, right=1.5cm, top=0.0cm, bottom=1.5cm}'),
        % ::cmd('geometry{paper=a4paper,showframe, includehead, left=1.5cm, right=1.5cm, top=0.0cm, bottom=1.5cm}'),
        ::run('\\let\\headwidth=\\textwidth'),
        ::cmd('setmainfont[Scale=1,ItalicFont=timesi.ttf,BoldFont=timesbd.ttf,BoldItalicFont=timesbi.ttf]{times.ttf}'),
        ::cmd('setmonofont[Numbers=SlashedZero,Scale=1,ItalicFont=couri.ttf,BoldFont=courbd.ttf,BoldItalicFont=courbi.ttf,]{cour.ttf}'),
        ::cmd('setsansfont[Scale=1,ItalicFont=Fira Sans Italic,BoldFont=Fira Sans Bold,BoldItalicFont=Fira Sans Bold Italic,]{Fira Sans Regular}'),
        ::cmd('newcounter{mytableline}'),
        true.

   :- protected(letterRegistration/2).
      %_______________ № ________________
      %На № __________ от _______________
   letterRegistration(AllSize, HruleSize):-
        ::underscoreFill('{18ex}'), ::run('№ '),  ::underscoreFill('{12ex}'), ::nl('0.5em'),
        ::run('На № '), ::underscoreFill(HruleSize), ::run(' от '), ::underscoreFill(HruleSize).

   :- public(openStream/0).
   :- use_module(user, [open/4, close/1]).
   openStream:-
        ( _FileName_=user -> OutputStream=user;
          open(_FileName_, write, OutputStream, [alias(outputStream)])),
        ::assertz(outputStream(OutputStream)).

   :- public(closeStream/0).
   closeStream:-
        ::outputStream(O),
        ( O\=user -> close(O); true),
        ::retractall(outputStream(_)).

   :- protected(outputStream/1).
   :- dynamic(outputStream/1).

   :- protected(auxPreamble/0).
   auxPreamble.

   :- protected(requirePackage/1).
   requirePackage(longtable).
   requirePackage(tabularx).
   requirePackage(graphicx).
   requirePackage(geometry).
   requirePackage(indentfirst).
   requirePackage(luatextra).
   requirePackage('unicode-math').
   requirePackage(color).
   requirePackage(tabularray).

   :- protected(requirePackage/2).
   requirePackage([final], hyperref).
   requirePackage([protrusion=false,expansion=false],microtype).
   requirePackage([russian,english], babel).

:- end_object.
```

Представленное приложение реализовано за три этапа, суммарно 0.7 человеко-дня, начиная с идеи, заканчивая многократным исправлением оценок в списке оценок.  Возможные направления совершенствования:

- разработать отрисовщик для DOCX-документов, HTML, Postscript, PCL-5,6 и другие популярные форматы
- представить данные об объектах предметной области при помощи графов знаний, сделать интерфейс к этим данным
- создать систему шаблонов или отдельные объекты, генерирующие части документов
- разработать новые документы
