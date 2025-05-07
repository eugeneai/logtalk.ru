---
path: '/part-2/3-pattern-directed-programming'
title: 'Прогаммирование при помощи типовых конфигураций'
hidden: false
---

<text-box variant='learningObjectives' name="Цель изучения раздела">

В результате изучения раздела вы

- Осоите методику проектирования программных систем на основе *образцев* (pattern-directed programming)
- Освоите задание новых операций ```op/2``` типа стандартных "```:-```", "```->```", но предназначенных для вашей программы
- Реализуем небольшую экспертную систему диагностики двигателя автомобиля

</text-box>

## Программирование при помощи типовых конфигураций

_Программирование_ при помощи _типовых_ (ударение на "и") _конфигураций_ (_образцов_, _patterns_) - популярная в 70-80-е годы XX века технология проектирования прграммных систем, в частности, имитационных моделей (computer simulation), где программа строиться из большого набора простых модулей (образцов), состоящих из *шаблона* (секции "**Если**") и _тела_ (секция "**То**").  "**Если**" выполняется некоторое условие, например, в работчей памяти есть структуры, подходящие под шаблон, "**То**" выполнить тело образца.  *Рабочая память* (*working memory*), называемая также *базой данных* (*data base*) и *классной доской* (*black board*), - область оперативной памяти, где хранится информация о текущем состоянии *среды* (*environment*).  В нашем случае рабочая память заполняется динамическими методами - фактами.

При исполнении тела, в общем случае, используются значения переменных, полученных при сопоставлении шаблона, то есть тело конкретизируется этими значениям.  В результате исполнения тела набор фактов в рабочей памяти меняется.  Процесс применения (активации) образцов идет до тех пор, пока хотябы образец активирован: шаблон образца получает значения переменных и условие является истинным.  Также остановить процесс можно специальной командой.

По английски "прошраммирование при помощи типовых конфигураций" называется "_pattern-directed programming_".  Именно этот термин следует использовать для поиска дополнительной информации в интернете.

## Реализация машины, активирующей образцы

Реализовывать машину, запускающую образцы начнем с **ядра** (объекта ```pattern_engine/0```).  Ядро состоит из одного метода ```run/0```, непосредственно запускающего машину.  Правила записываются в форме ```[<элементы шаблона>]``` ```>>>``` ```[<команды тела>]```.  Квадратные скобки в шаблоне - элемент синтексиса.  В них через запятую перечисляются элементы шаблона и команды тела образца.  Знак ```>>>``` разделяет шаблон и тело, которые еще называют "левой стороной" (left hand side, LHS) и "правой стороной" (right hand side, RHS).

```
% File: patterns.lgt

:- object(pattern_engine).

   :- op(1199, xfx, '>>>').
   :- protected('>>>'/2).
   :- dynamic('>>>'/2).

   :- public(run/0).

   run :-
      :: >>>(Pattern, Body),
      ::check(Pattern), !,
      ::execute(Body, Quit),
      (Quit == true -> true;
       run),!.

   run.

   :- meta_predicate([
      check(*),
      execute(*, *)
   ]).

   :- protected([
      check/1,
      execute/2
   ]).

   check([]).
   check([Goal| Tail]) :-
      call(Goal),
      check(Tail).

   execute([], false):-!.
   execute([halt| _], true):-!.
   execute([Goal| Tail], Result):-
      call(Goal), !,
      execute(Tail, Result).

:- end_object.
```

Поясним новые конструкции, использованные в реализации ядра машинф вывода на образцах.  Декларация ```:- op(1199, xfx, '>>>').``` задает неассоциативный _инфиксный оператор_```>>>``` с приоритетом 1199, на единицу меньший, чем максимальный приоритет 1200.  Этот оператор одновременно является защищенным динамическим методом, то есть, из конструкций вида ```[<элементы шаблона>]``` ```>>>``` ```[<команды тела>]``` можно теперь в ```pattern_engine``` задавать образцы.  На самом деле в этом классе жтот оператор в таком виде не используется, но мы его оставим, чтоб копировать его определение в подклассы, иак как ```:- op(...).```, заданные внутри объектов не наследуются потомками.

Другой новой декларарацией является ```:- meta_predicate([check(*), ...]).```.  Декларация требуется компилятору Logtalk для понимания при каких обстоятельствах будут **термы**, перечисленные в шаблоне и теле **запускаться** предикатом ```call/2```.  В данном случае мы сообщаем, что аргумент ```check/1``` не вызывается непосредственно (не является выражением пролога), так как он является списком.  Аналогично предоставляется информация и для execoute/2, у которого первй аргумент - список коман, вызываемых опать же ```call/1```-ом, второй аргумент - терм (```true``` или ```false```),  сообщающий машине, надо ли продолжать дальше выводить.

## Набор образцов для подсчета суммы чисел

Простейший классический пример машины на образцах - подсчет суммы чисел, хранящихся в рабочей памяти машины, в нашем случае объекте Logtalk, унаследованном от ```pattern_engine```.  Чтобы использовать инфиксную запись образцов, сообщим Logtalk информацию об инфиксном операторе ```>>>```, затем зададим правила:

- **Если** в рабочей памяти машины есть *число* X, а нет никакой *накопленной суммы*, **То** удалить это число и добавить факт о том, что накоплена *сумма*, равная X .
- **Если** в рабочей памяти есть и *число* X и *накопленная сумма* S, *То* заменить их на *наколенную сумму* X+S .
- **Если** в рабочей памяти не осталось *чисел*, но есть *накопленная сумма* S, *То* удалить эту *накопленную сумму* S и распечатать S .

Наша машина остановиться сама, так как после выполнения третьего правила рабочая память будет пуста.

```logtalk
% File: patterns.lgt

:- object(count_sums,
   extends(pattern_engine)).

   :- op(1199, xfx, '>>>').

   [::number(X), \+ ::sum(_)]
      >>>
      [::retract(number(X)), ::assert(sum(X))].

   [::number(X), ::sum(S)]
      >>>
      [::retract(number(X)), ::retract(sum(S)),
       SX is S+X,
       ::assert(sum(SX))].

   [\+ ::number(_), ::sum(S)]
      >>>
      [::retract(sum(S)),
       format('Общая сумма: ~w\n', [S])].

:- end_object.
```

Можем заметить, что в данной реализации набора правил используется вызовы вида ```::assert/1```, ```::retract/1```, с использованием ``::/1``.  Такая форма вызова означает, что манипулировать надо базой данных объекта, который был вызван (*receiver context*).  Такие вызовы будут вносить изменение в базу данных объекта-наследника, где будут определены удаляемые и добавляемые предикаты.  Receiver context обозначается в Logtalk ```self```.  Контекст реализации (*implementatoin context*) обозначается ```this```.  Подробно эти загадочные контексты мы разберем в следующих разделах.

## Данные для обработки

Осталось самое простое - сформировать начальное состояние машины.  Для этого при помощи директив опишем свойства методов-фактов, задающих числа и наколенные суммы.  Начельное состояние машины - набор чисел ```number(1)```, ```number(10)``` и т.д. Числа выбраны такими, чтобы проще проверять результат работы машины, активирующей образцы.

```logtalk
% File: patterns.lgt

:- object(sums,
   extends(count_sums)).

   :- protected([
      number/1,
      sum/1
   ]).

   :- dynamic([
      number/1,
      sum/1
   ]).

   number(10).
   number(1).
   number(100).

:- end_object.
```

Теперь настало время провести тест нашего вычислителя.

<sample-output>

$ **swilgt patterns.lgt**

Logtalk 3.91.1
Copyright (c) 1998-2025 Paulo Moura

?- **sums::run.**
Общая сумма: 111
true.

</sample-output>

## Экспертная система выбора вина

Итак, теперь самое интересное, реализуем экспертную систему ```wine``` выбора вина к застолью из статьи https://www.academia.edu/17305576/WINE_ADVISOR_EXPERT_SYSTEM_USING_DECISION_RULES .  Сразу замечу, пример приводится не с целью рекламы потребления алкогольных изделияй, просто набор правил попался раньше всего, и этот набор подходящего размера (примерно 40 правил).

Экспертная система кроме механизмма вывода еще содержит два модуля: *интерфейс пользователя*, *объяснение вывода*.  Задача интерфейса пользователя - задавать вопросы пользователю на понятном (естественном) языке и кодироват ответы в структуры, понимаемые машиной вывода (факты рабочей памяти).  Блок объяснения вывода отвечает на вопросы вида "как?" [получено заключение] (*how?*) и "почему?" [мне задается этот вопрос] (*why?*). Мы реализуем эти блоки в необходимой степени.

Сначала надо усовершенствовать машину вывода таким образом, чтобы сохранять порядок активации правил, имена которых обозначаются специальным предикатом ```rule/1```.

```logtalk
:- object(es_engine,
   extends(pattern_engine)).

   :- dynamic([fired_rule/1,
      consider_rule/1]).
   :- protected([fired_rule/1,
      consider_rule/1]).
   :- dynamic(answered/2).
   :- protected(answered/2).

   :- protected(rule/1).

   rule(RuleName) :-
      retractall(fired_rule(_)),
      retractall(consider_rule(_)),
      assertz(consider_rule(RuleName)).

   run :-
      :: >>>(Pattern, Body),
      ::check(Pattern), !,
      ::considered_rule(RuleName),
      assertz(fired_rule(RuleName)),
      ::execute(Body, Quit),
      (Quit == true -> true;
       run), !.

   run.

:- end_object.
```

Еще одно усовершенствование - собрать информацию о том, какие значения фактов можно вывести при помощи правил, а какие возможно только спросить у пользователя.

```logtalk
:- object(wine_es_scan,
   extends(es_engine)).

   :- op(1199, xfx, '>>>').

   [::rule('Выбор вина для аперитива'),
      ::
      ::number(X), \+ ::sum(_)]

:- end_object.
```

Далее интерпретируем правила из статьи в виде образцов.

```logtalk
:- object(wine_es_scan,
   extends(es_engine)).

   :- op(1199, xfx, '>>>').

   [rule(1-'Выбор вина для аперитива'),
      this_wine('для употребления до еды (аперитив)')]
      >>>
      [assert(recommended_generic_wine_type('вино для аперитива'))] .

   [rule(2-'Выбор румынского вина'),
      this_wine('вино для аперитива')]
      >>>
      [assert(recommended_generic_wine_type('румымское вино'))] .

   % . . . . . . . . . .

:- end_object.
```

## Приложение А Список правил экспертной системы

```ini
# APERITIF
RULE 1 [Choosing a aperitif wine]
If [this wine] = "to be consumed before a meal (aperitif)"
Then [a recommended generic wine type] = "aperitif wine"

RULE 2 [Choosing a Romanian wine]
If [this wine] = "aperitif wine"
Then [a recommended generic wine type] = "Romanian wine"

RULE 3 [Choosing a full bodied aperitif Romanian]
If [a recommended generic wine type] = "Romanian wine"
[the preferred body] = "full" and
Then [a recommended generic wine type] = "Cotnary vineyard wine"

RULE 4
If [a recommended generic wine type] = "Cotnary vineyard wine"
And
[a sparkling wine is preferred] = true
Then [a recommended generic wine type] = "sparkling wine"

RULE 5
If [a recommended generic wine type] = “sparkling wine"
Then [a recommended generic wine type] = "Champagne"

RULE 6
If [a recommended generic wine type] = "Champagne"
And
[the preferred colour] = white
And
[the preferred body] = "full/medium/light"
Then [a recommended generic wine type] = "White Champagne"

RULE 7
If [a recommended generic wine type] = "Champagne"
And
[the preferred colour] = pink
And
[the preferred body] = "full/medium/light"
Then [a recommended generic wine type] = "Pink Champagne"

RULE 8
If [a recommended generic wine type] = "Champagne"
And
[the preferred colour] = black
And
[the preferred body] = "full/medium/light"
Then [a recommended generic wine type] = "Black Champagne"

RULE 9
If [a recommended generic wine type] = "Cotnary vineyard wine"
And
[a sparkling wine is preferred] = false;
Then [a recommended generic wine type] = "table wine"

RULE 10
If [a recommended generic wine type] = "table wine"
And
[the colour]=white
Then [a recommended wine] = " Tamaioasa Romaneasca "

RULE 11
If [a recommended generic wine type] = "table wine"
And
[the colour]=pink
Then [a recommended wine] = " Busuioaca Romaneasca wine "

RULE 12
If [a recommended generic wine type] = "table wine"
And
[the colour]=red
Then [a recommended wine] = " Cabernet Sauvignion wine "

RULE 13
If [a recommended generic wine type] = "Romanian wine"
and
[the preferred body] = "medium"
Then [a recommended generic wine type] = "Dealu Mare vineyard wine"

RULE 14
If [a recommended generic wine type] = "Dealu Mare vineyard wine"
And [a sparkling wine is preferred] = false;
[the colour]=white;
Then [a recommended wine ] = "Feteasca Regala wine"

RULE 15
If [a recommended generic wine type] = "Dealu Mare vineyard wine"
And
[a sparkling wine is preferred] = false
And
[the colour]=pink
Then [a recommended wine ] = "Feteasca Roz wine"

RULE 16
If [a recommended generic wine type] = "Dealu Mare vineyard wine"
And
[a sparkling wine is preferred] = false
And
[the colour]=red
Then [a recommended wine ] = "Feteasca Neagra,Pinot Noir or Cabernet Byzantium
wine"

RULE 17
If [a recommended generic wine type] = "Romanian wine"
and
[the preferred body] = "light"
Then [a recommended generic wine type] = "Jidvei vineyard wine"

RULE 18
If [a recommended generic wine type] = "Jidvei vineyard wine"
And
[a sparkling wine is preferred] = false
And
[the colour]=white
Then [a recommended wine ] = "Sauvignion Blanc or Dry Muscat wine"

RULE 19
If [a recommended generic wine type] = "Jidvei vineyard wine"
And
[a sparkling wine is preferred] = false
And
[the colour]=pink
Then [a recommended wine ] = "Feteasca Regala wine"

RULE 20
If [a recommended generic wine type] = "Jidvei vineyard wine"
And
[a sparkling wine is preferred] = false
And
[the colour]=red
Then [a recommended wine ] = "Pinot Noir wine"

RULE 21
If [the wine is to accompany an entree]
Then
If [a recommended generic wine type] = "dinner(entree) wine"

RULE 22 % [Is the wine for a white meat entree?]
If [this wine] = "entrée wine" and
[the entree] : "white meat"
[the colour]:”white”
Then [a recommended wine] = "Muscat Sec,Samur Blanc or Griffin Vineyards
Verdelho"

RULE 23 % [Is the wine for a white meat entree?]
If [this wine] = "entrée wine" and
[the entree] : "white meat"
[the color]:”rose”
Then [a recommended wine] = "Pinot Grigio Rose,Veneto or Cuvee des Amardies
Rose"

RULE 24 % [Is the wine for a white meat entree?]
If [this wine] = "entrée wine" and
[the entree] : "white meat"
[the color]:”red”
Then [a recommended wine] = "Corbieres 2003,Domain Modelen or Mayor de
Castilla 2004,Ribera del Duero"

RULE 25 % [Is the wine for a steak entree?] 25
If [this wine] = "entrée wine" and
[the entree] : "steak" and
[the colour]:”white”
THEN [a recommended wine] = "Chateau Agnel 2000,Minervois(France) or Chateau
Syrah 2004(France)"

RULE 26 % [Is the wine for a steak entree?]
If [this wine] = "entrée wine" and
[the entree] : "steak"
[the color]:”rose”
Then [a recommended wine ] = "Busuioaca Romaneasca"

RULE 27 % [Is the wine for a steak entree?]
If [this wine] = "entrée wine" and
[the entree] : "steak"
[the color]:”red”
Then [a recommended wine ] = "Rosso di Sicilia 2004,Cantine Settesoli(Italy)"

RULE 28 % [Is the wine for a barbecues entree?]
If [this wine] = "entrée wine" and
[the entree] : "barbecues"
[the colour]:”white”
Then [a recommended wine] = "Riojo Blanco CVNE 2003,Vino Real (Spain) or
Chateau Camplazens Syrah 2004(France)"

RULE 29 % [Is the wine for a barbecues entree?]
If [this wine] = "entrée wine" and
[the entree] : "barbecues"
[the color]:”rose”
Then [a recommended wine type] = "Tamaioasa Regala(Romania)"

RULE 30 % [Is the wine for a barbecues entree?]
If [this wine] = "entrée wine" and
[the entree] : "barbecues"
[the color]:”red”
Then [a recommended wine type] = "Riojo Tempranillo 2003(Italy),Berberana(Spain)
or Protocola Tinto(Spain)"

RULE 31 % [Is the wine for a pizza entree?]
If [this wine] = "entrée wine" and
[the entree] : "pizza"
[a recommended wine type] = "Chianti wine"

RULE 32 % [Is the wine for Chinese Food entree?]
If [this wine] = "entrée wine" and
[the entree] : "Chinese Food"
Then [a recommended wine type] = "White Riesling wine"

RULE 33 % [Is the wine for cheese entree?]
If [this wine] = "entrée wine" and
[the entree] : "cheese"
Then [a recommended wine type] = "Burgundy wine"

RULE 34 % [Is the wine for fish entree?]
If [this wine] = "entrée wine" and
[the entree] : "fish"
Then [a recommended wine type] = "Chardonnay wine"

RULE 35 % [Is the wine for salads entree?]
If [this wine] = "entrée wine" and
[the entree] : "salads"
Then [a recommended wine type] = "Bordeau Blanc wines"

# DESSERT WINE

RULE 36 % [dessert class]
If [this wine] = "is to accompany an dessert" and
Then [a recommended generic wine type] = “dessert wine"

RULE 37 % [Fruit-based dessert]
If [this wine] = "dessert wine" and
[the dessert] = "fruit or primarily fruit"
Then [a suggested wine type] = "Rougeon"

RULE 38 % [Sweet dessert]
If [this wine] = "dessert wine" and
[the dessert] = "very sweet such as chocolate"
Then [a recommended generic wine type] = "Port"

# AFTER DINNER WINE

RULE 39 % [After dinner]
If [this wine] = "to be consumed after dinner"
```

<!--

A quiz to review the contents of this section:

<quiz id="6bfd7e0d-2998-5697-80dc-418703fabbbf"></quiz>

-->
