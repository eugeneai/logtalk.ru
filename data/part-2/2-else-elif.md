---
path: '/part-2/2-datalog'
title: 'Язык запросов Datalog'
hidden: false
---

<text-box variant='learningObjectives' name="Цель освоения материала">

В результате изучения этого раздела вы

- Получите представление об использовании Prolog в качестве языка запросов к базе данных
- Научимся выводить результаты запросов в виде таблиц
- Вспомним использование предикатов ```setof/3```, ```bagof/3```, ```findall/3```
- Научитесь инсталлировать внешние пакеты SWI-Prolog и использовать модули Prolog в программах Logtalk

</text-box>

Согласно [Wikipedia] Datalog - это декларативный логический язык программирования, синтаксическое подмножество Prolog.  Основным отличием Datalog от Prolog является модель вывода "от фактов к запросу", а не "от запроса к фактам".  Эта разница приводит к значительно отличающемуся поведению и свойствам от Prolog.  Часто используется как язык запросов для дедуктивных баз данных.  Datalog применяется к решению проблем интеграции данных, сетей, анализа программ и многому другому.

Программа Datalog состоит из фактов, которые являются утверждениями, которые считаются истинными, и правил, которые говорят, как вывести новые заключения из известных фактов.  Семантическая модель программы, ее смысл, выражается множеством всех фактов, которые можно вывести из исходных фактов и правил.  Папример для Datalog-программы

```prolog
parent(xerces, brooke).
parent(brooke, damocles).

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).
```

семантической моделью будет следующий набор фактов.

```prolog
parent(xerces, brooke).
parent(brooke, damocles).
ancestor(xerces, brooke).
ancestor(brooke, damocles).
ancestor(xerces, damocles).
```

В Datalog также запрещены к использованию сложные термы, вроде такого: ```sister(wife(socrates))```.

Здесь нас больше будут интересовать возможности Prolog и Logtalk в инкапсуляции баз данных в виде объектов.  Ввиду простоты языка в сравнении, например, с SQL, SPARQL и XPath, системы на основе Datalog в настоящее время часто используют как верхний уровень управления запросами к базам данных.  Мы построим такую систему над реляционными базами данных SQLite, заодно научимся устанавливать пакеты SQL-Prolog и использовать модкли Prolog в объектах Logtalk.

Для запуска последующих примеров нам надо установить пакет ```prosqlite.pl``` https://www.swi-prolog.org/pack/file_details/prosqlite/doc/html/prosqlite.html .
Система SQLite представляет собой мощную систему управления с "нулевой" конфигурацией, где в качестве баз данных выступают однофайловые базы данных.  Их можно создавать в однихъ системах программирования, и использовать в других.

```prolog
?- pack_install(prosqlite).
```

Эту команду надо набрать в командной строке SWI-Prolog ```sqipl``` или ```sqilgt```, а на все вопросы ответить **y** (yes).

<sample-output>

?- **pack_install(prosqlite).**

Create directory for packages
   (1) \* /home/eugeneai/.local/share/swi-prolog/pack
   (2)   Cancel

Your choice? **y**
<b class="green">% Contacting server at https://www.swi-prolog.org/pack/query ... ok</b>
Installation plan:
  Install prosqlite at version 2.0 from https://stoics.org.uk/~nicos/sware/packs/prosqlite/prosqlite-2.0.tgz <b class="green">(downloaded 1,179 times)</b>
Download packs? Y/n?  **y**
<b class="green">% Downloading prosqlite ... 125,958 bytes
% Contacting server at https://www.swi-prolog.org/pack/query ... ok</b>
The following packs have post install scripts:
  Build prosqlite in directory /home/eugeneai/.local/share/swi-prolog/pack/prosqlite
Run scripts? Y/n? **y**
<b class="green">% Building pack prosqlite in directory /home/eugeneai/.local/share/swi-prolog/pack/prosqlite
% Found foreign libraries for architecture 'x86_64-linux'
% Use ?- pack_rebuild(prosqlite). to rebuild from sources</b>
true.

?- use_module(library(prosqlite)).  % Тестируем загрузку модуля из библиотеки
true.

</sample-output>



Let's have a look at a program which asks the user to input a number, and then prints out different messages based on whether the number is negative, positive, or equal to zero:

```python
number = int(input("Please type in a number: "))

if number < 0:
    print("The number is negative")

if number >= 0:
    print("The number is positive or zero")
```

This looks a bit clumsy and repetitive. We only ever want to execute one of the `if` blocks, because the input will always be either below zero, or zero or above. That is, either `number < 0` or `number >= 0` is true, but never both at the same time. So, the first conditional statement actually contains all we need here. If it is true, the number is negative. If it is false, the number equals zero or is positive.

Instead of creating a whole another conditional statement, as in the example above, it is possible to create another branch of the same conditional statement to cover all cases _where the condition was false_. This is called the `else` statement.

The previous example rewritten:

```python
number = int(input("Please type in a number: "))

if number < 0:
    print("The number is negative")
else:
    print("The number is positive or zero")
```

When using an if-else construction, one and exactly one of the branches will always be executed. The following picture illustrates the structure:

<img src="2_2_1.png">

NB: there can never be an else branch without an if branch before it. The if-else construction as a whole forms a single _conditional statement_.

The following example checks whether a number given by the user is even or not. Parity can be checked with the modulo operator `%`, which produces the remainder of an integer division operation. When divided by two, if the remainder is zero, the number is even. Otherwise the number is odd.

```python
number = int(input("Please type in a number: "))

if number % 2 == 0:
    print("The number is even")
else:
    print("The number is odd")
```

<sample-output>

Please type in a number: **5**
The number is odd

</sample-output>

Another example with string comparison:

```python
correct = "kittycat"
password = input("Please type in the password: ")

if password == correct:
    print("Welcome")
else:
    print("No admittance")
```

With two different inputs this should print out:

<sample-output>

Please type in the password: **kittycat**
Welcome

</sample-output>

<sample-output>

Please type in the password: **monkey**
No admittance

</sample-output>


<in-browser-programming-exercise name="Age of maturity" tmcname="part02-04_age_of_maturity" height="400px">

Please write a program which asks the user for their age. The program should then print out a message based on whether the user is of age or not, using 18 as the age of maturity.

Some examples of expected behaviour:

<sample-output>

How old are you? **12**
You are not of age!

</sample-output>


<sample-output>

How old are you? **32**
You are of age!

</sample-output>

</in-browser-programming-exercise>

## Alternative branches using the elif statement

Often there are more than two options the program should account for. For example, the result of a football match could go three ways: home wins, away wins, or there is a tie.

A conditional statement can be added to with an `elif` branch. It is short for the words "else if", which means the branch will contain an alternative to the original condition. Importantly, an `elif` statement is executed only if none of the preceding branches is executed.

<img src="2_2_2.png">

Let's have a look at a program which determines the winner of a match:

```python
goals_home = int(input("Home goals scored: "))
goals_away = int(input("Away goals scored: "))

if goals_home > goals_away:
    print("The home team won!")
elif goals_away > goals_home:
    print("The away team won!")
else:
    print("It's a tie!")
```

This program could print out three different statements given different inputs:

<sample-output>

Home goals scored: **4**
Away goals scored: **2**
The home team won!

</sample-output>

<sample-output>

Home goals scored: **0**
Away goals scored: **6**
The away team won!

</sample-output>

<sample-output>

Home goals scored: **3**
Away goals scored: **3**
It's a tie!

</sample-output>

In the above example there are three alternative branches, exactly one of which will always be executed. However, there is no limit to the number of `elif` branches a conditional statement can contain, and the `else` branch is not mandatory.

This is also a valid conditional statement:

```python
print("Holiday calendar")
date = input("What is the date today? ")

if date == "Dec 26":
    print("It's Boxing Day")
elif date == "Dec 31":
    print("It's Hogmanay")
elif date == "Jan 1":
    print("It's New Year's Day")

print("Thanks and bye.")
```

<sample-output>

Holiday calendar
What is the date today? **Dec 31**
It's Hogmanay
Thanks and bye.

</sample-output>

Notice the previous example has no `else` branch. If the user inputs a date which is not mentioned in any of the `if` or `elif` branches, or inputs a date in a different format, none of the three branches of the conditional statement is executed.

<sample-output>

Holiday calendar
What is the date today? **Dec 25**
Thanks and bye.

</sample-output>

<in-browser-programming-exercise name="Greater than or equal to" tmcname="part02-05_greater_or_equal"  height="400px">

Please write a program which asks for two integer numbers. The program should then print out whichever is greater. If the numbers are equal, the program should print a different message.

Some examples of expected behaviour:

<sample-output>

Please type in the first number: **5**
Please type in another number: **3**
The greater number was: 5

</sample-output>

<sample-output>

Please type in the first number: **5**
Please type in another number: **8**
The greater number was: 8

</sample-output>

<sample-output>

Please type in the first number: **5**
Please type in another number: **5**
The numbers are equal!

</sample-output>

</in-browser-programming-exercise>


<in-browser-programming-exercise name="The elder" tmcname="part02-06_elder" height="550px">

Please write a program which asks for the names and ages of two persons. The program should then print out the name of the elder.

Some examples of expected behaviour:

<sample-output>

Person 1:
Name: **Alan**
Age: **26**
Person 2:
Name: **Ada**
Age: **27**
The elder is Ada

</sample-output>

<sample-output>

Person 1:
Name: **Bill**
Age: **1**
Person 2:
Name: **Jean**
Age: **1**
Bill and Jean are the same age

</sample-output>

</in-browser-programming-exercise>

<in-browser-programming-exercise name="Alphabetically last" tmcname="part02-07_alphabetically_last"  height="500px">

Python comparison operators can also be used on strings. String `a` is smaller than string `b` if it comes alphabetically before `b`. Notice however that the comparison is only reliable if
- the characters compared are of the same case, i.e. both UPPERCASE or both lowercase
- only the standard English alphabet of a to z, or A to Z, is used.

Please write a program which asks the user for two words. The program should then print out whichever of the two comes alphabetically last.

You can assume all words will be typed in lowercase entirely.

Some examples of expected behaviour:

<sample-output>

Please type in the 1st word: **car**
Please type in the 2nd word: **scooter**
scooter comes alphabetically last.

</sample-output>

<sample-output>

Please type in the 1st word: **zorro**
Please type in the 2nd word: **batman**
zorro comes alphabetically last.

</sample-output>

<sample-output>

Please type in the 1st word: **python**
Please type in the 2nd word: **python**
You gave the same word twice.

</sample-output>

</in-browser-programming-exercise>

<!--

A quiz to review the contents of this section:

<quiz id="82f644fe-5d89-5153-842a-11d5d11bc059"></quiz>

-->
