---
path: "/part-1/5-exercizes-list"
title: "Упражнения и тесты"
hidden: true
---

<text-box variant='learningObjectives' name="Упражнения и тесты">

Вы изучили первую часть, теперь надо сделать упражнения из файла ```Set1.lgt``` с набором тестов ```Set1Tests.lgt```.

- Упражнение 1-11.

</text-box>

## Как выполнять тесты

Задачи упражнений располагаются в файлах, назваемых в следующими именами: ```Set1.lgt```, ```Set2.lgt``` и так далее.  Каждому файлу упражнений соответствуют файлы тестов ```Set1Tests.lgt```, ```Set2Tests.lgt``` ...  Чтобы запустить, например, тест 1 надо выполнить команду:

```bash
$ swilgt -s Set1TestsLoader.lgt -q 'halt.'
```

В результате компилятор Logtalk загрузит библиотеку тестирования, ваши решения и программу тестирования, выполнит тесты, покажет результат на экран и закончит свое исполнение.

<sample-output>

$ *swilgt -s Set1TestsLoader.lgt -q halt.*

<b class="green">%     . . . . . . . .
%     library(process) compiled into process 0.01 sec, 70 clauses</b>

For help on Logtalk, type help::help.

<b class="blue">Задача 'Задача 1' решена!
Задача 'Задача 2' решена!
Задача 'Задача 3' решена!
Задача 'Задача 4' решена!
Задача 'Задача 5' решена!
Задача 'Задача 6' решена!
Задача 'Задача 7' решена!
Задача 'Задача 8' решена!
Задача 'Задача 9' решена!
Задача 'Задача 10' решена!
Задача 'Задача 11' решена!
Задача 'Практическая работа 1' решена!
Тестирование закончено.</b>

</sample-output>

Если необхдоимо после тестирования необходма командная строка, то надо убрать параметр ```-q 'halt.'```

<sample-output>

$ *swilgt -s Set1TestsLoader.lgt -q halt.*

<b class="green">%     . . . . . . . .
%     library(process) compiled into process 0.01 sec, 70 clauses</b>

For help on Logtalk, type help::help.

<b class="blue">Задача 'Задача 1' решена!
Задача 'Задача 2' решена!
Задача 'Задача 3' решена!
Задача 'Задача 4' решена!
Задача 'Задача 5' решена!
Задача 'Задача 6' решена!
Задача 'Задача 7' решена!
Задача 'Задача 8' решена!
Задача 'Задача 9' решена!
Задача 'Задача 10' решена!
Задача 'Задача 11' решена!
Задача 'Практическая работа 1' решена!
Тестирование закончено.</b>

Welcome to SWI-Prolog (threaded, 64 bits, version 9.2.9)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- _

</sample-output>


<!--

A quiz to review the contents of this section:

<quiz id="bc7e500f-a91e-5709-8ae6-34637ff01737"></quiz>

Please respond to a quick questionnaire on this week's materials. The questionnaire is worth one exercise point.

<quiz id="bcfae104-8cad-5fcd-ac64-d65ae689ac5f"></quiz>

-->
