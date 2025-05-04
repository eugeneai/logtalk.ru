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

В приведенном примере используется директива ```use_module/1```, аргументом которой является терм ```library(prosqlite)```, обозначающий библиотеку ```prosqlite```, расположенную в специально каталоге, содержащем библиотечные файлы, - элегантный способ представления местоположения библиотек, унаследованный Logtalk у Prolog.

Библиотека proSQLite поддерживает три уровня взаимодействия с базами данных SQLite.  На самом нижнем уровне взаимодействие реализуется при помощи запросов SQL.  Второй уровень позволяет запрашивать базу данных как словарь, а на верхнем уровене реализует просмотр таблиц баз данных инкасулированно в предикаты.

## Подключение к базе данных

Загрузим базу данных ```sqlite``` с этой ссыки https://www.sqlitetutorial.net/wp-content/uploads/2018/03/chinook.zip и разархивировать.  Структура базы данных здесь: https://www.sqlitetutorial.net/wp-content/uploads/2018/03/sqlite-sample-database-diagram-color.pdf .

```bash
$ unzip chinook.zip
$ mv chinook.db chinook.sqlite
```

Попробуем подключиться к базе данных и выполнить запрос.

<sample-output>

**$ swilgt**

?- **use_module(library(prosqlite)).**
true.

?- **sqlite_connect('chinook', chinook, as_predicates(true)).**
true.

?- **forall(sqlite_current_table( chinook, Table ),
   format('~w\n', [Table])).**
albums
sqlite_sequence
artists
customers
employees
genres
invoices
invoice_items
media_types
playlists
playlist_track
tracks
sqlite_stat1
true.

?- **employees(Id,LastName,FirstName,Job,A,F,T,Street,
         City,Region,Country,Code,Phone1,Phone2,Email).**
Id = 1,
LastName = 'Adams',
FirstName = 'Andrew',
Job = 'General Manager',
A = '',
F = '1962-02-18 00:00:00',
T = '2002-08-14 00:00:00',
Street = '11120 Jasper Ave NW',
City = 'Edmonton',
Region = 'AB',
Country = 'Canada',
Code = 'T5K 2N1',
Phone1 = '+1 (780) 428-9482',
Phone2 = '+1 (780) 428-3457',
Email = 'andrew@chinookcorp.com' **;**
Id = 2,
LastName = 'Edwards',
FirstName = 'Nancy',
Job = 'Sales Manager',
A = '1',
F = '1958-12-08 00:00:00',
T = '2002-05-01 00:00:00',
Street = '825 8 Ave SW',
City = 'Calgary',
Region = 'AB',
Country = 'Canada',
Code = 'T2P 2T3',
Phone1 = '+1 (403) 262-3443',
Phone2 = '+1 (403) 262-3322',
Email = 'nancy@chinookcorp.com' **.**

</sample-output>

Инкапсулируем нашу базу данных в объект ```chinook_db/0```.

```logtalk
% File: chinook_db.lgt

:- object(sqlite_db).

   :- protected(db_file_name/1).
   :- public(connect/0).

   :- use_module(library(prosqlite)).

   connect:-
      ::db_file_name(FileName),
      sqlite_connect(FileName, db, as_predicates(true)).

   :- public([
         show_tables/0, show_columns/0, show_counts/0
      ]).

   show_tables :-
      forall(
         sqlite_current_table(db, Table),
         format('~w\n', [Table])).

   show_columns :-
      forall(
         sqlite_current_table(db, Table),
         (  format('~w ===== \n', [Table]),
            forall(sqlite_table_column(db, Table, Col),
            format('~w ', [Col])),
            nl
         )).

   show_counts :-
      forall(
         (sqlite_current_table(db, Table),
          sqlite_table_count(db, Table, Count)),
         format('~w:~w\n', [Table, Count])).

:- end_object.


:- object(chinook_db,
   extends(sqlite_db)).

   db_file_name('chinook').

   :- initialization((
      ::connect
   )).

:- end_object.
```

Поясним некоторые строки исходного кода файла.  Метод ```db_file_name/1``` определяет файл базы данных, к кторому требуется осуществить подключение.  Директива ```initialization/1``` играет роль конструктора объекта и содержит запрос, который выполняется как только объект загружен в оперативную память Logtalk.  В данном случае самому себе посылается сообщение ```::connect/0``` с целью немедленного подключения к базе данных.  В данный момент объект ```chinook_db/0``` представляет собой конфигурацию, надстроенную над ```sqlite_db/0```.

Директива ```use_module/1``` импортирует **все предикаты** из библиотеки ```prosqlite``` в область видимости объекта ```sqlite_db/0```.  Импортированные предикаты не наследуются в дочерние объекты, так как имеют область видимости private.  Кроме ```use_module/1``` есть еще ```use_module/2```, где второй аргумент содержит список импортируемых предикатов, которые можно в процессе импорта переименовывать и каррировать.  Переименование позволяет решать проблему возможного наложения имен в общем пространстве (scope).

Инкасулируем пару таблиц (```artists``` и ```albums```) из базы данных в виде публичных методов ```artist/2``` и ```album/3``` (удобнее использовать единственное число в качестве имени метода).  Оба метода в качестве первого аргумента используют ключевое поле ```Id```, остальные аргументы соответствуют колонкам таблицы.  Реализацию доступа к строкам таблиц реализуем при помощи вспомогательного protected-матода ```collection/2```.  Метод принимает первый аргумет, название таблицы, и возвращает строку ```row/N``` из таблицы.  Тестирование библиотеки proSQLite показало, что библиотека не поддерживает одновременное отслеживание двух курсоров SQL-запросов, поэтому в ```collection/2``` мы сначала загружаем все строки в промежуточный список, затем выдаем элементы этого списка.  Такой вариант - это не совсем то, что мы хотели реализовать, но для демонстрации идеи Datalog достаточно.

Изменим реализацию ```chinook_db/0``` в соответствии с требованиями выше.  В реализации используется предикат стандартной библиотеки ```library(lists)``` ```member/2```, истинный, если первый элемент является элементом списка (второй параметр).

```logtalk
:- object(chinook_db,
   extends(sqlite_db)).

   db_file_name('chinook').

   :- initialization((
      ::connect
   )).

   :- use_module(library(prosqlite)).

   :- public(album_of_artist/2).

   album_of_artist(Name, AlbumTitle) :-
      ::artist(ArtistId, Name),
      ::album(_, AlbumTitle, ArtistId).

   :- public([
      artist/2, album/3
   ]).

   artist(ArtistId, Name) :-
      ::collection('artists', row(ArtistId, Name)).

   album(AlbumId, Title, ArtistId) :-
      ::collection('albums', row(AlbumId, Title, ArtistId)).

   % Вспомогательные предикаты

   :- protected([
      collection/2
   ]).

   :- use_module(library(lists), [member/2]).

   collection(TableName, Row):-
      setof(R,
        sqlite_format_query(db, 'SELECT * FROM ~w'-TableName, R),
        Rows), !,
      member(Row, Rows).

:- end_object.
```

Текст после комментария ```% Вспомогательные предикаты``` правильнее было б переместить в ```sqlite_db/0```, что мы потом и сделаем.  Предикат библиотеки ```library(prosqlite)``` ```sqlite_format_query/3``` подготавливает запрос, применяя параметры ```TableName``` к строке шаблону ```'SELECT * FROM ~w'```.  Предикат ```setof/3``` собирает все строки таблицы в список без повторений.

В ```chinook_db/0``` теперь находятся три Datalog-метода ```artist/3```, ```album/3``` и ```album_of_artist/2```, релизованный в виде правила - запроса Datalog.

<sample-output>

$ **swilgt chinook_db.lgt**
<b class="green">% [ /home/eugeneai/tmp/tst/chinook_db.lgt loaded ]
% 0 compilation warning</b>

?- **forall(chinook_db::album_of_artist(Artist, Album),**
|    **format('~w - ~w\n', [Artist, Album])).**
AC/DC - For Those About To Rock We Salute You
AC/DC - Let There Be Rock
Accept - Balls to the Wall
Accept - Restless and Wild
Aerosmith - Big Ones
Alanis Morissette - Jagged Little Pill
Alice In Chains - Facelift
Antônio Carlos Jobim - Warner 25 Anos
Antônio Carlos Jobim - Chill: Brazil (Disc 2)
Apocalyptica - Plays Metallica By Four Cellos
Audioslave - Audioslave
Audioslave - Out Of Exile
Audioslave - Revelations
BackBeat - BackBeat Soundtrack
% . . . . . . . . .

</sample-output>

Давате теперь добавим к выдаче еще один столбец с адресом группы на Wikipedia.

```logtalk
% File: chinook_db.lgt

% . . . . . . . . . .

:- object(chinook,
   extends(chinook_db)).

   :- public(artists_album/3).

   artists_album(Artist, Album, URL) :-
      ::album_of_artist(Artist, Album),
      ::artist_url(Artist, URL).

   :- protected([
      artist_url/2
   ]).

   :- use_module(library(uri)).
   :- use_module(library(pcre)).

   artist_url(Artist, IRI) :-
      re_replace("-", "_", Artist, Ar),
      atom_concat('http://wikipedia.org/wiki/', Ar, URL),
      uri_normalized_iri(URL, IRI).

:- end_object.
```

Для синтеза гипотетичексого URL группы пришлось использовать две вспомогательные библиотеки. Первая, ```library(pcre)``` содержит предикаты для манипулирования строками и атомами при помощи регулярных выражений, представленных близко к стандарту языка Perl.  Вторая библиотека манипулирует атомами в предположении, ято они URL-ы (адреса в интернете).

Наследуя ```chinook_db/0``` в новый объект ```chinook/0```, мы уходим от привязки объекта к базе данных (абстрагируемся).  В ```chinook/0``` все правила строятся на основе только сообщений без привязки к предикатам ```sqlite_*```.  Программу можно улучшить, если переименовать ```artists_album/3``` в ```album_of_artist/3```, следуя правилу "не преумножай сущности без необходимости".

Пример работы объекта ```chinook/0```.

<sample-output>

% Проверим генератор URL-ов

?- **chinook<<artist_url('Дональд Трамп', URL).**
URL = 'http://wikipedia.org/Дональд%20Трамп'.

?- **chinook<<artists_album(Artist, Album, URL).**
Artist = 'AC/DC',
Album = 'For Those About To Rock We Salute You',
URL = 'http://wikipedia.org/wiki/AC/DC' ;
Artist = 'AC/DC',
Album = 'Let There Be Rock',
URL = 'http://wikipedia.org/wiki/AC/DC' ;
Artist = 'Accept',
Album = 'Balls to the Wall',
URL = 'http://wikipedia.org/wiki/Accept' ;
Artist = 'Accept',
Album = 'Restless and Wild',
URL = 'http://wikipedia.org/wiki/Accept' ;
Artist = 'Aerosmith',
Album = 'Big Ones',
URL = 'http://wikipedia.org/wiki/Aerosmith'

</sample-output>

Аналогично приведенному примеру можно объединять запросы от нескольких источников данных.  Основная проблема приведенных примеров - невысокая скорость работы Datalog-системы.  Однако его абстрактный синтаксис и семантика могут быть использованы, а именно сами определения методов в объекте, проанализированы и а) сгенерированы специализированные варианты комплексных SQL-запросов, реализующих метод, б) построены параллельные схемы реализации запросов.

<!--

A quiz to review the contents of this section:

<quiz id="82f644fe-5d89-5153-842a-11d5d11bc059"></quiz>

-->
