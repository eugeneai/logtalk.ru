---
path: '/part-16/1-reverse-engineering'
title: 'Реверс-инжениринг'
hidden: false
---

Реверс-инжениринг (reverse engineering), называеемый также "обратная инженерия", используется для восстановления спецификаций сложных техънических объектов через анализ компонент и их взаимодействия.  В инфораматике реверс-инжениринг применяется в анализе кода программ, в частности, вирусов и малвари, протоколов передачи данных.  Целями реверс-инжениринга являются

- анализ качества дизайна, уровня защищенности, сложности реверс-инжениринга
- оценка качества инструмента, например, помпилятора, генерирующего машинный код
- восстановление программы, если потерян оригинальный исходный код

## Анализ протокола управления EPR-580

Реверс-инжениринг требовался дл анализа кодов управления осциллографом.  После инсталляции в 2009 году прибора, система управления 2.5 Гввс (гигавыборок в секунду) 8 бит осциллографа вышла из строя.  Компания прислала специалиста, проблема была исправлена.  Осциллограф проработал пару лет, и опять сломался, дальнейший ремонт стоил бы больших денег.

Для восстановления работоспособности осциллографа самостоятельно было принято решение пересобрать драйвер - модуль операционной системы Linux.  Исходный код новой версии имеется в диструбутиве, однако он не работал напрямую.  Необходимо было роаспознать обстановку, когда подсистема осциллографа подключается к система прибора EPR-580.

Адрес проекта: https://github.com/eugeneai/traffic-analyzer

## Загрузчик объектов

```logtalk
:- set_prolog_flag(stack_limit, 8_147_483_648).

:- initialization((
    % установить глобальные флаги для проекта
    set_logtalk_flag(report, warnings),
    set_logtalk_flag(events, allow),
    set_logtalk_flag(debug, on),
    logtalk_load(tutor(loader)),
    logtalk_load(tools(loader)),  % отладка, трассирование
    logtalk_load(debugger(loader)),  % отладка

    % загрузить исходные файлы проекта

    logtalk_load([pkt,'pkt_run']),
    pkt_run::run,  % Запуск анализатора
    true
)).
```

## Модуль анализа трафика

Модуль анализа трафика содержит объекты, распознающие среди цепочек событий передачи данных соединения, сообщения, а также команды от одной системы к другой.  Часть объектов представляют собой фасад-адаптеры, удобно представляющие данные для объектов распознавания.  Также есть утилитарные объекты, обеспечивающие доступ к данныт трафика и запись результата в выходной поток.

```logtalk
:- use_module(library(option),[option/2]).
:- use_module(library(pprint),[print_term/2]).

:- object(pkt_run).
  :- use_module(library(option), [option/2]).
  :- use_module(library(pprint), [print_term/2]).

  :- public(run/0).
  % Разные уровни анализа трафика
  run:-
    analyze(commands).
    % analyze(messages).
    % analyze(frames).

  % варианты загрузки исходного трафика
  load_all_tcp_icmp:-
    pcap('ih-tmp/very-total.pcap','tcp or icmp')::load.

  load_non_zero_length:-
    pcap('ih-tmp/very-total.pcap','tcp.len>0 or icmp')::load.

  % использование конфигурация объекта-конфигурации
  connect_sniffer(Snif) :-
    pcap_config::current_option(db_name,DBFile),
    Snif=db(DBFile),
    Snif::connect_db.

  % варианты анализа, отличаются глубиной
  analyze(frames):-
    pcap_config::current_option(event_store_name, FileName),
    connect_sniffer(Snif),
    Saver = event_saver(FileName),
    Saver::connect,
    frame_analyzer(Snif, Saver)::run(LastState),
    Saver::disconnect,
    format('LastState:~w~n',[LastState]).

  analyze(messages):-
    pcap_config::current_option(event_store_name, StoreName),
    pcap_config::current_option(message_store_name, MessageName),
    Events = term_reader(StoreName),
    Saver = event_saver(MessageName),
    Saver::connect,
    message_analyzer(Events,Saver)::run,
    Saver::disconnect.

  analyze(commands):-
    pcap_config::current_option(message_store_name, MessageName),
    pcap_config::current_option(command_store_name, StoreName),
    Events = term_reader(MessageName),
    Saver = event_saver(StoreName),
    Saver::connect,
    command_analyzer(Events, Saver)::run,
    Saver::disconnect.

:- end_object.
```

Объект, представляющий конфигурацию системы распознавания

```logtalk
:- use_module(library(process)).
:- use_module(library(sgml), [load_structure/3]).
:- use_module(util, [
     json_load/3,
     assert_package/1,
     retract_package/1,
     current_package/1,
     retractall_package/1,
     attach_package_db/1,
     detach_package_db
   ]).
:- use_module(library(option),[option/2,option/3]).
:- use_module(library(pcre),[re_replace/4]).

:- object(pcap_config).
  :- public([current_option/1, current_option/2]).
  current_option(A=B):-
    option(A,B).
  current_option(A,B):-
    option(A,B).

  % ek|fields|json|jsonraw|pdml|ps|psml|tabs|text
  option(output_format,'json').
  option(wireshark_executable, '/usr/bin/tshark').
  option(test_db_name, 'pcap-small.db').
  option(db_name, 'pcap.db').
  option(command_store_name, 'commands.pl').
  option(message_store_name, 'messages.pl').
  option(event_store_name, 'event.pl').
  % option()
:- end_object.
```

Объект, представляющий данных снифинга

```logtalk
:- object(sniffing).
  :- public(current_pack/1).
  :- use_module(lists, [member/2]).
  :- use_module(library(option), [option/2,option/3]).
  :- public(layers/2).
  layers(json(Attrs), Layers):-
    option('_source'(json(Sources)),Attrs),
    option(layers(Layers),Sources).
:- end_object.
```

Источник пакетов трафика - база данных

```logtalk
:- object(db(_FileName_),
     extends(sniffing)).

  :- use_module(util,[
       json_load/3,
       assert_package/1,
       retract_package/1,
       current_package/1,
       retractall_package/1,
       attach_package_db/1,
       detach_package_db/0
     ]).
  :- use_module(library(option), [option/2,option/3]).

  :- public(connect_db/0).
  connect_db:-
    attach_package_db(_FileName_).

  :- public(disconnect_db/0).
  disconnect_db:-
    detach_package_db.
  current_pack(Pkg):-
    current_package(Pkg).

:- end_object.
```

Объект-загрузчик XML-данных снифера в базу данных для быстрого доступа

```logtalk
:- object(pcap(_FileName_,_DisplayFilter_),
     extends(sniffing)).
  :- public(load/0).

  :- use_module(sgml,[load_structure/3]).
  :- use_module(process,[process_create/3]).
  :- meta_predicate(sgml:load_structure(*,*,*)).

  :- use_module(util,[
       json_load/3,
       assert_package/1,
       retract_package/1,
       current_package/1,
       retractall_package/1,
       attach_package_db/1,
       detach_package_db/0
     ]).
  :- use_module(lists,[member/2]).

  load :-
    pcap_config::current_option(wireshark_executable, Exec),
    pcap_config::current_option(output_format, Format),
    get_args(Format, Args),
    format('Exeuting ~w ~w ~n | swilgt ...',[Exec,Args]),
    process_create(Exec, Args, [stdout(pipe(Pipe))]),
    json_load(Pipe, Content, [null(null), true(true), false(false)]),
    format('JSON loaded~nNow converting into db.',[]),
    store(Content).

  get_args(Format, ['-r', _FileName_, '-T', Format]):-
    _DisplayFilter_ = none, !.
  get_args(Format, ['-r', _FileName_, '-Y', _DisplayFilter_, '-T', Format]).

  :- public(store/1).
  store(Packages):-
    pcap_config::current_option(db_name,DBFile),
    attach_package_db(DBFile),
    forall(member(P,Packages), package_store(P)),
    detach_package_db.

  package_store(JSON):-
    ::layers(JSON,Layers),
    assert_package(Layers).

:- end_object.
```

Базовый объект, реализующий сценарии анализа списка событий, передавая результат анализа в ```_Receiver_```, приемник событий-заключений.

```logtalk
:- object(analyzer(_Events_, _Receiver_)).
   :- use_module(library(pcre),[re_replace/4]).
   :- use_module(library(crypto),[hex_bytes/2]).
   :- use_module(lists,[append/3,length/2]).
   :- use_module(user,[open_codes_stream/2]).

   :- protected(current_event/2).
   current_event(Ev, PkgN) :-
     _Events_::current_term(event(PkgN, Ev)).
   :- protected(event/1).
   event(Ev):-
     _Receiver_::event(Ev).
   :- protected(event/2).
   event(Ev,Mark):-
     _Receiver_::event(Ev, Mark).

   :- protected(filter_event/2).
   filter_event(_,_).

   :- public(run/0).
   run :-
      ::init,
      forall(
        ( ::current_event(Ev, PkgN),
          ::filter_event(Ev, PkgN) ),
        ::analyze(Ev, PkgN)),
      ::done.

   :- protected(analyze/2).
   % Анализ реализуется в подклассе

   :- protected(init/0).
   init. % Нечего не надо делать по умолчанию.

   :- protected(done/0).
   done. % Нечего не надо делать по умолчанию.

   :- protected(buffer_bytes/2).
   buffer_bytes([], []).
   buffer_bytes([X|T], R) :-
     buffer_bytes(T, R1),
     buffer_bytes(X, BX),
     append(R1, BX, R).
   buffer_bytes(Buf, Bytes) :-
     re_replace(':'/g, '', Buf, Str),
     hex_bytes(Str, Bytes).

   :- protected(buffer_string/2).
   buffer_string(Buf, Str) :-
     buffer_bytes(Buf, Bytes),
     string_chars(Str,Bytes).

   :- protected(open_buffer/2).
   open_buffer(Buf, Stream) :-
     buffer_bytes(Buf, Bytes),
     open_codes_stream(Bytes, Stream).

   :- public(bytes_dump/1).
   bytes_dump(Bytes) :-
     length(Bytes,Len),
     format('Length: 0x~16r (~w)~n',[Len,Len]),
     open_codes_stream(Bytes, Stream),
     dump_lines(Stream, 0),
     close(Stream).

   dump_lines(Stream, N) :-
     read_string(Stream, 16, S16),
     S16\="",!,
     string_length(S16,L16),
     string_codes(S16, C16),
     hex_bytes(Bytes, C16),
     string_codes(Bytes, BB),
     thin(BB,BBS),
     string_codes(BBytes,BBS),
     clean_chars(C16,CC16),
     format(atom(HexAddr),'~16r',[N]),
     string_field(HexAddr, 5, HA5),
     format('~w_|_~w_|_~s_|~n',[HA5, BBytes, CC16]),
     L1 is N + L16,
     dump_lines(Stream, L1).
   dump_lines(_,_).

   thin([C1,C2],[C1,C2]) :- !.
   thin([C1,C2|T], [C1,C2,32|CT]) :-
     thin(T,CT).

   :- protected(clean_chars/2).

   clean_char(C,C) :-
     C >= 32, C < 127,!.
   clean_char(_,183).

   clean_chars([],[]).
   clean_chars([C|T],[CC|R]) :-
     clean_char(C,CC),
     clean_chars(T,R).

   :- public(buffer_dump/1).
   buffer_dump(Buf) :-
     buffer_bytes(Buf, Bytes),
     bytes_dump(Bytes).

   :- public(string_field/3).
   string_field(Str, Width, Res) :-
     string_length(Str, N),
     ( N>=Width -> Res = Str ;
       D is Width - N,
       string_chars(Str,Chars),
       zeroes(Chars, CRes, D),
       string_chars(Res, CRes)
       ).

   :- public(zeroes/3).
   zeroes(Str,Str,0).
   zeroes(Str,Res,N) :- N>0,!,
     N1 is N - 1,
     zeroes(['0'|Str],Res,N1).

:- end_object.
```

Объект - адаптер-фасад, удобно представляющий при помощи своих методов, список полей пакета (option-список).

```logtalk
:- object(packet(_Layers_)).
   :- use_module(library(option), [option/2]).
   :- use_module(lists, [member/2,append/3]).

   :- protected(path/2).
   :- dynamic(path/2).

   update_path(Option, SubPath) :-
     Option=..[Name,_],
     (
      path(Name, Path) ->
        P=Path ;
        P = []
     ),
     (
       P = [SubPath|_] ->
        true ;
        retractall(path(Name, Path)),
        assertz(path(Name, [SubPath|P]))
     ).  % ! Reverse order

   :- public(print_paths/2).
   print_paths(Option, Path) :-
     forall(path(Option, Path), format('~w->~w~n',[Option, Path])).

   :- protected(field/3).
   field([],A,A).
   field([Subpath|T],A,Value):-
     field(T,A,Attrs),  % ! Reverse order
     Segment=..[Subpath, json(Value)],
     option(Segment,Attrs).

   :- public(field/2).
   field(Option, json(Attrs)):-
     field(Option, Attrs),!.
   field(Option, Attrs):-
     Attrs\=json(_),
     option(Option, Attrs),!.

   field(Option, Attrs):-
     Option=..[Name,_],
     path(Name, Path),!,
     field(Path, Attrs, SubTree),
     field(Option, SubTree).

   field(Option, Attrs):-
     Attrs\=json(_),
     Option =.. [Name,_],
     split_ref(Name, Head, _),
     Op1 =.. [Head, json(JSON)],
     option(Op1, Attrs),
     update_path(Option,Head),
     field(Option, JSON),!.

   :- public(field/1).
   field(Option):-
     field(Option, _Layers_).

   :- public(split_ref/3).
   split_ref(Atom,Ref,Refs):-
     atom(Atom),
     sub_atom(Atom,B,1,TL,'.'),
     S is B+1,
     sub_atom(Atom,S,TL,0,Refs),
     sub_atom(Atom,0,B ,_,Ref),!.

   :- public(tcp_addr/2).
   tcp_addr(src, A:P):-
     field('ip.src'(A)),
     field('tcp.srcport'(B)),
     atom_number(B,P).
   tcp_addr(dst, A:P):-
     field('ip.dst'(A)),
     field('tcp.dstport'(B)),
     atom_number(B,P).
   tcp_addr(src-dst,S-D):-
     tcp_addr(src,S),
     tcp_addr(dst,D).

   :- public(ip_addr/2).
   ip_addr(src,S) :-
     field('ip.src'(S)).
   ip_addr(dst,D) :-
     field('ip.dst'(D)).
   ip_addr(src-dst,S-D) :-
     ip_addr(src,S),
     ip_addr(dst,D).

   :- public(time/2).
   time(abs,A):-
     field('frame.time_epoch'(A)).

   :- public(eth_addr/2).
   eth_addr(src,A):-
     field('eth.src_tree'(json(Tree))),
     field('eth.addr'(A),Tree).
   eth_addr(dst,A):-
     field('eth.dst_tree'(json(Tree))),
     field('eth.addr'(A),Tree).

   :- public(number/1).
   number(N):-
     field('frame.number'(A)),!,
     atom_number(A,N).

   :- public(tcp_flag/2).
   tcp_flag(Flag,A):-
     member(Flag,[ns,cwr,ecn,urg,ack,push,reset,syn,fin]),
     field('tcp.flags_tree'(T)),
     atom_concat('tcp.flags.',Flag,F),
     Q =.. [F,A],
     field(Q,T).

   :- public(tcp_flag/1).
   tcp_flag(Flag):-
     tcp_flag(Flag,'1').

   :- public(tcp_fin/0).
   tcp_fin :-
     tcp_flag(fin).

   :- public(tcp_push/0).
   tcp_push :-
     tcp_flag(push).

   :- public(tcp_ack/1).
   tcp_ack(N):-
     tcp_flag(ack),
     field('tcp.ack_raw'(A)),
     atom_number(A,N).

   :- public(tcp_seq/1).
   tcp_seq(N):-
     tcp_flag(syn),
     field('tcp.seq_raw'(A)),
     atom_number(A,N).

   :- public(tcp_len/1).
   tcp_len(N):-
     field('tcp.len'(A)),
     atom_number(A,N).

   :- public(tcp_payload/1).
   tcp_payload(Data):-
     field('tcp.payload'(Data)).

:- end_object.
```
Объект, распознающий TCP-состояния в процессе обмена данными.

```logtalk
:- object(state(_Pkg_)).
   :- public(inc/2).
   inc(A,B):- add(A,1,B).
   :- public(add/3).
   add(A,B,C):-
     C is (A + B) mod 4294967296.

   :- protected(tcpep/2). % участник обменя по TCP
   %                state,     ip:port       syn:ack    receive buffer
   tcpep(Ip:Port, e(none,      Ip:Port,     none:none,  []         )).

   :- public(conn_none/2).
   % TODO: sa(SYNS-ACKS,SYND-ACKD)
   %         Addrs  SRC  DST       sa    receiver buffers
   conn_none(S-D, c(SE,DE)) :-
     tcpep(S,SE),
     tcpep(D,DE).

   ack(A):-
     _Pkg_::tcp_ack(A),
     \+ _Pkg_::tcp_fin.
   conn_ack(A,A1):-
     inc(A,A1),
     _Pkg_::tcp_ack(A1),
     \+ _Pkg_::tcp_fin.
   fin_ack(A):-
     inc(A,A1),
     _Pkg_::tcp_ack(A1),
     _Pkg_::tcp_fin.

   :- protected(tcp_payload/4).
   tcp_payload(SS,SS1, DB,DB1) :-
     (
      _Pkg_::tcp_len(LA), LA>0 ->
        add(SS,LA,SS1),
        _Pkg_::tcp_payload(Data),
        DB1=[Data|DB] ;
        SS1 = SS,
        DB1 = DB
     ).

   :- public(shift/4).

   shift(   % ----> Syn
       tcp,
       c(e(none ,S,  _:SA, SB), e(none,D, DS:DA,DB)),
       c(e(start,S, SS:SA, SB), e(none,D, DS:DA,DB)),
       init
       ) :-
     _Pkg_::tcp_seq(SS),    % Generated
     \+ _Pkg_::tcp_fin,!.

   shift(   % ----> Ack
       tcp,
       c(e(none, S,  _:_,  SB), e(start,D, DS:DA ,DB)),
       c(e(start,S, SS:SA, SB), e(start,D, DS:DA,DB)),
       syn
   ) :-
     _Pkg_::tcp_seq(SS),  % Generated
     conn_ack(DS,SA),!.

   shift(   % ----> Ack
       tcp,
       c(e(start,S, SS :_,  SB), e(start,D, DS:DA, DB)),
       c(e(est  ,S, SS1:SA, SB), e(est  ,D, SA:DA, DB)),
       established
   ) :-
     inc(SS,SS1),
     conn_ack(DS,SA),!.

   % Обмен трафиком (Push)
   shift(   % ----> push
       tcp,
       c(e(est,S,  SS:SA, SB), e(est,D, DS:_,   DB)),
       c(e(est,S, SS1:SA, SB), e(est,D, DS:SS1, [])),
       push(DB1)
    ) :-
     _Pkg_::tcp_push,
     tcp_payload(SS,SS1, DB, DB1),
     ack(SA),!.

   shift(   % ----> ACK
       tcp,
       c(e(est,S,  SS:SA, SB), e(est,D, DS:_, DB)),
       c(e(est,S, SS1:SA, SB), e(est,D, DS:SS1, DB1)),
       ack
       ) :-
     _Pkg_::tcp_ack(GSA),
     GSA=<SA,             % Check and late ack
     \+ _Pkg_::tcp_fin,
     tcp_payload(SS,SS1, DB,DB1).

   shift(   % ----> ACK,Fin
       tcp,
       c(e(est,S, SS:SA, SB), e(est,D, DS:DA, DB)),
       c(e(fw1,S, SS:SA, SB), e(cw, D, DS:DA, DB)),
       fin_start
       ) :-
     _Pkg_::tcp_ack(SA),    % Check
     _Pkg_::tcp_fin,!.

   shift(   % ----> Ack (finalizing)
       tcp,
       c(e(cw, S, SS:SA, SB), e(fw1, D, DS:DA, DB)),
       c(e(cw, S, SS:SA, SB), e(fw1, D, DS:DA, DB)),
       fin_ack
       ) :-
     conn_ack(SA,_),!.

   shift(   % ----> Ack,Fin
       tcp,
       c(e(cw,  S, SS:SA, SB),   e(fw1,    D, DS:DA, DB)),
       c(e(last,S, SS:SA, SB), e(closed, D, DS:DA, DB)),
       fin_ack
       ) :-
     fin_ack(SA),!.

   shift(   % ----> Ack,FIN
       tcp,
       c(e(cw,    S, SS:SA, SB), e(fw2,    D, DS:DA, DB)),
       c(e(last  ,S, SS1:SA, SB), e(closed, D, DS:DA, [])),
       [push(DB1),fin_fin]
       ) :-
     fin_ack(SA),!,
     tcp_payload(SS,SS1, DB,DB1).

   shift(   % ----> ACK
       tcp,
       c(e(closed,S, SS:SA, SB), e(last,   D, DS:DA, DB)),
       c(e(closed,S, SS:SA, SB), e(closed, D, DS:DA, DB)),
       closed
       ) :-
     conn_ack(SA,_),!.

   % reset, сброс соединения
   shift(   % ----> Rst
       tcp,
       c(e(none,  S,    _:_,  SB), e(start,  D, DS:DA, DB)),
       c(e(closed,S, none:SA, SB), e(closed, D, DS:DA, DB)),
       reset
   ) :-
     _Pkg_::tcp_flag(reset),
     conn_ack(DS,SA),!.

   % icmp
   shift(   % ---->
       ip,
       c(e(none, S, _,    SB),e(none,D, _,    DB)),
       c(e(none, S, none, SB),e(none,D, none, DB)),
       icmp
       ) :-
     _Pkg_::field('ip.proto'('1')).
:- end_object.
```

Анализатор соединения, представляемых противоположными потоками, логически связанными друг с другом

```logtalk
:- object(connections(_Pkg_)).
   :- public(shift/3).
   shift([], List, Event):-
     _Pkg_::number(N),
     state(_Pkg_)::conn_none(_-_,InitialState),!,
     shift(InitialState,NewState,Event),!,
     (
       final_state(Event) ->
         List = [];
         List = [s(NewState,[N])]
     ).

   % прямое направление
   shift(State, NextState, e(Event,S-D)):-
     State=c(e(_,S,_,_),e(_,D,_,_)),
     _Pkg_::tcp_addr(src-dst,S-D),!,
     state(_Pkg_)::shift(tcp, State, NextState, Event),
     !.

   % обратное направление
   shift(
     c(SE, DE), c(SE1,DE1), backward(e(Event,S-D))
   ):-
     c(SE, DE)=c(e(_,S,_,_),e(_,D,_,_)),
     _Pkg_::tcp_addr(src-dst,D-S),!,
     state(_Pkg_)::shift(
      tcp,
      c(DE, SE), c(DE1,SE1), Event
   ),!.

   % ICMP/IP
   shift(State, NextState, e(Event,S-D)):-
     State=c(e(none,S:_,_,_),e(none,D:_,_,_)),
     _Pkg_::ip_addr(src-dst,S-D),!,
     % format('~nicmp ~w~n',[S-D]),
     state(_Pkg_)::shift(ip, State, NextState, Event),
     !.
   shift([s(State,_)|T],NT, [removed(closed(S-D)),Event]) :-
     State=c(e(closed,S:_,_,_),e(closed,D:_,_,_)),!,
     shift(T,NT,Event).
   shift([s(State,_)|T],T, Event) :-
     shift(State, _, Event),
     final_state(Event),!.
   shift([s(State,P)|T],[s(NextState,N)|T], Event) :-
     shift(State, NextState, Event),
     _Pkg_::number(N),
     N>P, % допущение-проверка (assert)
     !.
   shift([X|T],[X|R],Event) :-
     shift(T, R, Event),!.

   :- protected(final_state/1).

   % что такое "конец" соедиения
   final_state(closed).
   final_state(reset).
   final_state(icmp).
   final_state(removed).

:- end_object.
```
Вспомогательный объект, предназначен для анализа TCP-фреймов, их организация в потоки

```logtalk
:- object(frame_analyzer(_Sniffing_,_Receiver_)).

   :- protected(current_pack/1).
   current_pack(packet(Layers)) :-
     _Sniffing_::current_pack(json(Layers)).

   :- protected(init/1).

   init([]).

   :- protected(state/1).
   :- dynamic(state/1).
   :- public(run/1).

   run(LastState) :-
     init(State),
     assertz(state(State)),
     proceed(LastState).

   :- use_module(user,[gtrace/0]).
   :- use_module(lists,[member/2]).
   :- protected(proceed/1).

   proceed(LastState):-
     nl,
     forall(
       (
         current_pack(Pkg),
         Pkg::number(PkgN),
         format('PKG ~w~n',[PkgN])
       ),
       (
         state(State),
         connections(Pkg)::shift(State,NextState,Event),
         format('Event ~w ~n',[Event]),!,
         _Receiver_::event(Event,PkgN),!,
         retract(state(State)),!,
         assertz(state(NextState))
       )
     ),
     state(LastState),
     forall(member(X,LastState),
      _Receiver_::event(removed(state(X)),none)).

:- end_object.
```

Категория, используемая для поименования физических адресов в человекочитаемые имена.  Она используется для отображения информации об участниках обмена.

```logtalk
:- category(ih_dns).
   :- public([dns/3,dns/2]).

   dns('00:50:c2:00:5a:b3', cntrl, '192.168.1.10').
   dns('00:15:17:6a:98:1f', xepr,  '192.168.1.1').
   dns('00:00:ad:0e:93:12', esigl, '192.168.1.12').
   dns('00:00:ad:0e:91:12', esigh, '192.168.1.11').
   dns('00:00:ad:0b:75:12', tnkr0, '192.168.1.14').
   dns('00:00:ad:0e:e3:12', ehall, '192.168.1.13').
   dns('00:30:64:05:af:8c', spjet, '192.168.1.16'). % No data
   dns('00:00:ad:0d:86:12', abrig, '192.168.1.107').
   dns('00:00:ad:0b:74:12', ptjet, '192.168.1.15'). % No DATA

   dns(IP:Port, Name:Port) :-
     dns(_, Name, IP).

:- end_category.
```

Анализатор пересылаемых сообщений, объект, унаследованый от ```analyzer/2```.

```logtalk
:- object(message_analyzer(_Events_, _Receiver_),
     extends(analyzer(_Events_,_Receiver_)),
     imports(ih_dns)).

   :- dynamic(conn/2).
   :- private(conn/2).
   :- protected(current_conn/2).
   current_conn(Client,Server):-
     conn(Client,Server).

   init.

   to_107(_ , '192.168.1.107' : _).
   to_10(_ , '192.168.1.10' : N) :- N\=50000.

   from_1('192.168.1.1' : _, _).

   % Сервер XEPR соединяется только с CTRL
   % CTRL в BR, длина команды или 8 байт или 16

   to_11(_, '192.168.1.11':_).
   to_12(_, '192.168.1.12':_).
   to_ESIG(S,D) :-
     to_11(S,D).
   to_ESIG(S,D) :-
     to_12(S,D).

   to_TNK(_,'192.168.1.14':_).
   to_HALL(_,'192.168.1.13':_).
   to_SPJET(_,'192.168.1.16':_).
   to_PTJET(_,'192.168.1.16':_).

   ctrl_cmds('192.168.1.10':_,IP:_) :-
     IP\='192.168.1.1'.

   xchg(S,D):-
     from_1(S,D).
   xchg(S,D):-
     ctrl_cmds(S,D).

   filter(S,D) :-
     xchg(S,D).

   analyze(e(push(Data), S-D), N) :-
     filter(S,D),
     !,
     ::dns(S,SN),
     ::dns(D,DN),
     format('~n~w REQ: from ~w to ~w~n', [N, SN,DN]),
     ::buffer_bytes(Data, Bytes),
     ::event(e(command,S-D,Bytes), N).

   analyze(backward(e(push(Data), S-D)), N) :-
     filter(S,D),
     !,
     ::dns(S,SN),
     ::dns(D,DN),
     format('~n~w ANS: to ~w from ~w~n', [N, SN,DN]),
     ::buffer_bytes(Data, Bytes),
     ::event(e(answer,S-D,Bytes), N).

   analyze(e(icmp, _-_), N) :- !,
     format('~n~w ICMP~n',[N]).

   analyze(_,_):-!.
   analyze(Event, PkgN) :-
     format('SKIP: ~w ~w ~n',[PkgN,Event]).

:- end_object.
```

Объект-анализатор команд, основан на ```analyzer/2```

```logtalk
:- object(command_analyzer(_Events_, _Receiver_),
      extends(analyzer(_Events_, _Receiver_)),
      imports(ih_dns)).

    init:-
      ^^init,
      clear.

    % Для каждой команды из XEPR (USER PC)
    % может быть несколько ответов из
    % блока CTRL.
    % Таким образом, мы предполагаем, что ответы принимаются
    % до тех пор, пока новая команда не будет отправлена в CTRL.

    % filter_event(_,N) :-
    %   N<5000.

    done:-
      command(C),
      analyze_command(C),
      clear.

    :- private(command/1).
    :- private(answer/1).
    :- dynamic([command/1,answer/1]).

    :- protected(clear/0).

    clear :-
      retractall(command(_)),
      retractall(answer(_)).

    analyze(e(command, Client-Server, Data), N):-!,
      analyze_command(e(Client-Server, Data,N)).
    analyze(e(answer, Client-Server, Data), N):-!,
      analyze_answer(e(Client-Server,Data,N)).

    analyze(Ev,_):-
      format('Got a ~w',[Ev]),
      fail.

    :- dynamic([command/2,answer/2]).

    analyze_command(e(C-S,Data,N)) :-
      ::dns(C,CN:_),
      ::dns(S,SN),
      interp_command(CN-SN,Data,Interp),
      format('~w CMD: ~w ~w ~n',[N, CN-SN, Interp]),!.

    analyze_answer(e(C-S,Data,N)) :-
      ::dns(C,CN:_),
      ::dns(S,SN),
      interp_answer(CN-SN,Data,Interp),!,
      format('~w ANS: ~w::~n',[N, CN-SN]),!,
      dump_interp(Interp), nl.

    dump_interp([]).
    dump_interp([a(L,m(C,Cmd,S,B))|T]) :- !,
      ::bytes_dump(B),
      dump_interp(T).
    dump_interp(answers(L)) :- !,
      dump_interp(L).
    dump_interp(A) :- format('~w',[A]).

    simple_string(Codes, Str) :-
      string_codes(S,Codes), split_string(S,"","\x0",[Str]).

    no_trail_0([],[]).
    no_trail_0([X|T],R):-
      no_trail_0(T,T1),
      (
        T1 = [], X=0 -> R=[] ;
        T1 = [] -> R = [X] ;
        R = [X|T1]
      ).

    % таблица команд и оценка данных параметра
    %       Name,                       Len, Skip_after_name0
    cmd_len('gTempCtrl.TemperatureMon',   7, 0).
    cmd_len('cwBridge.CalibState',   10, 2).
    cmd_len('cwBridge.LevelState',   10, 2).
    cmd_len('cwBridge.OpModeMon',   10, 3).
    cmd_len('cwBridge.SignalPhaseMon',   22,0).
    cmd_len('cwBridge.TuneState',   105,3).
    cmd_len('cwBridge.PowerAtten',  88,2).
    cmd_len('cwBridge.BridgeCalib',  80,1).
    cmd_len('cwBridge.CWBridgeType',   11,0).
    cmd_len('cwBridge.CWUpgrade',   11,2).
    cmd_len('cwBridge.EmbBridge',   11,2).
    cmd_len('cwBridge.EMBType',   11,4).
    cmd_len('cwBridge.EWSMBC',   11,5).
    cmd_len('cwBridge.ExtStabPresent',   14,0).
    cmd_len('cwBridge.FineAFC',   15,5).
    cmd_len('cwBridge.LockPhase',  15,3).
    cmd_len('cwBridge.SignalPhase',  15,1).
    cmd_len('ffLock.DialogPanelLock',  15,0).
    cmd_len('ffLock.Attenuator',  15,4).
    cmd_len('ffLock.FFNbAccum',  15,5).
    cmd_len('ffLock.FFOffset',  15,6).
    cmd_len('ffLock.FFProp',  24,0).
    cmd_len('ffLock.GmAcqTime',  10,5).
    cmd_len('ffLock.GmDeadTime',  10,4).
    cmd_len('ffLock.GmDefField',  11,4).
    cmd_len('ffLock.GmEchoAllowed',  11,1).
    cmd_len('ffLock.GmEchoPlsLen',  11,2).
    cmd_len('ffLock.GmFFTLen',  11,6).
    cmd_len('ffLock.GmGain',  16,8).
    cmd_len('ffLock.GmLockResol',  16,3).
    cmd_len('ffLock.GmLowerLimit',  16,2).
    cmd_len('ffLock.GmModulation',  16,2).
    cmd_len('ffLock.GmNbAccum',  16,5).
    cmd_len('ffLock.GmPlsDist',  16,5).
    cmd_len('ffLock.GmPlsLen',  16,6).
    cmd_len('ffLock.GmRecDelay',  16,4).
    cmd_len('ffLock.GmSTLevel',  16,5).
    cmd_len('sctCalib.StepModFreq',   104, 0).
    cmd_len('sctCalib.StartMod',  104, 0).

    get_cmd([],[]):-!.
    get_cmd([0|_],[]):-!.
    get_cmd([46|T],[46|R]):-
      get_cmd(T,R).
    get_cmd([C|T],[C|R]):-
      C>65, C=<122,
      get_cmd(T,R).

    msg_acqhidden(Data, Res, Cmd) :-
      cmd_len(Cmd, CmdLen, SkipLen),
      string_codes(Cmd, Codes),
      append(Codes, Rest, Data),!,
      Rest = [0|Rest1],
      length(Skip,SkipLen),
      length(Res,CmdLen),
      append(Skip,Rest2, Rest1),
      append(Res,_, Rest2).

    msg_acqhidden(Data, Res, Cmd) :-
      length(CMD,22),
      append(CMD, Res, Data),
      % debugger::trace,
      get_cmd(CMD,CmdC), !,
      string_codes(Cmd,CmdC).


    msg_acqhidden(D,D,'unrecognized_cmd').

    msg([0,0,3,32], [0,0,3, C, 0x41, 0x63, 0x71, 0x48, 0x69, 0x64, 0x64, 0x65, 0x6e, 0x2e|R],
        Res, pkgno(C), command(Cmd)) :-
      % debugger::trace,
      %format(atom(PC), '[0x~16r]', [C]),
      %string_codes(PC, PCC),
      msg_acqhidden(R, Res, Cmd).
      %append(PCC,D, Res).


    msgs(Data, [a([C,D,E,F],m(CodeNum, Cmd, MsgData, Q1))|T]):-
      Data = [0,0,A,B|_],
      Len is A*256+B,
      length(Q, Len),
      append(Q, Rest, Data),!,
      msgs(Rest,T),
      Q=[C,D,E,F|QT],
      msg([C,D,E,F],QT,RT,CodeNum, Cmd),
      no_trail_0(RT,Q1),
      ::clean_chars(Q1,CQ),
      string_codes(MsgData,CQ).

    msgs([], []):-!.
    msgs(Data, [r(Data)]).

    cmd(cntrl - (_ : 10000), [00, 00, 00, 00, 0x40, 00, 00, 0x28], cyclope(sad)):-!.
    cmd(cntrl - (_ : 10000), [00, 00, 00, 00, 0x40, 00, 00, 0x29], cyclope(happy)):-!.
    cmd(cntrl - (_ : _),     [00, 00, 00, 00, 0x40|T], cyclope(T)):-length(T,3),!.
    cmd(cntrl - (Unit : _), [00, 00, 00, Reg |T], reg(Unit, Reg, T)):-length(T,4),!.

    cmd(xepr  - (cntrl : _), [0,0,0,Value|T], init(Value,T)).
    cmd(xepr  - (cntrl : _), [0x41, 0x63, 0x71, 0x48, 0x69, 0x64, 0x64, 0x65, 0x6e, 0x2e|T], 'AcqHidden'(S1)) :- length(T,14), !, simple_string(T,S1).

    cmd(xepr  - ( cntrl : _), [03, 0x00, 0x00, 0x64, 0x03, 0xe8, 0x78, 0x75,
      0x73, 0x65, 0x72, 0x00, 0x68, 0x74, 0x74,
      0x70, 0x5f, 0x70, 0x72, 0x6f, 0x78, 0x00,
      0x43, 0x38, 0x32, 0x36, 0x35, 0x52, 0x46, 0x4c, 0x00], 'xuser.http_prox.C8265RFL').

    cmd(cntrl - ( abrig : 10106 ), [0,0,0,0], get_graph).

    :- use_module(lists, [append/3]).
    :- dynamic(prev_answer/1).

    ans(cntrl - ( abrig : 10106 ) , _, skip_bridge_graph) :-! .

    ans(xepr - (cntrl : P), Data, Event) :-
      retract(prev_answer(PrevData)), !,
      append(PrevData, Data, AllData),
      ans(xepr - (cntrl : P ), AllData, Event).

    ans(xepr - (cntrl : _), Data, 'answers'(Msgs)) :-
      Data = [0,0, A,B|_],
      length(Data,L), L >= A*256+B, !,
      msgs(Data, Msgs).

    ans(xepr - (cntrl : _), Data, data_part) :-
        Data = [0,0, A,B|_],
        length(Data,L), L < A*256+B,!,
        asserta(prev_answer(Data)).

    interp_command(C-S, Data, Cmd):-
      cmd(C-S, Data, Cmd),!.
    interp_command(C-S, Data, Data):-
      format('CMD: ~w DUMP~n',[C-S]),
      ::bytes_dump(Data),!.

    interp_answer(C-S, Data, Ans):-
      ans(C-S, Data, Ans),!.
    interp_answer(C-S, Data, none):-
      format('ANS: ~w DUMP~n',[C-S]),
      ::bytes_dump(Data),!.

:- end_object.
```
Объект, позволяет сохранить в файл результаты распознавания, называемые в проекте событиями

```logtalk
:- object(event_saver(_FileName_)).
   :- public(event/2).
   :- public(event/1).

   :- protected(stream/1).
   :- dynamic(stream/1).
   :- public(connect/0).

   connect :-
     open(_FileName_, write, Stream),!,
     assertz(stream(Stream)),
     !.
   :- public(disconnect/0).
   disconnect :-
     retract(stream(Stream)),!,
     close(Stream),!.

   event([],_):-!.
   event([Event|T],N) :- !,
     event(Event,N),
     event(T,N).
   event(Event, N) :-
     stream(Stream), !,
     format(Stream, '~k.~n', [event(N,Event)]),!.

   % Событие без пометки - это событие с пометкой none. ;-)
   event(Event):-
     event(Event, none).
:- end_object.
```

Объект, позволяющий считывать термы из файла. <!-- Делает обратную операцию в сравнении с ```event_saver/1```. -->

```logtalk
:- object(term_reader(_FileName_)).

   :- use_module(user, [setup_call_cleanup/3]).

   :- public(current_term/1).
   current_term(T):-
     setup_call_cleanup(open(_FileName_, read, Stream, []),
       current_term(Stream, T),
       close(Stream)).

   current_term(Stream, T) :-
     repeat,
     (read_term(Stream, T0, []),
      T0 \= end_of_file
      -> T=T0 ; !, fail).

:- end_object.
```
