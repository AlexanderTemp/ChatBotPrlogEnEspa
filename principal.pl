:- consult('funcion.pl').
elemento_aleatorio(L, E) :-
  length(L, Len),
  Ran is random(Len),
  nth0(Ran, L, E).

listarlo([_],[]).
listarlo([X|Xs],[X|Ys]) :-
  listarlo(Xs,Ys).

iniciar :-
  write('Estoy listo, empezemos a dialogar!'), nl,
  repeat,
    write('> '), leer_oracion(Entrada),
    listarlo(Entrada,EntradaCadena),
    responder(EntradaCadena, CadenaResultante),
    escribir_oracion(CadenaResultante), nl,
    EntradaCadena == [quit].

responder(EntradaDePalabraLista, ListaPalabrasRespuesta) :-
   intercambiar_persona(EntradaDePalabraLista, ListaPalabrasCambiadas),
   formular_respuesta(ListaPalabrasCambiadas, ListaPalabrasRespuesta), !.

yo_tu(mio,tuyo).
yo_tu(yo,tu).
yo_tu(mio,tuyo).
yo_tu(soy,eres).
yo_tu([nosotros, somos], [tu, eres]).

yo_tu(que,[lo, que]).
palabra_cambiada(X,Y) :- yo_tu(X,Y).
palabra_cambiada(X,Y) :- yo_tu(Y,X).
palabra_cambiada(W,W).
intercambiar_persona([], []).
intercambiar_persona([X|Xs], [Y|Ys]) :-
   palabra_cambiada(X,Y),
   !, intercambiar_persona(Xs,Ys).


%base de hechos ontología

respuesta( [_,property(W,kind_of,comida),_],
          [ [estas,hablando,sobre,W,porque,tienes,hambre,'?'],
            [cuando,fue,la,ultima,vez,que,comiste,W,'?'] ]).
respuesta( [_,property(W,kind_of,windows),_],
          [ [deberias,cambiar,W,por,unix,'.'],
            [tal,vez,linux,es,una,mejor,opcion,'.'] ]).

%base de hechos PATRONES

respuesta( [_,pantalla,azul,_],
  [ [por,que,sigues,usando,windows,'?'],
    [formatea,tu,computadora,particiona,e,instala,linux,'.'] ]).

respuesta( [_,linux,_],
  [ [a,muchas,personas,se,les,dificulta,usar,linux,'.'] ]).


respuesta([_,tengo,X],
         [[por,qué,consideras,que,tienes,X,'.']]).

respuesta([_,enfermedad,_],
         [[las,enfermedades,son,muy,peligrosas,deberías,ir,con,un,medico,'.']]).

respuesta([_,informatica_],[por,que,no,te,postulas,a,la,umsa,'.']).




respuesta( [si,_],
  [ [por,qué,'?'] ]).
respuesta( [no,_],
  [ [por,qué,no,'?'] ]).
respuesta( [_,te,gusta,X],
  [ [como,te,puede,gustar,X,'?'],
    [es,raro,que,te,guste,X,'?'] ]).
respuesta( [_,quiero,X],
          [ [por,que,quieres,X,'?'],
            [no,puedes,X,'.'],
            [es,peligroso,X,'?'] ]).

respuesta( [X],
          [ [X,'?'] ]).

%Formulación respuesta Patrones


formular_respuesta(Entrada,Response) :-
   respuesta(PatronDeEntrada, PatronesDeSalida),
   emparejar(PatronDeEntrada, Entrada),
   elemento_aleatorio(PatronesDeSalida, ResponsePattern),
   flatten(ResponsePattern, Response).

emparejar([], []).
emparejar([Xs], IWs) :-
   var(Xs),
   !,
   Xs = IWs.
emparejar([Xs,W|PWs], IWs) :-
   var(Xs),
   !,
   fill_var(Xs, W, IWs, IWsLeft),
   emparejar([W|PWs], IWsLeft).
emparejar([W|PWs], [W|IWs]) :-
   !,
   emparejar(PWs, IWs).

emparejar([], []).
emparejar([Xs], IWs) :-
   var(Xs),
   !,
   Xs = IWs.
emparejar([Xs,W|PWs], IWs) :-
   var(Xs),
   !,
   fill_var(Xs, W, IWs, IWsLeft),
   emparejar([W|PWs], IWsLeft).
emparejar([property(W,P,V)|PWs], [W|IWs]) :-
   property(W,P,V),
   !,
   emparejar(PWs,IWs).
emparejar([W|PWs], [W|IWs]) :-
   !,
   emparejar(PWs, IWs).

fill_var([], W, [W|IWs], [W|IWs]) :-
   !.
fill_var([X|Xs], W, [X|IWs], IWsLeft) :-
   fill_var(Xs, W, IWs, IWsLeft).

fill_var([], W, [W|IWs], [W|IWs]) :-
   !.
fill_var([], property(W,P,V), [W|IWs], [W|IWs]) :-
   property(W,P,V),
   !.
fill_var([X|Xs], W, [X|IWs], IWsLeft) :-
   fill_var(Xs, W, IWs, IWsLeft).

%Inicios de Ontología

word(pizza, [
  kink_of = comida,
  contains = queso,
  contains = tomate ]).
word(broccoli, [
  kind_of = comida,
  color = verde ]).
word(dona, [
  kind_of = comida,
  contains = azucar ]).
word(salsa_de_spaguetti, [
  kind_of = comida,
  contains = tomate ]).

word(joe, [
  instance_of = persona,
  allergy = tomate ]).
word(jill, [
  instance_of = persona,
  allergy = queso ]).

word(pizza, [
kind_of = comida,
contains = queso,
contains = salsa_de_spaguetti ]).

word(windows, [
  kind_of = sistema_operativo ]).
word(unix, [
  kind_of = sistema_operativo ]).
word(xp, [
  kind_of = windows ]).
word(w2000, [
  kind_of = windows ]).
word(linux, [
  kind_of = unix ]).

transitive(kind_of).
transitive(contains).

property(Word, Property, Value) :-
   word(Word, PropertyList),
   member(Property = Value, PropertyList).
property(Word, Property, Value) :-
   transitive(Property),
   word(Word, PropertyList),
   member(Property = Word2, PropertyList),
   property(Word2, Property, Value).
