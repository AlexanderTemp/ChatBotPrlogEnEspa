%    leer_oracion/1
%    leer_oracion_pc/1 -- preserve case of letters
%    escribir_oracion/1
%    espacio/0
%    leer_cadena/1
%    escribir_cadena/1

leer_oracion([PirmeraPalabra|RestoOracion]) :-
  leerOracion([PirmeraPalabra|RestoOracion]).

leerOracion([PirmeraPalabra|RestoOracion]) :-
  get0(Char),
  leerPalabra(Char,PirmeraPalabra,NextChar),
  leerRestoOracion(PirmeraPalabra,NextChar,RestoOracion).

   %--- ancillaries to leer_oracion -------------------------
   leerRestoOracion(Word,_,[]) :-
     palabraFinDeOracion(Word),!.
   leerRestoOracion(_,Char,[NextWord|RestoOracion]) :-
     leerPalabra(Char,NextWord,NextChar),
     leerRestoOracion(NextWord,NextChar,RestoOracion).

   leerPalabra(Char,Word,NextChar) :-
     charEspecial(Char),!,name(Word,[Char]),get0(NextChar).
   leerPalabra(Char,Word,NextChar) :-
     charCompuesto(Char,NewChar),
     !,
     get0(TempNextChar),
     restoPalabra(TempNextChar,RestWord,NextChar),
     name(Word,[NewChar|RestWord]).
   leerPalabra(_,Word,NextChar) :-
     get0(TempChar),
     leerPalabra(TempChar,Word,NextChar).

   restoPalabra(Char,[NewChar|RestWord],NextChar) :-
     charCompuesto(Char,NewChar),
     !,
     get0(TempNextChar),
     restoPalabra(TempNextChar,RestWord,NextChar).
     restoPalabra(Char,[],Char).

   charEspecial(44).  /* , */
   charEspecial(59).  /* ; */
   charEspecial(58).  /* : */
   charEspecial(63).  /* ? */
   charEspecial(33).  /* ! */
   charEspecial(46).  /* . */

   charCompuesto(Char,Char) :- Char>96,Char<123.

   charCompuesto(Char,L) :- Char>64,Char<91,L is Char+32.

   charCompuesto(Char,L) :- Char>64,Char<91,L is Char+32.
   charCompuesto(Char,Char) :- Char>47,Char<58.
   charCompuesto(39,39).  /* ' */
   charCompuesto(45,45).  /* - */
   charCompuesto(95,95).  /* _ */

   palabraFinDeOracion('.').
   palabraFinDeOracion('!').
   palabraFinDeOracion('?').


leer_oracion_pc([PirmeraPalabra|RestoOracion]) :-
  leerOracion_pc([PirmeraPalabra|RestoOracion]).

leerOracion_pc([PirmeraPalabra|RestoOracion]) :-
  get0(Char),
  leerPalabra_pc(Char,PirmeraPalabra,NextChar),
  leerRestoOracion_pc(PirmeraPalabra,NextChar,RestoOracion).

   leerRestoOracion_pc(Word,_,[]) :-
     palabraFinDeOracion(Word),!.
   leerRestoOracion_pc(_,Char,[NextWord|RestoOracion]) :-
     leerPalabra_pc(Char,NextWord,NextChar),
     leerRestoOracion_pc(NextWord,NextChar,RestoOracion).

   leerPalabra_pc(Char,Word,NextChar) :-
     charEspecial(Char),!,name(Word,[Char]),get0(NextChar).
   leerPalabra_pc(Char,Word,NextChar) :-
     charCompuesto_pc(Char,NewChar),
     !,
     get0(TempNextChar),
     restoPalabra_pc(TempNextChar,RestWord,NextChar),
     name(Word,[NewChar|RestWord]).
   leerPalabra_pc(_,Word,NextChar) :-
     get0(TempChar),
     leerPalabra_pc(TempChar,Word,NextChar).

   restoPalabra_pc(Char,[NewChar|RestWord],NextChar) :-
     charCompuesto_pc(Char,NewChar),
     !,
     get0(TempNextChar),
     restoPalabra_pc(TempNextChar,RestWord,NextChar).
     restoPalabra_pc(Char,[],Char).

   charCompuesto_pc(Char,Char) :- Char>96,Char<123.

   charCompuesto_pc(Char,Char) :- Char>64,Char<91.

   charCompuesto_pc(Char,L) :- Char>64,Char<91,L is Char+32.
   charCompuesto_pc(Char,Char) :- Char>47,Char<58.
   charCompuesto_pc(39,39).  /* ' */
   charCompuesto_pc(45,45).  /* - */
   charCompuesto_pc(95,95).  /* _ */

escribir_oracion([F|R]) :-
   write(F),
   escribir_resto_de_oracion(R).
  
   escribir_resto_de_oracion([F|R]) :-
     write(' '),
     write(F),
     escribir_resto_de_oracion(R).
   escribir_resto_de_oracion([]).


espacio :- write(' ').


leer_cadena(S) :-
   get0(C),
   (
      C == -1,  S = [], !, fail;
      C == 10,  S = [], ! ;
      C == 32, !, leer_cadena(S);
      !, leer_cadena(C,S)
   ).

leer_cadena(C,[C|Cs]) :-
   get0(D),
   (
      D == -1,  Cs = [], !, fail;
      D == 10,  Cs = [], ! ;
      D == 32,  Cs = [], ! ;
      !, leer_cadena(D,Cs)
   ).


escribir_cadena([]) :- !.
escribir_cadena([C|Cs]) :- put(C), escribir_cadena(Cs).