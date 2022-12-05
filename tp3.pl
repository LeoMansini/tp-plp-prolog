%------------------Predicados predefinidos:------------------%

%fliplength(?Longitud, ?Lista)
fliplength(N, L) :- length(L, N).

%matriz(?Matriz, ?Filas, ?Columnas)
matriz(M, F, C) :- length(M, F), maplist(fliplength(C), M).

%dif1(+N1, ?N2)
dif1(N1, N2) :- N2 is N1 + 1.
dif1(N1, N2) :- N2 is N1 - 1.

%adyacente(+F1, +C1, ?F2, ?C2)
adyacente(F1,C1,F1,C2) :- dif1(C1,C2).
adyacente(F1,C1,F2,C1) :- dif1(F1,F2).
adyacente(F1,C1,F2,C2) :- dif1(C1,C2), dif1(F1,F2).

%enRango(+Matriz, +Fila, +Columna)
enRango([Fila|Filas], F, C) :- F > 0, C > 0, length([Fila|Filas], FMax), F =< FMax, length(Fila, CMax), C =< CMax.

%adyacenteEnRango(+Tablero, +F1, +C1, ?F2, ?C2)
adyacenteEnRango(T,F1,C1,F2,C2) :- adyacente(F1,C1,F2,C2), enRango(T,F2,C2).

%------------------Predicados a definir:------------------%

%setEqual(+S, +T)
% Se fija si dos arreglos son iguales como sets, es decir, tienen los mismos elementos.
% Precondicion: S y T no tienen repetidos.
setEqual(S, T) :- sort(S, Ordenada), sort(T, Ordenada).

%iesimo(+I,?L,?X)
% Verdadero si el iesimo valor de L es X.
iesimo(1,[X|_],X).
iesimo(I,[_|Xs],X):- N is I - 1, iesimo(N,Xs,X).

%indexarLista(?Lista, ?Indice, ?Contenido)
% Funciona como iesimo pero es reversible en todos sus parámetros.
% Sirve para instanciar listas con un elemento en una dada posición, si I y X se pasan instanciados y L solo parcialmente instanciado o no instanciado.
indexarLista(L, I, X) :- length(L, Long), between(1, Long, I), iesimo(I,L,X).

%contenido(+?Tablero, ?Fila, ?Columna, ?Contenido)
% Verdadero si el tablero en la posición (F, C) tiene el contenido Con.
contenido(T, F, C, Con) :- indexarLista(T, F, Fil), indexarLista(Fil, C, Con).

%disponible(+?Tablero, ?Fila, ?Columna)
% Verdadero si la posición (F, C) esta libre para ubicar un barco o parte de él, es decir, no esta instanciada ni la posición ni sus adyacentes.
disponible(T,F,C) :- contenido(T,F,C,Y), var(Y), not((adyacenteEnRango(T,F,C,F1,C1), contenido(T,F1, C1, X), nonvar(X))).

%siguienteConDireccion(+Direccion,+Fila,+Columna,-FilaSig,-ColSig)
% Verdadero si avanzando en el sentido Direccion desde (Fila, Columna) se llega a (FilaSig, ColSig).
siguienteConDireccion(vertical, F, C, F1, C) :- F1 is F-1.
siguienteConDireccion(horizontal, F, C, F, C1) :- C1 is C+1.

%puedoColocar(+CantPiezas, ?Direccion, +?Tablero, ?Fila, ?Columna)
% Verdadero si desde la posición (F, C) en la dirección D, entra un barco de CantPiezas de tamaño.
puedoColocar(1, _, T, F, C) :- disponible(T,F,C).
puedoColocar(I, D, T, F, C) :- I > 1, disponible(T,F,C), siguienteConDireccion(D,F,C,F1,C1), I1 is I-1, puedoColocar(I1, D, T, F1, C1).

%ubicarBarcoEn(+CantPiezas, ?Direccion, +?Tablero, ?Fila, ?Columna)
% Instancia en T las posiciones que corresponden a un barco de CantPiezas de tamaño, ubicado en la posición (F, C) en la dirección D.
ubicarBarcoEn(1, _, T, F, C) :- contenido(T, F, C, o).
ubicarBarcoEn(I, D, T, F, C) :- I > 1, contenido(T, F, C, o), siguienteConDireccion(D,F,C,F1,C1), I1 is I-1, ubicarBarcoEn(I1, D, T, F1, C1).

%ubicarBarcos(+Barcos, +?Tablero)
% Instancia en T un barco para cada tamaño en el arreglo Barcos, en cualquier posición.
ubicarBarcos([], _).
ubicarBarcos([Tam|Barcos],T) :- puedoColocar(Tam,Dir,T,F,C), ubicarBarcoEn(Tam,Dir,T,F,C), ubicarBarcos(Barcos, T).

%nextPos(+?Tablero, +F, +C, -F1, -C1)
% Instancia en (F1, C1) la siguiente posición, recorriendo el tablero de manera antilexicográfica comenzado por (M, N).
nextPos(T, 1, C, F1, C1) :- C > 1, C1 is (C-1), length(T,F1).
nextPos(_, F, C, F1, C) :- F1 is F-1, F1 > 0.

%aguaSiVar(?Posicion)
% Si X no esta instanciada, la instancia con el átomo ~, que simboliza el agua.
aguaSiVar(X) :- nonvar(X).
aguaSiVar(X) :- var(X), X = ~ .

%completarConAguaRec(+?Tablero, +F, +C)
% Predicado auxiliar para completarConAgua. Recorre el tablero de manera antilexicográfica, y en cada posición libre la instancia con agua.
completarConAguaRec(T, 1, 1) :- contenido(T,1,1,X), aguaSiVar(X).
completarConAguaRec(T, F, C) :- contenido(T,F,C,X), aguaSiVar(X), nextPos(T, F, C, F1, C1), completarConAguaRec(T, F1, C1).

%completarConAgua(+?Tablero)
% Cada posición libre del tablero es instanciada con agua.
completarConAgua(T) :- matriz(T, N, M), completarConAguaRec(T, N, M).

%golpearAux(+Tablero, +NumFila, +NumColumna, +PosFila, +PosColumna, +?NuevoTab)
% Predicado auxiliar para golpear. Recorre el tablero de manera antilexicográfica, y es verdadero si, 
% en cada posición distinta de (X, Y) T y T2 coinciden, y en la posición (X, Y) T2 tiene agua.
golpearAux(_, 1, 1, 1, 1, T2) :- contenido(T2, 1, 1, ~).
golpearAux(T, X, Y, 1, 1, T2) :- [X, Y] \= [1, 1], contenido(T, 1, 1, A), contenido(T2, 1, 1, A).
golpearAux(T, X, Y, X, Y, T2) :- [X, Y] \= [1, 1], contenido(T2, X, Y, ~), nextPos(T, X, Y, X2, Y2), golpearAux(T, X, Y, X2, Y2, T2).
golpearAux(T, X, Y, X1, Y1, T2) :- [X, Y] \= [X1, Y1], [X1, Y1] \= [1, 1], contenido(T, X1, Y1, A), contenido(T2, X1, Y1, A), nextPos(T, X1, Y1, X2, Y2), golpearAux(T, X, Y, X2, Y2, T2).

%golpear(+Tablero, +NumFila, +NumColumna, -NuevoTab)
% Verdadero si T y T2 son iguales excepto por la posición (X, Y), donde T2 tiene agua.
golpear(T, X, Y, T2) :- matriz(T, N, M), matriz(T2, N, M), golpearAux(T, X, Y, N, M, T2).

%hundioBarco(+Tablero, +Fila, +Columna)
% Verdadero si todos los adyacentes a la posición (F, C) son agua, es decir, si el barco que estaba en (X, Y) fue totalmente hundido.
% Adyacente en rango mira en diagonal también, pero esto es correcto porque los barcos no pueden estar a una posición en diagonal.
hundioBarco(T, F, C) :- not((adyacenteEnRango(T,F,C,F1,C1), contenido(T, F1, C1, o))).

% Completar instanciación soportada y justificar.
%atacar(+Tablero, +Fila, +Columna, -Resultado, -NuevoTab)
% T2 es el resultado de golpear a T en (F, C), y resultado indica el efecto del ataque:
% - Si es agua, hay agua en (F, C) en T.
% - Si es hundido, hay barco en (F, C) en T, y está aislado (es la última parte del barco).
% - Si es tocado, hay barco en (F, C) en T, y queda parte del barco por atacar.
atacar(T, F, C, agua, T2) :- contenido(T, F, C, ~), T2 = T.
atacar(T, F, C, hundido, T2) :- contenido(T, F, C, o), golpear(T, F, C, T2), hundioBarco(T, F, C).
atacar(T, F, C, tocado, T2) :- contenido(T, F, C, o), golpear(T, F, C, T2), not(hundioBarco(T, F, C)).

%% EJERCICIO 8

% Tablero no es reversible porque contenido de un tablero no inicializado tiene infinitas soluciones. Además, se queda generando infinitos tableros de una única fila.

% Fila y columna son reversibles, porque en cada caso de atacar se instancian con contenido.

% Puede pasarse un tablero instanciado, filas y columnas no instanciadas y Resultado instanciado, y instanciará las filas y columnas con las posiciones que, si se atacaran
% tendrían el resultado instanciado.

% Puede pasarse el tablero inicial y el final instanciados, y se instanciarán la fila y la columna con el resultado.

% Son reversibles todos los parámetros excepto el tablero inicial.

%------------------Tests:------------------%

test(1) :- matriz(M,2,3), adyacenteEnRango(M,2,2,2,3).
test(2) :- matriz(M,2,3), setof((F,C), adyacenteEnRango(M,1,1,F,C), [ (1, 2), (2, 1), (2, 2)]).
%+T +F +C +Con
testContenido(1) :- not(contenido([[1, 2], [3, 4]], 2, 2, 5)). 
%+T -F -C +Con
testContenido(2) :- setof([N,M],contenido([[1, 4], [3, 4]], N, M, 4),S),setEqual(S, [[1, 2], [2, 2]]).
%+?T +F +C +Con
testContenido(3) :- contenido([[1, C], [3, 4]], 1, 2, 4),C is 4.
%+T +F +C -Con
testContenido(4) :- contenido([[1, 2], [3, 4]], 2, 2, X),X is 4.
%+?T -F -C -ConsetEqual(S,[[1,1],[2,1]])
testContenido(5) :- setof([N,M,T,C], (between(1, 7, N), between(1,7,M), matriz(T,5,5),between(1,5,C), contenido(T,N,M,C)), S), length(S, 125).
%+?T +F +C
testDisponible(1) :- not(disponible([[_,_,_],[_,o,_],[_,o,_]], 2, 2)).
%+?T +F +C
testDisponible(2) :- disponible([[_,_,_],[_,_,_],[_,_,_]], 2, 2).
%+?T -F -C
testDisponible(3) :- setof([Fil,Col],disponible([[_,_,_],[_,_,_],[_,_,_],[o,o,o]], Fil, Col),S),setEqual(S,[[1,1],[1,2],[1,3],[2,1],[2,2],[2,3]]).
%+Cant +Dir +?T -F -C
testPuedoColocar(1) :- setof([F,C],puedoColocar(2,vertical,[[o,_,_],[o,_,_],[o,_,_],[o,o,o]],F,C),[[2,3]]).
%+Cant +Dir +?T -F -C
testPuedoColocar(2) :- setof([F,C],puedoColocar(3,horizontal,[[_,_,_],[_,_,_],[_,_,_],[o,o,o]],F,C),S),setEqual(S,[[1,1],[2,1]]).
%+Cant -Dir +?T -F -C
testPuedoColocar(3) :- setof([F,C,Dir],puedoColocar(3,Dir,[[_,_,_],[_,_,_],[_,_,_],[_,_,_]],F,C),S),length(S,10).
%+Barcos +?Tablero
testUbicarBarcos(1) :- matriz(M,3,2), setof(M,ubicarBarcos([2,1],M),S), setEqual(S ,
 [[[o, o], [_, _], [o, _]], 
 [[o, o], [_, _], [_, o]],
 [[o, _], [_, _], [o, o]],
 [[_, o], [_, _], [o, o]]]).
%+?T
testCompletarConAgua(1) :- M = [[o,_,_],[o,_,_],[o,_,_],[o,o,o]], setof(M, completarConAgua(M), [[[o,~,~],[o,~,~],[o,~,~],[o,o,o]]]).
testCompletarConAgua(2) :- M = [[_,o,o],[_,o,o],[_,o,o],[_,o,o]], setof(M, completarConAgua(M), [[[~,o,o],[~,o,o],[~,o,o],[~,o,o]]]).
%+T +F +C -T2
testGolpear(1) :- T = [[o,~,~],[o,~,~],[o,~,~],[o,o,o]], setof(T2, golpear(T, 2, 1, T2), [[[o,~,~],[~,~,~],[o,~,~],[o,o,o]]]).
testGolpear(2) :- T = [[o,~,~],[o,~,~],[o,~,~],[o,o,o]], setof(T2, golpear(T, 3, 3, T2), [[[o,~,~],[o,~,~],[o,~,~],[o,o,o]]]).

%+T +F +C -Res -T2
testAtacar(1) :- T = [[o,~,~],[o,~,~],[~,~,~],[o,o,o]], setof([T2, Res], atacar(T, 2, 1, Res, T2), [[[[o,~,~],[~,~,~],[~,~,~],[o,o,o]], tocado]]).
testAtacar(2) :- T = [[o,~,~],[o,~,~],[~,~,~],[o,o,o]], setof([T2, Res], atacar(T, 1, 1, Res, T2), [[[[~,~,~],[o,~,~],[~,~,~],[o,o,o]], tocado]]).
testAtacar(3) :- T = [[o,~,~],[~,~,~],[~,~,~],[o,o,o]], setof([T2, Res], atacar(T, 1, 1, Res, T2), [[[[~,~,~],[~,~,~],[~,~,~],[o,o,o]], hundido]]).
testAtacar(4) :- T = [[o,~,~],[o,~,~],[~,~,~],[o,o,o]], setof([T2, Res], atacar(T, 1, 3, Res, T2), [[[[o,~,~],[o,~,~],[~,~,~],[o,o,o]], agua]]).

tests :- 
    forall(between(1,2,N), test(N)),
    forall(between(1, 5, N), testContenido(N)),
    forall(between(1, 3, N), testDisponible(N)),
    forall(between(1, 3, N), testPuedoColocar(N)),
    testUbicarBarcos(1), 
    forall(between(1, 2, N), testCompletarConAgua(1)),
    forall(between(1, 2, N), testGolpear(N)),
    forall(between(1, 4, N), testAtacar(N)).
