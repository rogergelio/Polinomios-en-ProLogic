%Rogelio Torres
%Alejandro Bermudez
%Lucía Lizardi
%Enrique Orozco
%Ana Cristina Sánchez
%Proyecto#1 Inteligencia artificial

%Proyecto de Programación 1
%Se nos ha entregado un proyecto en NetBeans el cual cuenta con la funcionalidad de realizar diferentes operaciones con polinomios,
% así como la definición de estos. El objetivo es traducir el programa dado en lenguaje Java a Prolog.

/*Método defpolinomio */

/*
Este metodo sería el equivalente al constructor de Java. Lo que hace este método es armar el polinomio dados los parámetros.
*/
%defpolinomio(i,i,o,o)
%defpolinomio
defpolinomio(CoefA,D,Lista,Deg):-
    defpolinomio(CoefA,D,[],Lista,Deg).

defpolinomio(M,I,ListaCeros,Lista,De):-
    I==0,
    combina(ListaCeros,[M],Lista),
    degree(Lista,De),
    !.
defpolinomio(M,I,ListaCeros,Lista,De):-
    I1 is I-1,
    combina([0], ListaCeros,ListaCeros2),
    defpolinomio(M,I1,ListaCeros2,Lista,De).

%Se usa combina
combina([], Lista, Lista) :- !.
    combina([X|Lista1], Lista2, [X|Lista3]) :-
    combina(Lista1, Lista2, Lista3).


/*
Método degreePolinomio
El método "degreePolinomio" sirve, como lo indica su nombre, para declarar un
polinomio. Usa 2 variables, un arreglo de los coeficientes del
polinomio, y "D", que indica el grado (degree en inglés) del
polinomio. El arreglo es una entrada, mientras que "D" es una
salida. Este método es bastante simple. Básicamente, recibe un arreglo
de n dígitos e indica el grado de dicho polinomio. El caso base para
este método es cuando recibe un arreglo vacío. Para esta situación, se
almacena -1 en D. En caso de que se reciba un arreglo de números,
entonces la variable D almacenará el grado del polinomio.
*/
%degreePolinomio(i,o).
degreePolinomio([],D):-D=(-1).
degreePolinomio([H|X],D):-
    degreePolinomio(X,D2),
    D is D2+1.

%Función degree 

/*
Lo que hace el método degree es dar el grado del polinomio a usar. Si este recibe una lista que es vacia, va a decir que el polinomio es de grado 0 y hace un paro para no seguir. Si es una constante, es decir un polinomio con head nada más y es diferente de 0 el grado es 0, de igual manera hace un paro. Después conforme vamos aumentando index va aumentando como un contador y ese es el grado de nuestro polinomio.
*/
%degree(i,o)
degree([],0):-!.
degree([X|_],0):-
    X\==0, !. 
degree([_|Tail],Index):-
    degree(Tail,Index1),
Index is Index1+1.

/*Método plus
El método “plus” tiene la funcionalidad de sumar dos polinomios de cualquier grado. Para esto se necesitan dos polinomios de entrada, representados por arreglos que contienen sus coeficientes.  Para la suma resultante se toman las colas de las listas PolA y PolB y esto se va agregando a la cola resultante. La cabeza de la lista resultante es la suma de las cabezas de PolA y PolB. El método se llama de forma recursiva hasta llegar a uno de tres casos base. El primero, donde ambas listas se encuentran vacías, lleva al programa a un punto de corte, y no se hace nada. El segundo y tercer caso es cuando se acaba alguna de las listas antes de que se le sume el valor de las demás. Para cualquiera de estos, el resultado que se devuelve son los polinomios intactos de la lista que “faltaba” por sumar, y se le agrega a esta lista las cabezas previamente calculadas. Este proceso lleva a que se almacene la respuesta en [RespHead|RespTail]. 
 */

%plus(i,i,o)
%Base de conocimiento, casos base: 
plus([],[],[]). %Si no hay valores -> polinomio "vacío".
plus([],PolA, PolA). %Si la 1era lista de coeficientes del polinomio está vacía
plus(PolB,[], PolB). %Si la 2da lista de coeficientes del polinimio está vacía
%función suma en caso que demos dos polinomios de entrada.
plus([Head1|Tail1],[Head2|Tail2],[RespHead|RespTail]):-
    plus(Tail1,Tail2,RespTail),
    RespHead is Head1+Head2.

/*Método minus:
El método “minus” tiene la funcionalidad de restar dos polinomios. Para esto se necesitan dos polinomios de entrada.  Para la cola de la resta resultante, se hace la diferencia de las colas de las listas PolA y PolB. Al igual que el método plus, la cabeza de la lista resultante Pol3 es la resta de las cabezas de PolA y PolB. Es importante aclarar que se le está restando PolB a PolA. El método se llama de forma recursiva hasta llegar a uno de tres casos base. El primero, donde ambas listas se encuentran vacías, lleva al programa a un punto de corte, y no se hace nada. El segundo caso es cuando el segundo polinomio (PolB) se encuentra vacío. Para este caso, se retorna el resto de PolA y se le agregan las cabezas calculadas. El tercer y último caso sucede cuando el primer polinomio (PolA) está vacío. En este caso, se recorre PolB recursivamente hasta llegar al final, y se multiplica cada valor del arreglo por -1. Posteriormente, todos estos valores y las cabezas calculadas se agregan en una lista. Este proceso lleva a que se almacene la respuesta en [RespHead|RespTail]. 

 */

%minus(i,i,o)
%Base de conocimiento: Casos base que sabemos para la resta:
minus([],[],[]). %Si no hay valores -> polinomio "vacío". .
minus(PolA,[],PolA):- !. %Si el 2do polinomio a restar es el polinomio 0.
minus([],[H1|X1],[H2|X2]):-
    H2 is -(H1),
    minus([],X1,X2).
minus([Head1|PolA],[Head2|PolB],[Head3|Pol3]):-
    Head3 is Head1-Head2,
    minus(PolA,PolB,Pol3).

/*Método times
El método “times” tiene la funcionalidad de multiplicar dos polinomios. Para esto necesitamos dos polinomios de entrada (PolA y PolB). El polA se multiplicará por la cabeza del PolB, que son los coeficientes del polinomio, estas son multiplicaciones por número escalares que se harán por medio de la función auxiliar de multiplicación por escalar, el resultado de esta multiplicación es MultEscalares. El resultado de MultEscalares se irá sumando para el resultado final de los polinomios. Este procedimiento se repetirá para poder multiplicar cada valor de PolA con cada valor de PolB, hasta llegar y retornar el resultado correcto en Res2. 
 */
%times(i,i,o)
%Base de conocimiento: Casos que sabemos para la multiplicación:
times([],_,[]). %Se intenta multiplicar al menos una lista vacía
times(_,[],[]).
times(PolA,[Head2|PolB], Res2) :-
   times(PolA,PolB, Res1),
        escalar(PolA, Head2, MultEscalares),
    plus(MultEscalares, [0|Res1], Res2).

/*Método escalar.
Función que nos permite realizar la multiplicación de polinomios por algún número escalar. Recibe la lista con los coeficientes del polinomio y un número escalar. El método es bastante simple. Funciona de tal forma que almacena el resultado en un tercer polinomio identificado como [Head2|Poli2]. El método se llama a sí mismo recursivamente, almacenado los valores de la multiplicación de la cabeza del polinomio original por el escalar en Head2. Cuando llega al caso base, que es una lista vacía, se juntan todas las nuevas cabezas en Poli2 y se devuelve el resultado. 
*/
%escalar(i,i,o)
escalar([],_Escalar,[]).
escalar([Head1|Poli1],Escalar, [Head2|Poli2]) :-
   Head2 is Head1*Escalar,
   escalar(Poli1,Escalar, Poli2).


/*Método Compose. 
El método “compose” tiene la funcionalidad de hacer la composición de dos polinomios de la forma:  f(g(x)). Se dan de entrada dos polinomios: el polinomio al que se le hará la composición ‘f(x)’ escrito de la forma [Head1|T]  y ‘g(x)', que será llamado PolB. Para realizar esta operación, se compone la cola del polinomio f(x) con el PolB, que le llamamos F1. F1 se multiplica por PolB y el resultado de esto se sumará con la cabeza del polinomio1. 
 */
%comp(i,i,o) 
%Base de conocimiento: Casos que sabemos para la multiplicación:
comp([],_,[0]). %Cuando componemos una lista vacía, es el polinomio 0.
comp([H|_], [],[H]). %Cuando componemos al PolA con una lista vacía, nos regresará el PolA

comp([Head1|T], PolB, F) :-
        comp(T, PolB,F1),
	times(F1, PolB,Res),
	plus([Head1],Res,F).



%derivada

/*
El método derivada lo que hace es recibir primero un polinomio de entrada y tiene una salida. Después se usa como apoyo el método degreePolinomio para calcular el grado del polinomio de entrada, posteriormente a este se le pone a la otra lista de salida el grado 1. La variable coef, en el deriv de 4 argumentos es el coeficiente que se va a imprimir. Conforme va avanzando el método se va sumando a coef en 1 y al grado se le va restando uno para que vaya recorriendo todo el arreglo del polinomio. Posteriormente va a ir multiplicando el coeficiente por el grado menos 1 tal como se hace una derivada de un polinomio, coeficiente por grado menos 1
*/

%Cláusula de 2 argumentos (1 entrada y una salida)
%deriv(i,o)
deriv([X1|Poli1],[X3|Poli3]):-
    degreePolinomio(Poli1,AuxDeg),
    deriv(Poli1,AuxDeg,1,[X3|Poli3]),!.

%Cláusula de 4 argumentos (3 entradas y una salida)
%deriv(i,i,i,o)
%Caso base
deriv([X],_,Coef,[Res]):-
	Res is X*Coef,!.

deriv([X1|Poli1],Deg,Coef,[X3|Poli3]):-
        Deg2 is Deg-1,
        Coef2 is Coef+1,
        deriv(Poli1,Deg2,Coef2,Poli3),
	Coef=\=0,X3 is X1*Coef,!.
deriv(Lista1,_,0,Lista1):-!.

/*Método evaluar:
El método evaluar recibe un arreglo de coeficientes del polinomio y el valor en el que se desea evaluar (“V”) el polinomio como entradas. Como salida, recibe únicamente la variable “R”. El método calcula el grado del polinomio utilizando el método “polinomio” explicado arriba. Posteriormente, llama a un nuevo método “evaluarPoli”, que recibe en la variable “D” al grado del polinomio y en C un contador inicializado en 0 para saber el grado del polinomio. Dentro de “evaluarPoli” se calcula “Aux”, qué es el valor del término de grado “C”. Posteriormente, se llama de nuevo a “evaluarPoli”, que retornará 0 como caso base para una lista vacía, y que llevará la cuenta de la suma de los términos. Cuando los retorna todos, almacena la suma en la variable “R” y la devuelve, dando el resultado de la evaluación del polinomio. 
 */
evaluarPoli([],_,_,_,Res):-Res is 0.
evaluar([H|X],V,R):-
    degreePolinomio([H|X],D),
    evaluarPoli([H|X],D,V,0,R).
evaluarPoli([H|X],D,V,C,R):-
    NC is C+1,
    Aux is H*(V**C),
    evaluarPoli(X,D,V,NC,R2),
    B=R2+Aux,
    R is B.

%Aquí van métodos auxiliares
minMáx(X,Y,Z):-
    X>Y, Z is X; Z is Y.
unElementoEnLista([_|[]]):-
    write(_);!.
imprimirNeg(H,Z):-
    write(" "),
    write(H),
    write("*x^"),
    write(Z).
imprimirPos(H,Z):-
    write(" + "),
    write(H),
    write("*x^"),
    write(Z).


%tostring

/*
El método toString lo que va a hacer es armar el polinomio para imprimir, este puede ser de cualquier grado. Inicialmente lo que se tiene es una entrada y una salida, num es el parámetro de entrada y string el parámetro de salida. Lo primero que se hace es checar el que num sea mayor que 0 para poder hacer el toString, en caso de que sea menor que cero la salida se vuelve num. y se pone un menos para que sea un número negativo. Después se la da una otra vez un toString de 2 parámetros y se hace un toString con 4 parámetros.
Posteriormente va a ir con una lista y va a ir concatenando en salida el toString con todo armado.

Adicionalmente, tenemos un método signos que nos hará el cambio de signos para la impresión. 
*/


signos(Num, String) :-
  Num >= 0,
  string_concat("+", Num, String),!.
signos(Num, String) :-
  Num < 0,
  String is Num,
  %% string_concat("-", Num, String),
  !.
%toString(i,o)
toString(Lista, Salida) :-
    toString(Lista,0, "", Salida).
%toString(i,i,i,o)
toString([],_,Str,Salida) :-
    atom_chars(Str,Char_str),
    Char_str = [First|_],
    First=='+',
    string_length(Str, Leng),
    L is Leng-1,
    sub_atom(Str,1,L,_,Salida),
    !.
toString([],_,Str,Salida) :-
    atom_chars(Str,Char_str),
    Char_str = [First|_],
    First=='-',
    Salida = Str,
    !.
toString([X|Lista1],Index,String,Salida):-
    Index==0,
    signos(X,String1),
    string_concat(String1, String, Salida1),
    Index1 is Index+1,
    toString(Lista1, Index1, Salida1, Salida),
    !.
toString([X|Lista1],Index,String,Salida) :-
  Index==1,
  signos(X,String1),
  string_concat(String1, "x", String2),
  string_concat(String2, String, String3),
  Index1 is Index+1,
  toString(Lista1, Index1, String3, Salida),
   !.
toString([X|Lista1],Index,String,Salida) :-
  Index>=2,
  signos(X,String1),
  string_concat(String1, "x^", String2),
  string_concat(String2, Index, String3),
  string_concat(String3, String, String4),
  Index1 is Index+1,
  toString(Lista1, Index1, String4, Salida).


%pruebas

zero(R):-
   defpolinomio(0,0,R,_DegZ).

p(C):-
    defpolinomio(4,3,R1,_Deg1),
    defpolinomio(3,2,R2,_Deg2),
    defpolinomio(1,0,R3,_Deg3),
    defpolinomio(2,1,R4,_Deg4),
    plus(R1,R2,A),
    plus(R3,R4,B),
    plus(A,B,C).

q(Resp):-
    defpolinomio(3,2,R1,_Degq1),
    defpolinomio(5,0,R2,_Degq2),
    plus(R1,R2,Resp).

r(R):-
    q(Resp),
    p(Resp2),
    plus(Resp,Resp2,R).

s(R):-
    p(Res),
    q(Res2),
    times(Res,Res2,R).

t(R):-
    p(Res),
    q(Res2),
    comp(Res,Res2,R).

%def main
main:-
    write("zero(x)= "), zero(R),toString(R,RespZero), write(RespZero),nl,
    write("p(x)= "),p(X),toString(X,ResX),write(ResX),nl,
    write("q(x)= "),q(X1),toString(X1,ResX1), write(ResX1),nl,
    write("p(x)+q(x)= "),r(X2),toString(X2,ResX2), write(ResX2),nl,
    write("p(x)*q(x)= "),s(X3),toString(X3,ResX3),write(ResX3),nl,
    write("p(q(x))= "),t(X4), toString(X4,ResX4), write(ResX4),nl,
    write("0-p(X)= "),minus(R,X,Resp),toString(Resp,RespRes), write(RespRes),nl,
    write("p(3)= "),evaluar(X,3,Res),write(Res),nl,
    write("p'(x)= "),deriv(X,ResX5),toString(ResX5,RespResX5),write(RespResX5),nl,
    write("p''(x)= "), deriv(ResX5,ResX6), toString(ResX6,ResX7),write(ResX7).


