# Modulo para predicados extras
/* Tienes que indicar los modulos a exportar. "[predicado/n]" donde n es el numero de argumentos */
:- module(utils, [greet/0]). 

greet :- 
    write('Hola Mundo'), nl.