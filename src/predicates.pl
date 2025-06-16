# Modulo para funciones del proyecto

% Declaramos barrel/3 como dinámico
:- dynamic barrel/3.

% Este es el predicado principal para inicializar tres barriles
initialBarrels(Barrels, Capacities, Beers) :-
    % Verificamos que las listas tengan exactamente tres elementos
    Barrels = [ID1, ID2, ID3],
    Capacities = [Cap1, Cap2, Cap3],
    Beers = [Beer1, Beer2, Beer3],
    % Limpiar hechos previos de barrel/3
    retractall(barrel(_, _, _)),
    % Validamos y almacenamos cada barril
    validate_barrel(ID1, Cap1, Beer1, ValidCap1, ValidBeer1),
    validate_barrel(ID2, Cap2, Beer2, ValidCap2, ValidBeer2),
    validate_barrel(ID3, Cap3, Beer3, ValidCap3, ValidBeer3),
    % Agregamos los hechos validados a la base de conocimientos
    assertz(barrel(ID1, ValidCap1, ValidBeer1)),
    assertz(barrel(ID2, ValidCap2, ValidBeer2)),
    assertz(barrel(ID3, ValidCap3, ValidBeer3)).

% Este es el predicado auxiliar para validar un barril
validate_barrel(_ID, Cap, Beer, ValidCap, ValidBeer) :-
    % Validar que Cap > 0, Beer >= 0 y Beer <= Cap
    (   Cap > 0, Beer >= 0, Beer =< Cap
    ->  ValidCap = Cap, ValidBeer = Beer
    ;   Cap =< 0
    ->  ValidCap = 0, ValidBeer = 0
    ;   Beer > Cap
    ->  ValidCap = Cap, ValidBeer = Cap
    ;   Beer < 0
    ->  ValidCap = Cap, ValidBeer = 0
    ;   ValidCap = Cap, ValidBeer = 0
    ).