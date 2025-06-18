# Modulo para funciones del proyecto
- dynamic barrel/3.

% Predicado principal para inicializar tres barriles
initialBarrels(Barrels, Capacities, Beers) :-
    % Verificar que las listas tengan exactamente tres elementos
    Barrels = [ID1, ID2, ID3],
    Capacities = [Cap1, Cap2, Cap3],
    Beers = [Beer1, Beer2, Beer3],
    % Validar que los identificadores sean "A", "B", "C"
    Barrels = ["A", "B", "C"],
    % Validar cada barril y obtener valores corregidos
    (   validate_barrel(ID1, Cap1, Beer1, ValidCap1, ValidBeer1),
        validate_barrel(ID2, Cap2, Beer2, ValidCap2, ValidBeer2),
        validate_barrel(ID3, Cap3, Beer3, ValidCap3, ValidBeer3)
    ->  % Limpiar hechos previos
        retractall(barrel(_, _, _)),
        % Agregar hechos validados a la base de conocimientos
        assertz(barrel(ID1, ValidCap1, ValidBeer1)),
        assertz(barrel(ID2, ValidCap2, ValidBeer2)),
        assertz(barrel(ID3, ValidCap3, ValidBeer3))
    ;   % Si alguna validación falla, devolver false
        false
    ).

% Validar un barril, devuelve valores corregidos o falla
validate_barrel(ID, Cap, Beer, ValidCap, ValidBeer) :-
    % Validar identificador
    member(ID, ["A", "B", "C"]),
    % Validar que Cap y Beer sean números
    number(Cap), number(Beer),
    % Validar que Cap sea no negativo
    Cap >= 0,
    % Validar que Beer sea no negativo
    Beer >= 0,
    % Si Beer > Cap, establecer Beer igual a Cap
    (   Beer =< Cap
    ->  ValidCap = Cap, ValidBeer = Beer
    ;   ValidCap = Cap, ValidBeer = Cap
    ).