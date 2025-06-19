# Modulo para funciones del proyecto
:- dynamic barrel/3.

% Predicado principal para inicializar barriles
initialBarrels(Barrels, Capacities, Beers) :-
    Barrels = [ID1, ID2, ID3],
    Capacities = [Cap1, Cap2, Cap3],
    Beers = [Beer1, Beer2, Beer3],
    Barrels = ["A", "B", "C"],
    (   validate_barrel(ID1, Cap1, Beer1, ValidCap1, ValidBeer1),
        validate_barrel(ID2, Cap2, Beer2, ValidCap2, ValidBeer2),
        validate_barrel(ID3, Cap3, Beer3, ValidCap3, ValidBeer3)
    ->  retractall(barrel(_, _, _)),
        assertz(barrel(ID1, ValidCap1, ValidBeer1)),
        assertz(barrel(ID2, ValidCap2, ValidBeer2)),
        assertz(barrel(ID3, ValidCap3, ValidBeer3))
    ;   false
    ).

validate_barrel(ID, Cap, Beer, ValidCap, ValidBeer) :-
    member(ID, ["A", "B", "C"]),
    number(Cap), number(Beer),
    Cap >= 0,
    Beer >= 0,
    (Beer =< Cap -> ValidBeer = Beer, ValidCap = Cap ; ValidBeer = Cap, ValidCap = Cap).

% Predicado iSolution/3
iSolution(Barrel, Beer, Goal) :-
    % Validaciones
    (Barrel == "A" ; Barrel == "C"),
    number(Beer), Beer >= 0,
    number(Goal), Goal >= 0,
    
    % Obtenemos el estado inicial
    barrel("A", CapA, A0),
    barrel("B", CapB, B0),
    barrel("C", CapC, C0),
    
    % Simulamos agregar cerveza con propagación de desbordes
    add_with_propagation(Barrel, Beer, [A0, B0, C0], [CapA, CapB, CapC], [A1, B1, C1], 0),
    
    % Verificamos si algún barril cumple con la meta
    (   A1 >= Goal
    ;   B1 >= Goal
    ;   C1 >= Goal
    ),
    !. 

% Reglas de propagación
add_with_propagation(_, _, State, _, State, Depth) :- Depth >= 10, !.
add_with_propagation(_, 0, State, _, State, _) :- !.

add_with_propagation("A", Amount, [A0, B0, C0], [CapA, CapB, CapC], FinalState, Depth) :-
    Total is A0 + Amount,
    (Total =< CapA
        -> FinalState = [Total, B0, C0]
        ;  A1 is CapA,
           Excess is Total - CapA,
           NewDepth is Depth + 1,
           add_with_propagation("B", Excess, [A1, B0, C0], [CapA, CapB, CapC], FinalState, NewDepth)
    ).

add_with_propagation("C", Amount, [A0, B0, C0], [CapA, CapB, CapC], FinalState, Depth) :-
    Total is C0 + Amount,
    (Total =< CapC
        -> FinalState = [A0, B0, Total]
        ;  C1 is CapC,
           Excess is Total - CapC,
           NewDepth is Depth + 1,
           add_with_propagation("B", Excess, [A0, B0, C1], [CapA, CapB, CapC], FinalState, NewDepth)
    ).

add_with_propagation("B", Amount, [A0, B0, C0], [CapA, CapB, CapC], FinalState, Depth) :-
    Total is B0 + Amount,
    (Total =< CapB
        -> FinalState = [A0, Total, C0]
        ;  B1 is CapB,
           Excess is Total - CapB,
           (A0 =< C0 -> Target = "A" ; Target = "C"),
           NewDepth is Depth + 1,
           add_with_propagation(Target, Excess, [A0, B1, C0], [CapA, CapB, CapC], FinalState, NewDepth)
    ).