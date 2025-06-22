% Modulo para funciones del proyecto
:- module(predicates, [initialBarrels/3, validate_barrel/5, addBeer/3, findSolution/3]). 

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

% Predicado para agregar cerveza a un barril
addBeer(Barrel, Beer, Transfer) :-
    string(Barrel),
    number(Beer),
    var(Transfer),
    Beer >= 0,
    not(Barrel = "B"),
    barrel(Barrel, MaxA, CurrA),
    NewA is CurrA + Beer,
    (   NewA =< MaxA
    ->  retract(barrel(Barrel, MaxA, CurrA)),
        assertz(barrel(Barrel, MaxA, NewA)),
        Transfer = 0
    ;   Overflow is NewA - MaxA,
        retract(barrel(Barrel, MaxA, CurrA)),
        assertz(barrel(Barrel, MaxA, MaxA)),
        barrel("B", MaxB, CurrB),
        NewB is CurrB + Overflow,
        (   NewB =< MaxB
        ->  retract(barrel("B", MaxB, CurrB)),
            assertz(barrel("B", MaxB, NewB))
        ;   OverflowB is NewB - MaxB,
            retract(barrel("B", MaxB, CurrB)),
            assertz(barrel("B", MaxB, MaxB)),
            barrel("A", MaxA2, CurrA2),
            barrel("C", MaxC, CurrC),
            (   CurrA2 =< CurrC
            ->  MinBarrel = "A", MinMax = MaxA2, MinCurr = CurrA2
            ;   MinBarrel = "C", MinMax = MaxC, MinCurr = CurrC
            ),
            NewMin is MinCurr + OverflowB,
            (   NewMin =< MinMax
            ->  retract(barrel(MinBarrel, MinMax, MinCurr)),
                assertz(barrel(MinBarrel, MinMax, NewMin))
            ;   retract(barrel(MinBarrel, MinMax, MinCurr)),
                assertz(barrel(MinBarrel, MinMax, MinMax))
            )
        ),
        Transfer = Overflow
    ).


all_solutions(Goal, Result):-
    Solutions0 = [],
    % 1. Desde A: llenar A
    barrel("A", CapA, CurrA),
    NeedA is Goal - CurrA,
    (NeedA > 0, CurrA + NeedA =< CapA ->
        Solutions1 = [(NeedA, "A")|Solutions0]
    ;   Solutions1 = Solutions0
    ),

    % 2. Desde A: llenar B
    barrel("B", CapB, CurrB),
    OverflowA = CapA - CurrA,
    NeedB is Goal - CurrB,
    
    (NeedB > 0, CurrB + NeedB =< CapB ->
        R is NeedB + OverflowA,
        Solutions2 = [(R, "A")|Solutions1]
    ;   Solutions2 = Solutions1
    ),
    
    % 3. Desde A: llenar C (solo si C < A)
    barrel("C", CapC, CurrC),
    OverflowB is CapB - CurrB,
    (CurrC =< CapA ->
        NeedC is Goal - CurrC,
        (NeedC > 0, CurrC + NeedC =< CapC ->
            R2 is NeedC + OverflowA + OverflowB,
            Solutions3 = [(R2, "A")|Solutions2]
        ;   Solutions3 = Solutions2
        )
    ;   Solutions3 = Solutions2
    ),
        
    % 4. Desde C: llenar C
    barrel("C", CapC2, CurrC2),
    OverflowC is CapC2 - CurrC2,
    NeedC2 is Goal - CurrC2,
    (NeedC2 > 0, CurrC2 + NeedC2 =< CapC2 ->
        Solutions4 = [(NeedC2, "C")|Solutions3]
    ;   Solutions4 = Solutions3
    ),

    % 5. Desde C: llenar B
    barrel("B", CapB2, CurrB2),
    NeedB2 is Goal - CurrB2,
    (NeedB2 > 0, CurrB2 + NeedB2 =< CapB2 ->
        R3 is NeedB2 + OverflowC,
        Solutions5 = [(R3, "C")|Solutions4]
    ;   Solutions5 = Solutions4
    ),

    % 6. Desde C: llenar A (solo si A < C)
    barrel("A", CapA2, CurrA2),
    (CurrA2 =< CapC2 ->
        NeedA2 is Goal - CurrA2,
        (NeedA2 > 0, CurrA2 + NeedA2 =< CapA2 ->
            R4 is NeedA2 + OverflowC + OverflowB,
            Solutions6 = [(R4, "C")|Solutions5]
        ;   Solutions6 = Solutions5
        )
    ;   Solutions6 = Solutions5
    ),
    % Filtrar soluciones válidas (ya filtradas arriba)
    Result = Solutions6.

findSolution(Goal, SolutionType, Result) :-
    number(Goal),
    string(SolutionType),
    var(Result),
    Goal >= 0,
    (SolutionType = "best"; SolutionType="all"),
    (   (forall(barrel(_, _, Beer), Beer >= Goal);
            (
            SolutionType = "best",
            barrel(_, _, Beer), Beer >= Goal
            )
        )
    ->  Result = (0, "N/A"),!
    ; 
        all_solutions(Goal,List),
        ( SolutionType = "best" ->
            ( not(List = []) ->
                sort(List, Sorted), Sorted = [Best|_], Result = Best,!
            ;   fail
            )
        ; SolutionType = "all" ->
            ( not(List = []) ->
                sort(List, Sorted), member(Result, Sorted)
            ;  fail
            )
        )
    ).



