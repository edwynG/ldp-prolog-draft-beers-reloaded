% Modulo para funciones del proyecto
:- module(predicates, [initialBarrels/3, iSolution/3, addBeer/3, findSolution/3, barrel/3]). 

:- dynamic barrel/3.

% Predicado principal para inicializar barriles
initialBarrels(Barrels, Capacities, Beers) :-
    Barrels = ["A", "B", "C"], % Unificar
    Capacities = [Cap1, Cap2, Cap3],
    Beers = [Beer1, Beer2, Beer3],
    retractall(barrel(_, _, _)),
    assertz(barrel("A", Cap1, 0)),
    assertz(barrel("B", Cap2, 0)),
    assertz(barrel("C", Cap3, 0)),
    validate_and_update_barrels(["A", "B", "C"], [Cap1, Cap2, Cap3], [Beer1, Beer2, Beer3]).

% Predicado auxiliar para validar y actualizar barriles con desbordes
validate_and_update_barrels(["A", "B", "C"], [Cap1, Cap2, Cap3], [Beer1, Beer2, Beer3]) :-
    number(Cap1), number(Cap2), number(Cap3),
    number(Beer1), number(Beer2), number(Beer3),
    Cap1 >= 0, Cap2 >= 0, Cap3 >= 0,
    Beer1 >= 0, Beer2 >= 0, Beer3 >= 0,
    process_barrel("A", Cap1, Beer1),
    process_barrel("B", Cap2, Beer2),
    process_barrel("C", Cap3, Beer3),
    !. % Corte para garantizar determinismo

% Procesar un barril y manejar desbordes
process_barrel(ID, Cap, Beer) :-
    barrel(ID, Cap, CurrBeer),
    NewBeer is CurrBeer + Beer,
    (   NewBeer =< Cap
    ->  retract(barrel(ID, Cap, CurrBeer)),
        assertz(barrel(ID, Cap, NewBeer))
    ;   Excess is NewBeer - Cap,
        retract(barrel(ID, Cap, CurrBeer)),
        assertz(barrel(ID, Cap, Cap)),
        transfer_excess(ID, Excess, 0)
    ).

% Transferir exceso según las conexiones A <-> B <-> C
transfer_excess(_, _, Depth) :- Depth >= 10, !. % Límite para evitar bucle infinito
transfer_excess("A", Excess, Depth) :-
    barrel("B", CapB, CurrB),
    NewB is CurrB + Excess,
    NewDepth is Depth + 1,
    (   NewB =< CapB
    ->  retract(barrel("B", CapB, CurrB)),
        assertz(barrel("B", CapB, NewB))
    ;   ExcessB is NewB - CapB,
        retract(barrel("B", CapB, CurrB)),
        assertz(barrel("B", CapB, CapB)),
        transfer_excess("B", ExcessB, NewDepth)
    ).

transfer_excess("C", Excess, Depth) :-
    barrel("B", CapB, CurrB),
    NewB is CurrB + Excess,
    NewDepth is Depth + 1,
    (   NewB =< CapB
    ->  retract(barrel("B", CapB, CurrB)),
        assertz(barrel("B", CapB, NewB))
    ;   ExcessB is NewB - CapB,
        retract(barrel("B", CapB, CurrB)),
        assertz(barrel("B", CapB, CapB)),
        transfer_excess("B", ExcessB, NewDepth)
    ).

transfer_excess("B", Excess, Depth) :-
    barrel("A", CapA, CurrA),
    barrel("C", CapC, CurrC),
    NewDepth is Depth + 1,
    (   CurrA =< CurrC
    ->  Target = "A", TargetCap = CapA, TargetCurr = CurrA
    ;   Target = "C", TargetCap = CapC, TargetCurr = CurrC
    ),
    NewTarget is TargetCurr + Excess,
    (   NewTarget =< TargetCap
    ->  retract(barrel(Target, TargetCap, TargetCurr)),
        assertz(barrel(Target, TargetCap, NewTarget))
    ;   ExcessTarget is NewTarget - TargetCap,
        retract(barrel(Target, TargetCap, TargetCurr)),
        assertz(barrel(Target, TargetCap, TargetCap)),
        transfer_excess(Target, ExcessTarget, NewDepth)
    ).

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