# Modulo para funciones del proyecto
:- module(predicates, [initialBarrels/3, validate_barrel/5, addBeer/3, findSolution/3]). 

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

barrel("A", 10, 3). 
barrel("B", 7, 0).   
barrel("C", 4, 0).

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
    OverflowC is CapC - CurrC,
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
    (   barrel(_, _, Beer), Beer >= Goal
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



