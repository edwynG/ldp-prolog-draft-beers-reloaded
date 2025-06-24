:- dynamic barrel/3.

% Inicializa los barriles con capacidades y volúmenes
initialBarrels(["A", "B", "C"], [CA, CB, CC], [BA0, BB0, BC0]) :-
    maplist(valid_number, [CA, CB, CC, BA0, BB0, BC0]),
    retractall(barrel(_, _, _)),
    cascade_fix(CA, CB, CC, BA0, BB0, BC0, FinalA, FinalB, FinalC),
    FinalA =< CA, FinalB =< CB, FinalC =< CC,
    assertz(barrel("A", CA, FinalA)),
    assertz(barrel("B", CB, FinalB)),
    assertz(barrel("C", CC, FinalC)).

cascade_fix(CA, CB, CC, A, B, C, FA, FB, FC) :-
    fix_once(CA, CB, CC, A, B, C, NA, NB, NC),
    ( A = NA, B = NB, C = NC ->
        FA = A, FB = B, FC = C
    ; cascade_fix(CA, CB, CC, NA, NB, NC, FA, FB, FC)
    ).

fix_once(CA, CB, CC, A0, B0, C0, A2, B2, C2) :-
    excess(A0, CA, EA, A1),
    excess(C0, CC, EC, C1),
    B1Temp is B0 + EA + EC,
    excess(B1Temp, CB, EB, B1),

    ( A1 =< C1, CA > 0 ->
        free_space(A1, CA, FA),
        Transfer is min(EB, FA),
        A2 is A1 + Transfer,
        C2 = C1
    ; C1 =< A1, CC > 0 ->
        free_space(C1, CC, FC),
        Transfer is min(EB, FC),
        C2 is C1 + Transfer,
        A2 = A1
    ;
        A2 = A1,
        C2 = C1
    ),
    B2 = B1.

excess(Value, Max, Excess, Fixed) :-
    (Value > Max -> Excess is Value - Max, Fixed is Max ; Excess = 0, Fixed = Value).

free_space(Current, Capacity, Free) :-
    Free is Capacity - Current.

valid_number(N) :- integer(N), N >= 0.

iSolution(Barrel, Beer, Goal) :-
    barrel("A", CA, BA),
    barrel("B", CB, BB),
    barrel("C", CC, BC),
    
    % Validar si podemos agregar cerveza al barril indicado
    (Barrel = "A", CA > 0 -> NewBA is BA + Beer, NewBB = BB, NewBC = BC ;
     Barrel = "C", CC > 0 -> NewBC is BC + Beer, NewBA = BA, NewBB = BB ;
     fail),

    % Simulación sin tocar la base real
    cascade_fix(CA, CB, CC, NewBA, NewBB, NewBC, FA, FB, FC),
    
    % Evaluar el resultado sin modificar los hechos
    (
        FA >= Goal ;
        FB >= Goal ;
        FC >= Goal
    ),
    !.

% Predicado para agregar cerveza a un barril
addBeer(Barrel, Beer, Transfer) :-
    string(Barrel),
    number(Beer),
    var(Transfer),
    Beer >= 0,
    not(Barrel = "B"),
    barrel(Barrel, Max, Curr),
    New is Curr + Beer,
    (   New =< Max
    ->  retract(barrel(Barrel, Max, Curr)),
        assertz(barrel(Barrel, Max, New)),
        Transfer = 0
    ;   Overflow is New - Max,
        retract(barrel(Barrel, Max, Curr)),
        assertz(barrel(Barrel, Max, Max)),
        barrel("B", MaxB, CurrB),
        NewB is CurrB + Overflow,
        (   NewB =< MaxB
        ->  retract(barrel("B", MaxB, CurrB)),
            assertz(barrel("B", MaxB, NewB))
        ;   OverflowB is NewB - MaxB,
            retract(barrel("B", MaxB, CurrB)),
            assertz(barrel("B", MaxB, MaxB)),
            barrel("A", MaxA, CurrA),
            barrel("C", MaxC, CurrC),
            (   CurrA =< CurrC
            ->  MinBarrel = "A", MinMax = MaxA, MinCurr = CurrA
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
    (barrel(_, _, Beer), Beer >= Goal
    -> Solutions0 = [(0, "N/A")]
    ;  Solutions0 = []
    ),

    barrel("A", CapA, CurrA),
    barrel("B", CapB, CurrB),
    barrel("C", CapC, CurrC),

    % 1. Desde A: llenar A directamente
    NeedA is Goal - CurrA,
    (CapA > 0, NeedA > 0, CurrA + NeedA =< CapA ->
        Solutions1 = [(NeedA, "A")|Solutions0]
    ;   Solutions1 = Solutions0
    ),

    % 2. Desde A: llenar B (A desborda a B)
    OverflowA is CapA - CurrA,
    NeedB is Goal - CurrB,
    (CapA > 0, CapB > CurrB, NeedB > 0, CurrB + NeedB =< CapB ->
        R is NeedB + OverflowA,
        Solutions2 = [(R, "A")|Solutions1]
    ;   Solutions2 = Solutions1
    ),

    % 3. Desde A: llenar C vía B
    OverflowB is CapB - CurrB,
    (CapA > 0, CapB > CurrB, CapC > CurrC,
     CurrC =< CapA,
     NeedC is Goal - CurrC,
     NeedC > 0, CurrC + NeedC =< CapC ->
        R2 is NeedC + OverflowA + OverflowB,
        Solutions3 = [(R2, "A")|Solutions2]
    ;   Solutions3 = Solutions2
    ),

    % 4. Desde C: llenar C directamente
    NeedC2 is Goal - CurrC,
    (CapC > 0, NeedC2 > 0, CurrC + NeedC2 =< CapC ->
        Solutions4 = [(NeedC2, "C")|Solutions3]
    ;   Solutions4 = Solutions3
    ),

    % 5. Desde C: llenar B
    barrel("B", CapB2, CurrB2),
    NeedB2 is Goal - CurrB2,
    OverflowC is CapC - CurrC,
    (CapC > 0, CapB2 > CurrB2, NeedB2 > 0, CurrB2 + NeedB2 =< CapB2 ->
        R3 is NeedB2 + OverflowC,
        Solutions5 = [(R3, "C")|Solutions4]
    ;   Solutions5 = Solutions4
    ),

    % 6. Desde C: llenar A vía B
    barrel("A", CapA2, CurrA2),
    (CapC > 0, CapB > CurrB, CapA2 > CurrA2,
     CurrA2 =< CapC,
     NeedA2 is Goal - CurrA2,
     NeedA2 > 0, CurrA2 + NeedA2 =< CapA2 ->
        R4 is NeedA2 + OverflowC + OverflowB,
        Solutions6 = [(R4, "C")|Solutions5]
    ;   Solutions6 = Solutions5
    ),

    Result = Solutions6.
findSolution(Goal, SolutionType, Result) :-
    number(Goal),
    string(SolutionType),
    var(Result),
    Goal >= 0,
    (SolutionType = "best"; SolutionType = "all"),

    % Verificar si ya se cumple sin agregar cerveza
    ( forall(barrel(_, _, Beer), Beer >= Goal)
    -> Result = (0, "N/A"), !

    % Verificar si existe al menos un barril A o C con capacidad > 0
    ; ( barrel("A", CapA, CurrA), CapA > CurrA
      ; barrel("C", CapC, CurrC), CapC > CurrC
      )
    -> all_solutions(Goal, List),
       ( SolutionType = "best" ->
            ( List \= [] ->
                sort(List, Sorted), Sorted = [Best|_], Result = Best, !
            ; fail )
       ; SolutionType = "all" ->
            ( List \= [] ->
                sort(List, Sorted), member(Result, Sorted)
            ; fail )
       )
    ; fail  % No hay forma de agregar cerveza a ningún barril
    ).
