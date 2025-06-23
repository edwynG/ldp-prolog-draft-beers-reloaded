# Prolog draft beers reloaded

En una fiesta, se cuenta con 3 barriles de cerveza de diferentes capacidades. Los barriles están conectados entre
sí mediante tubos que permiten transferir cerveza de uno a otro. Además, cada barril tiene una salida que
permite servir cerveza directamente en vasos.

Su objetivo como especialista en programación funcional es diseñar un programa que permita determinar
cuántos litros de cerveza deben agregarse entre los barriles para servir exactamente n vasos de cerveza desde
cualquiera de las salidas

## Estructura del proyecto
```{html}
prolog-draft-beers-reloaded/
├── docs/               # Enunciado y documentación
├── src/                # Código fuente
│   ├── main.pl         # Punto de entrada del programa
│   └── ...             # Otros módulos
├── tests/              # Archivos de pruebas
│   └── ...             # Casos de prueba
├── Makefile            # Archivo makefile para ejecutar
└── README.md           # Este archivo
```
> [!Note]
> Al culminar el proyecto todos los archivos que tengan el **codigo fuente** seran unidos en un unico archivo, la razón de esta estructura es simplemente para un mejor desarrollo.

## Constructor del proyecto

Para lograr compilar el proyecto, haremos uso de un archivo **makefile** el cual permite la construcción de archivos ejecutables programados en lenguajes compilados. Esta es una herramienta para realizar el build del proyecto. En este caso, solo se abrira el interprete SWI-Prolog con todos los predicados cargados

> [!Warning]
> Si estas en windows, recomiendo usar la consola **Git bash**. Puede llegar a fallar alguno de los comandos que se usaron en el makefile si se usa **poweshell** o la **cmd** de windows.

- Windows
    ```{powershell}
        mingw32-make 
    ```
- Linux
    ```{bash}
        make
    ```

> [!Note]
> **make** es la herramienta que se utiliza para ejecutar archivos **makefile**. En windows este viene junto con la instalación de **C/C++**. Y si estas en linux este viene junto con el entorno **Unix**.

## Documentación
En este proyecto se aborda el problema de la distribución óptima de cerveza entre tres barriles interconectados, cada uno con capacidades y cantidades iniciales distintas. El sistema permite que la cerveza fluya entre los barriles a través de desbordes, siguiendo reglas específicas de transferencia y pérdida de líquido cuando los barriles alcanzan su capacidad máxima. El objetivo es diseñar, utilizando programación logica en prolog, un programa capaz de determinar cuántos litros de cerveza deben agregarse a los barriles para servir exactamente $n$ vasos de cerveza desde cualquiera de las salidas disponibles.

El programa simula el proceso de llenado y desborde de los barriles, considerando tanto las restricciones físicas de capacidad como la dinámica de los desbordes entre los barriles. Para lograrlo, se implementan funciones que validan el estado de los barriles, agregan cerveza controlando el desbordamiento, y buscan la estrategia más eficiente para alcanzar la cantidad deseada de vasos servidos. La solución óptima se determina evaluando diferentes caminos de llenado y seleccionando aquel que minimiza la cantidad de cerveza agregada, garantizando así un uso eficiente de los recursos y un comportamiento realista del sistema.

### 1- Inicializacion de barriles 
```{prolog}
initialBarrels(Barrels, Capacities, Beers) :-
    Barrels = ["A", "B", "C"], % Unificar
    Capacities = [Cap1, Cap2, Cap3],
    Beers = [Beer1, Beer2, Beer3],
    retractall(barrel(_, _, _)),
    assertz(barrel("A", Cap1, 0)),
    assertz(barrel("B", Cap2, 0)),
    assertz(barrel("C", Cap3, 0)),
    validate_and_update_barrels(["A", "B", "C"], [Cap1, Cap2, Cap3], [Beer1, Beer2, Beer3]).

validate_and_update_barrels(["A", "B", "C"], [Cap1, Cap2, Cap3], [Beer1, Beer2, Beer3]) :-
    number(Cap1), number(Cap2), number(Cap3),
    number(Beer1), number(Beer2), number(Beer3),
    Cap1 >= 0, Cap2 >= 0, Cap3 >= 0,
    Beer1 >= 0, Beer2 >= 0, Beer3 >= 0,
    process_barrel("A", Cap1, Beer1),
    process_barrel("B", Cap2, Beer2),
    process_barrel("C", Cap3, Beer3),
    !. % Corte para garantizar determinismo

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
```
El predicado `initialBarrels` inicializa el sistema de barriles, unificando la lista de barriles con ["A", "B", "C"] y asociando a cada uno las capacidades y cantidades iniciales de cerveza proporcionadas en las listas Capacities y Beers. 
El proceso comienza limpiando el estado previo mediante `retractall`, que elimina todos los hechos dinámicos existentes relacionados con barriles en la base de conocimientos. Posteriormente, se crean nuevos hechos dinámicos `barrel` para cada barril, asignándoles su capacidad correspondiente y un volumen inicial de cerveza de cero.
Finalmente, el predicado invoca `validate_and_update_barrels`, un auxiliar que valida las cantidades de cerveza iniciales y las procesa, gestionando posibles desbordes según las reglas de transferencia automática entre barriles, asegurando así una distribución correcta del líquido.

### - Existe solucion 
```{prolog}
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
```
El predicado `iSolution` gestiona la adición de una cantidad de cerveza a un barril específico (que puede ser "A" o "C") y verifica si, tras propagar posibles desbordes, al menos uno de los barriles (A, B o C) alcanza o supera una cantidad objetivo de cerveza.
El proceso inicia validando que el barril sea "A" o "C", y que tanto Beer como Goal sean números no negativos. Luego, consulta el estado inicial de los barriles A, B y C (capacidades y cantidades actuales de cerveza) desde la base de conocimientos mediante el hecho `barrel`.
Posteriormente, invoca el predicado auxiliar `add_with_propagation` para simular la adición de la cerveza al barril especificado, manejando desbordes según las conexiones. Este predicado actualiza las cantidades de cerveza en los barriles, redistribuyendo cualquier exceso al barril conectado (A → B, C → B, B → A o C, según cuál tenga menos cerveza).
Finalmente, el predicado verifica si la cantidad de cerveza en alguno de los barriles (A, B o C) alcanza o supera el objetivo (Goal). Un corte (!) asegura que el predicado sea determinista, deteniendo la búsqueda tras encontrar una solución válida.



### 3 - Añadir cerveza
```{prolog}
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
```
El predicado `addBeer` modela la acción de añadir cerveza a un barril específico, asegurando que no se sobrepase su capacidad máxima. Si al sumar la cantidad de cerveza (Beer) al barril (Barrel) no se excede el límite, simplemente actualiza el contenido del barril y no hay desbordamiento. Si se supera la capacidad, el exceso (desbordamiento) se transfiere primero al barril "B". Si "B" también se llena, el nuevo desbordamiento se transfiere al barril con menos contenido entre "A" y "C". Si este último barril también se llena, el exceso se descarta. Así, la función gestiona de forma encadenada el desbordamiento entre barriles, priorizando siempre llenar primero el barril objetivo, luego "B", y finalmente el que tenga menos cerveza entre "A" y "C

### 4 - Solución
```{prolog}
all_solutions(Goal, Result):-
    (barrel(_, _, Beer), Beer >= Goal
    -> Solutions0 = [(0, "N/A")]
    ; Solutions0 = []
    ),

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
    ( forall(barrel(_, _, Beer), Beer >= Goal)
    ->  Result = (0, "N/A")
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

```
El predicado `findSolution` es el encargado de determinar la cantidad óptima de cerveza que debe agregarse a los barriles para servir exactamente una cantidad objetivo de vasos. Su funcionamiento se basa en dos modos: encontrar la mejor solución (`best`), es decir, la que requiere agregar la menor cantidad de cerveza posible, o listar todas las soluciones válidas (`all`). 

El predicado auxiliar `all_solutions` explora sistemáticamente las distintas formas de agregar cerveza para alcanzar la cantidad deseada de vasos, considerando los posibles caminos de llenado y el comportamiento de los desbordes entre barriles. Su funcionamiento consiste en analizar, para cada barril, si es posible satisfacer la meta sirviendo desde ese barril y, en caso afirmativo, calcular la cantidad mínima de cerveza que debe agregarse.

Primero, evalúa si es posible servir la cantidad objetivo directamente desde el barril "A". Si se puede, agrega esa solución a la lista. Luego, simula el caso en que se llena "A" y el desbordamiento pasa a "B", verificando si así se puede servir la cantidad deseada desde "B"; si es posible, también se añade esa alternativa. Posteriormente, considera el llenado de "C" desde "A", pero solo si "C" tiene menos cerveza que "A" en ese momento; si se cumple esta condición y se puede alcanzar la meta desde "C", se agrega la solución correspondiente.

El proceso se repite de forma análoga comenzando desde el barril "C": primero verifica si se puede servir la cantidad objetivo directamente desde "C", luego simula el desbordamiento hacia "B", y finalmente hacia "A" (en este último caso, solo si "A" tiene menos cerveza que "C"). En cada paso, si la combinación de llenado y desbordamiento permite alcanzar la meta, la solución se añade a la lista. Si no es posible en algún caso, simplemente se ignora y se continúa con las demás alternativas.

De esta manera, `all_solutions` construye una lista de todas las formas válidas de agregar cerveza para cumplir el objetivo, priorizando siempre las combinaciones que respetan las restricciones de capacidad y el flujo de desbordes entre barriles.

Finalmente, dependiendo del modo seleccionado, `findSolution` selecciona la mejor alternativa (la de menor cantidad agregada) o devuelve todas las opciones ordenadas. Si no existe ninguna solución válida, el predicado devuelve false. Así, `findSolution` abstrae la lógica de búsqueda y selección de estrategias, facilitando la obtención de la solución más eficiente o el análisis de todas las posibilidades.
