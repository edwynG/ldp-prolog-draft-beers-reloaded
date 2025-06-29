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
initialBarrels(["A", "B", "C"], [CA, CB, CC], [BA0, BB0, BC0]) :-
    maplist(valid_number, [CA, CB, CC, BA0, BB0, BC0]),
    CA > 0, CB > 0, CC > 0,
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

% - A y C transfieren exceso a B
% - B transfiere exceso a barril con menos cerveza (A o C)
fix_once(CA, CB, CC, A0, B0, C0, A2, B2, C2) :-
    % Desborde de A y C hacia B
    excess(A0, CA, EA, A1),
    excess(C0, CC, EC, C1),
    B1Temp is B0 + EA + EC,

    % Desborde de B hacia el barril con menor cantidad
    excess(B1Temp, CB, EB, B1),
    ( A1 =< C1 ->
        free_space(A1, CA, FA),
        Transfer is min(EB, FA),
        A2 is A1 + Transfer,
        C2 = C1
    ;
        free_space(C1, CC, FC),
        Transfer is min(EB, FC),
        C2 is C1 + Transfer,
        A2 = A1
    ),
    B2 = B1.

% Calcula el exceso si se pasa de la capacidad, devuelve el valor fijo
excess(Value, Max, Excess, Fixed) :-
    (Value > Max -> Excess is Value - Max, Fixed is Max ; Excess = 0, Fixed = Value).

% Calcula el espacio libre en un barril
free_space(Current, Capacity, Free) :-
    Free is Capacity - Current.

% Verifica que sea un número entero no negativo
valid_number(N) :-
    integer(N), N >= 0.
```
El predicado `initialBarrels` se encarga de inicializar el estado del sistema de barriles, definiendo dinámicamente los hechos barrel(ID, Capacidad, CantidadActual) en la base de conocimientos.
Este predicado recibe tres listas como argumentos: una lista fija de identificadores de barril: ["A", "B", "C"], una lista de capacidades máximas: [CapA, CapB, CapC] estas siendo estrictamente mayores a cero y una lista con las cantidades iniciales de cerveza en litros: [CervezaA, CervezaB, CervezaC].
Se verifica que todos los elementos sean números enteros no negativos mediante `maplist`.
Si alguna cantidad inicial excede la capacidad del barril, el exceso se transfiere automáticamente a otros barriles siguiendo una política de desbordes, implementada en `cascade_fix`, en donde se toma en cuenta la conexion entre los barriles, y que B tiene que trasnferir la cerveza que se desborde a el barril que contenga menos cerveza. 

### - Existe solucion 
```{prolog}
iSolution(Barrel, Beer, Goal) :-
    % Guardar capacidades y estado actual
    barrel("A", CA, BA),
    barrel("B", CB, BB),
    barrel("C", CC, BC),

    % Limpiar base de conocimientos
    retractall(barrel(_, _, _)),

    % Agregar cerveza según el barril destino
    (
        Barrel = "A" -> NewBA is BA + Beer, NewBB = BB, NewBC = BC ;
        Barrel = "C" -> NewBC is BC + Beer, NewBA = BA, NewBB = BB ;
        fail  % solo A o C permiten agregar cerveza
    ),

    % Manejo de desbordes en cascada
    cascade_fix(CA, CB, CC, NewBA, NewBB, NewBC, FinalA, FinalB, FinalC),

    % Restaurar nuevo estado corregido
    assertz(barrel("A", CA, FinalA)),
    assertz(barrel("B", CB, FinalB)),
    assertz(barrel("C", CC, FinalC)),

    % Verificar si existe solución en algún barril
    (
        FinalA >= Goal ;
        FinalB >= Goal ;
        FinalC >= Goal
    ),
    !.
```
El predicado `iSolution` determina si es posible alcanzar una cantidad objetivo (Goal) de cerveza en alguno de los barriles, luego de agregar una cantidad Beer de cerveza al barril indicado (Barrel), tomando en cuenta el manejo de desbordes en cascada.
Se recupera el estado actual de los tres barriles mediante los hechos dinámicos `barrel`.
Se agrega la cerveza a A o C según el parámetro Barrel. Si se intenta usar "B", el predicado falla.
Se aplica el manejo de desbordes mediante `cascade_fix`, que redistribuye el exceso de cerveza entre los barriles conforme a las reglas del sistema:
Desbordes en A o C se transfieren a B.
Desbordes en B se transfieren al barril con menor cantidad entre A y C.
Finalmente, se verifica si alguno de los barriles contiene al menos Goal litros. Si es así, se retorna true y se detiene la ejecución con el operador de corte (!) para evitar múltiples soluciones o esperas innecesarias.


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

```
El predicado `findSolution` es el encargado de determinar la cantidad óptima de cerveza que debe agregarse a los barriles para servir exactamente una cantidad objetivo de vasos. Su funcionamiento se basa en dos modos: encontrar la mejor solución (`best`), es decir, la que requiere agregar la menor cantidad de cerveza posible, o listar todas las soluciones válidas (`all`). 

El predicado auxiliar `all_solutions` explora sistemáticamente las distintas formas de agregar cerveza para alcanzar la cantidad deseada de vasos, considerando los posibles caminos de llenado y el comportamiento de los desbordes entre barriles. Su funcionamiento consiste en analizar, para cada barril, si es posible satisfacer la meta sirviendo desde ese barril y, en caso afirmativo, calcular la cantidad mínima de cerveza que debe agregarse.

Primero, evalúa si es posible servir la cantidad objetivo directamente desde el barril "A". Si se puede, agrega esa solución a la lista. Luego, simula el caso en que se llena "A" y el desbordamiento pasa a "B", verificando si así se puede servir la cantidad deseada desde "B"; si es posible, también se añade esa alternativa. Posteriormente, considera el llenado de "C" desde "A", pero solo si "C" tiene menos cerveza que "A" en ese momento; si se cumple esta condición y se puede alcanzar la meta desde "C", se agrega la solución correspondiente.

El proceso se repite de forma análoga comenzando desde el barril "C": primero verifica si se puede servir la cantidad objetivo directamente desde "C", luego simula el desbordamiento hacia "B", y finalmente hacia "A" (en este último caso, solo si "A" tiene menos cerveza que "C"). En cada paso, si la combinación de llenado y desbordamiento permite alcanzar la meta, la solución se añade a la lista. Si no es posible en algún caso, simplemente se ignora y se continúa con las demás alternativas.

De esta manera, `all_solutions` construye una lista de todas las formas válidas de agregar cerveza para cumplir el objetivo, priorizando siempre las combinaciones que respetan las restricciones de capacidad y el flujo de desbordes entre barriles.

Finalmente, dependiendo del modo seleccionado, `findSolution` selecciona la mejor alternativa (la de menor cantidad agregada) o devuelve todas las opciones ordenadas. Si no existe ninguna solución válida, el predicado devuelve false. Así, `findSolution` abstrae la lógica de búsqueda y selección de estrategias, facilitando la obtención de la solución más eficiente o el análisis de todas las posibilidades.
