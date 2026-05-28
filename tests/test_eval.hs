-- ==============================================================================
-- JOC DE PROVES EXTENS PER A L'INTÈRPRET DE HASKELL (REDUCCIÓ DE GRAFS)
-- Format: Una expressió per línia (les línies que comencen amb '--' són comentaris)
-- ==============================================================================

-- --- CATEGORIA 1: ARITMÈTICA I BOOLEANS BÀSICS ---
-- Objectiu: Validar les primitives binàries directes i l'espina (spine unwinding) bàsica.
5 + 3
20 - 7
4 * 6
12 / 4
True && False
True || False
5 == 5
10 < 3
5 <= 5
8 > 12
7 >= 7

-- --- CATEGORIA 2: EXPRESSIONS NIUADES ---
-- Objectiu: Forçar l'avaluació d'arguments abans d'executar la primitiva (reducció cap a WHNF de subarbres).
(2 + 3) * (10 - 6)
100 - (24 / (2 + 2))
(5 < 10) && (3 == 3)
(10 <= 10) || (5 > 20)
((2 * 3) + (4 * 5)) - (10 / 2)

-- --- CATEGORIA 3: LAMBDES AMB UN PARÀMETRE (BETA-REDUCCIÓ BÀSICA) ---
-- Objectiu: Comprovar la creació de nodes NLam, l'enllaç de la variable i la substitució bàsica al Heap.
(\x -> x + 5) 3
(\x -> x) (20 / 4)
(\x -> x < 10) (3 * 3)
(\id -> id) True

-- --- CATEGORIA 4: MÚLTIPLES PARÀMETRES I CURRYING ---
-- Objectiu: Avaluar l'acumulació d'arguments a la pila (stk) i el desenrotllament de lambdes niuades.
(\x y -> x + y) (2 * 3) 4
(\x y -> x - y) 10 3
(\x y -> x == y) 5 (2 + 3)
(\x y z -> x * y + z) 2 3 4
(\f x -> f x) (\y -> y + 1) 5

-- --- CATEGORIA 5: COMPARTICIÓ AL HEAP (VARIABLES DUPLICADES) ---
-- Objectiu: Verificar que la mateixa adreça del Heap s'usa en múltiples llocs sense crear inconsistències.
(\x -> x + x) (3 * 4)
(\x -> (x == 5) && (x < 10)) 5
(\x -> x * x * x) 3
(\x -> (x < 0) || (x > 0)) 4

-- --- CATEGORIA 6: LAMBDES NIUADES I OMBREJAT (SHADOWING) ---
-- Objectiu: Assegurar que l'entorn de noms (strAddr) gestiona correctament els àmbits (scopes) i que la x interna tapa la x externa.
(\x -> (\y -> x * y) 2) 10
(\x -> (\x -> x + 1) 5) 100
(\x y -> (\z -> x + y + z) 1) 2 3
(\x -> x + (\x -> x * 2) 3) 5

-- --- CATEGORIA 7: CASOS LÍMIT I APLICACIÓ PARCIAL (WHNF) ---
-- Objectiu: Testejar que le motor s'atura correctament quan s'arriba a una forma normal de cap feble, encara que faltin arguments.
(\x y -> x + y) 5
(\x y z -> x - y - z) 10 2
