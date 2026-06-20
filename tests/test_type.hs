-- Funcions bàsiques
const a b = a
id x = x
(.) f g x = f (g x)
not x = if x then False else True

:t id True
:t id id
:t const 5 True
:t const id 1

:t (.) not (const False)
:t (.) id id
:t (.) (\x -> x + 1) (\y -> y * 2)
:t (.) (\b -> not b) (\x -> x > 5)

:t (\f x -> f (f x))
:t (\f x -> f (f x)) (\n -> n + 1)
:t (\f x -> f (f x)) not

:t (\x -> (x + 1) > 0)
:t (\x y -> (x == y) && (x > 0))

:t (\x -> (\x -> x > 0) (x + 1))
:t (\f x -> f (x == 5))

:t 1 + True
:t not 5
:t id 1 2
:t (.) not not 5

-- Pattern matching
:t (\1 -> True)
:t (\True -> 123)
:t (\1 -> 1+1)

-- ==========================================
-- === NOUS TESTS NEGATIUS (Sense :t) =======
-- ==========================================

-- 1. Variables no lligades (Unbound variables)
variableFantasma
(\x -> x + y) 5

-- 2. Error d'ocurrència / Tipus infinit (Occurs check)
-- L'inferidor no hauria de poder unificar la 'x' amb el tipus de 'x x'
\x -> x x
(\f -> f f) (\x -> x)

-- 3. Errors de Tipus (Type Mismatches) directes
-- Intentar utilitzar un enter com a condició
if 42 then 1 else 0

-- Les branques de l'If retornen tipus diferents (Int vs Bool)
if True then 1 else False

-- Intentar aplicar un valor no funcional com si fos una funció
True 5
(1 + 2) 3

-- Passar l'argument incorrecte a una lambda
(\x -> x && False) 10

-- 4. Errors en temps d'execució (si el teu llenguatge té divisió)
10 / 0
