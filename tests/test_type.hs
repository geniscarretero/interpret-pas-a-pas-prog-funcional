-- 1. Polimorfisme bàsic i aplicació
:t id True
:t id id
:t const 5 True
:t const id 1

-- 2. Composició de funcions (El punt és el cas més difícil)
:t (.) not (const False)
:t (.) id id
:t (.) (\x -> x + 1) (\y -> y * 2)
:t (.) (\b -> not b) (\x -> x > 5)

-- 3. Funcions d'ordre superior (passar funcions com a arguments)
:t (\f x -> f (f x))
:t (\f x -> f (f x)) (\n -> n + 1)
:t (\f x -> f (f x)) not

-- 4. Unificació múltiple amb operadors
:t (\x -> (x + 1) > 0)
:t (\x y -> (x == y) && (x > 0))

-- 5. Casos de "Shadowing" i Scope (comprova si el context es neteja bé)
:t (\x -> (\x -> x > 0) (x + 1))
:t (\f x -> f (x == 5))

-- 6. Casos que HAURIEN DE FALLAR (per comprovar que el teu Either retorna Left)
:t 1 + True
:t not 5
:t id 1 2
:t (.) not not 5
