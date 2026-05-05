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
