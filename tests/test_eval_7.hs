f x = \y -> \z -> (+) x ((+) y z)
tancament = f 10 20

tancament 5
