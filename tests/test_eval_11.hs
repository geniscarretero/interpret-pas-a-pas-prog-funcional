aplicaVegades n f x = if (==) n 0 then x else aplicaVegades ((-) n 1) f (f x)

aplicaVegades 3 (\y -> (*) y 2) 5
