aplicaMult f a i = if i == 0 then a else f (aplicaMult f a (i-1))

aplicaMult (\x -> x + 1) 0 5
