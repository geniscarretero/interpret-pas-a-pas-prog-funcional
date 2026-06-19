bucle x = bucle x
mandra x = if (==) x 0 then 42 else bucle x

mandra 0
