factorial n acc = if (==) n 0 then acc else factorial ((-) n 1) ((*) n acc)

factorial 5 1
