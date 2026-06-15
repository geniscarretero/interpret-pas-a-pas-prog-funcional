-- === FUNCIONS COMBINATÒRIES BÀSIQUES ===

f $ x = f x

(.) f g x = f (g x) 

x & f = f x

on f g x y = f (g x) (g y)

x /= y = if (x == y) then False else True 

p ==> q = if p then q else True

x ^ y = if x then not y else y

base ** exp = if exp == 0 then 1 else base * (base ** (exp - 1))

-- Funció Identitat
id x = x

-- Funció Constant (ignora el segon argument)
const x y = x

-- Intercanvia l'ordre dels arguments d'una funció
flip f x y = f y x


-- === LÒGICA I BOOLEANS ===

-- Negació lògica
not b = if b then False else True


-- === ARITMÈTICA ===

-- Successor i Predecessor
succ x = x + 1
pred x = x - 1


-- === FUNCIONS RECURSIVES ESTÀNDARD ===
-- Nota: Funcionaran si el teu DefEnv ja permet que una funció es cridi a si mateixa recursivament

-- Factorial d'un nombre
factorial n = if (n == 0) then 1 else (n * factorial (n - 1))

-- Fibonnaci (versió ingènua de doble recursivitat)
fibonacci n = if n <= 1 then n else (fibonacci (n-1) ) + (fibonacci (n-2))

-- Inversa/Negació d'un nombre (x * -1)
negate x = 0-x

-- Valor absolut
abs x  = if x < 0 then (0-x) else x
