# interpret-pas-a-pas-prog-funcional
Un intèrpret pas a pas per a un subconjunt de Haskell, construït amb Cabal. Actualment en la fase d'avaluació lazy.

## Estructura del Projecte

- **bhct.cabal** - Configuració de construcció de Cabal
- **HSParser.hs** - Parser per a operacions bàsiques
- **HSTipus.hs** - Definicions de tipus
- HSInferenciaTipus.hs - Inferència de tipus
- HSEval.hs - Avaluació lazy
- **BHCt.hs** - Punt d'entrada principal amb intèrpret línia per línia

## Primer Passos

```bash
cabal run
```
Un cop obert l'intèrpret...
Si es vol veure el tipus d'una expressió:
```bash
:t (\x -> x + x)
```
Si es vol veure l'ast:
```bash
:a (+) 2 3
```
Si es vol usar de forma normal, avaluant les expressions:
```bash
(\x -> x * 2) 2
```
## Característiques Actuals

- Parsing de variables, operacions, literals, aplicacions i abstraccions
- Intèrpret línia per línia
- Inferència de tipus amb polimorfisme
- Construcció d'un graf dirigit a partir de l'ast 
