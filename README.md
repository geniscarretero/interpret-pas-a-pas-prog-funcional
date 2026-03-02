# interpret-pas-a-pas-prog-funcional
Un intèrpret pas a pas per a un subconjunt de Haskell, construït amb Cabal. Actualment en la fase de parser per a operacions bàsiques, amb millores incrementals previstes.

## Estructura del Projecte

- **bhct.cabal** - Configuració de construcció de Cabal
- **HSParser.hs** - Parser per a operacions bàsiques
- **HSTipus.hs** - Definicions de tipus
- **BHCt.hs** - Punt d'entrada principal amb intèrpret línia per línia

## Primer Passos

```bash
cabal run
```

## Característiques Actuals

- Parsing d'operacions bàsiques
- Intèrpret línia per línia

## Roadmap

Expandint les capacitats del parser i les característiques de l'intèrpret.