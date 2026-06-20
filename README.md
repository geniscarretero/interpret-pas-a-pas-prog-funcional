# Intèrpret Pas a Pas de Programació Funcional (BHCt)

Un intèrpret educatiu i interactiu pas a pas per a un subconjunt del llenguatge Haskell, desenvolupat íntegrament en Haskell i gestionat amb Cabal. 

Aquest projecte neix amb l'objectiu de fer transparent la "màgia" dels compiladors funcionals tradicionals. Exposa detalladament el procés intern d'inferència de tipus i imprimeix la traça d'execució pas a pas de l'avaluació mandrosa (*lazy evaluation*) mitjançant la reducció de grafs.

## Estructura del Projecte

- **bhct.cabal** - Configuració de construcció i dependències de Cabal.
- **HSParser.hs** - Analitzador lèxic i sintàctic construït des de zero basat en combinadors de parsers monàdics.
- **HSTipus.hs** - Definicions de l'Arbre de Sintaxi Abstracta (AST) i de les estructures algebraiques de tipus.
- **HSInferenciaTipus.hs** - Motor d'inferència de tipus basat en l'algorisme de Hindley-Milner, amb unificació de Robinson i protecció *Occurs Check*.
- **HSDef.hs** - Declaració del mapa de memòria (*Heap* pur), definició de les adreces del graf i registre de Supercombinadors.
- **HSEval.hs** - Motor d'avaluació mandrosa, basat en *Spine Unwinding* (exploració del camí esquerre) i reducció de grafs.
- **BHCt.hs** - Punt d'entrada principal (REPL) per a la interacció per línia de comandes.

## Primers Passos

Per arrencar l'intèrpret interactiu, executa la següent comanda a l'arrel del projecte:

```bash
cabal run
