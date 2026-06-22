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
```

Un cop dins de l'intèrpret (`bhct> `), tens diverses eines a la teva disposició:

**1. Definició de funcions (Bindings):**
Pots definir funcions lineals o amb guàrdies directament a la consola.
```haskell
bhct> suma x y = x + y
bhct> factorial n | n == 0 = 1 | otherwise = n * factorial (n - 1)
```

**2. Inspecció de Tipus (`:t`):**
Avalua estàticament l'expressió i en dedueix el tipus més general (polimòrfic).
```haskell
bhct> :t (\x -> x + x)
Int -> Int

bhct> id x = x

bhct> :t id
t6.8 -> t6.8

bhct> (.) f g x = f (g x) 

bhct> :t (.)
(t16.18 -> t17.18) -> (t12.18 -> t16.18) -> t12.18 -> t17.18
```

**3. Inspecció de l'AST (`:a`):**
Mostra la representació interna de l'arbre de sintaxi abans de convertir-lo en graf.
```haskell
bhct> :a (+) 2 3
App (App (Var "+") (Val 2)) (Val 3)
```

**4. Avaluació Pas a Pas:**
Introdueix qualsevol expressió per veure la seva seqüència de reducció (com col·lapsa el graf a cada pas fins arribar a la Forma Normal de Cap Feble - WHNF).
```haskell
bhct> (\x y -> x * y) 2 3
(\x y -> (*) x y) 2 3
(\y -> (*) 2 y) 3
(*) 2 3
6
```

## Característiques Actuals

- **Anàlisi Sintàctica Nativa:** Construïda exclusivament amb combinadors monàdics, capaç de desugarejar aplicacions parcials (*currying*), lambdes aniuades i guàrdies lògiques (`|`).
- **Inferència de Tipus Robusta:** Sistema Hindley-Milner complet. Resol el polimorfisme paramètric generant variables fresques dinàmiques i atura l'execució d'expressions divergents gràcies al *Occurs Check* (prevenció de tipus infinits).
- **Avaluació Mandrosa (*Lazy Evaluation*):** El nucli redueix les expressions sota demanda mitjançant *Spine Unwinding*, compartint referències de memòria temporal mitjançant un *Heap* purificat basat en `IntMap`.
- **Recursivitat Eficient:** Implementació de *Supercombinadors* que permet resoldre funcions recursives de manera nativa al graf, evitant saturacions de l'AST.
- **Transparència Educativa:** Formatatge textual estètic de l'estat del graf a cada instant (`hsprint`) dissenyat expressament per seguir les beta-reduccions i el càlcul de les primitives.
