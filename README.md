# [scapin](https://git.monade.li/scapin).

## Acte I. Le simulateur

### Instructions

Le simulateur est réalisé en Haskell (GHC), et dépend des paquets [`base`](https://hackage.haskell.org/package/base), [`bytestring`](https://hackage.haskell.org/package/bytestring), [`containers`](https://hackage.haskell.org/package/containers) et [`array`](https://hackage.haskell.org/package/array).

Le Makefile fourni *devrait* fonctionner si ces paquets sont installés globalement (sous Arch Linux, compiler avec `GHCFLAGS=-dynamic`).

Le projet peut également être compilé et lancé avec [Cabal](https://www.haskell.org/cabal/), qui se charge d'installer les dépendances :

```
cabal v2-build
cabal v2-exec simulator -- [arguments...]
```

Les arguments sont attendus au format suivant :

```
simulator [-n steps] netlist
```

La ROM (resp. la RAM) est initialisée avec le contenu du fichier `rom.bin` (resp. `ram.bin`) s'il existe, et complétée par des zéros.

### Commentaires

La ROM et la RAM sont représentées comme des tableaux (`Data.Array`) de booléens, afin de ne pas privilégier une taille de mot donnée.

Les entiers sont interprétés et stockés de manière gros-boutiste.

Le simulateur maintient un environnement consistant en une table de hachage (`Data.Map`) mutable qui associe à chaque variable de la netlist une valeur (initialement zéro).

À chaque étape de la simulation :

- les variables d'entrée sont lues au clavier et mises à jour dans l'environnement ;
- pour chaque équation `x = e` de la netlist (supposée triée), on évalue l'expression `e` et on affecte la valeur obtenue à `x` dans l'environnement ;
- les variables de sortie sont affichées.
