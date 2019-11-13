# [scapin](https://git.monade.li/scapin).

## Acte I : le simulateur

### *Scène 1* : compilation

Le simulateur est réalisé en Haskell (GHC), et dépend des paquets [`base`](https://hackage.haskell.org/package/base), [`containers`](https://hackage.haskell.org/package/containers) et [`array`](https://hackage.haskell.org/package/array).

Le Makefile fourni *devrait* fonctionner si ces paquets sont installés globalement (sous Arch Linux, compiler avec `GHCFLAGS=-dynamic`).

### *Scène 2* : utilisation

```
./simulator [-n steps] netlist
```

La ROM (resp. la RAM) est initialisée avec le contenu du fichier `rom.bin` (resp. `ram.bin`) s'il existe, et complétée par des zéros.

### *Scène 3* : commentaires

La ROM et la RAM sont représentées comme des tableaux (`Data.Array`) de booléens, afin de ne pas privilégier une taille de mot donnée.

Les entiers sont interprétés et stockés de manière gros-boutiste.

Le simulateur maintient un environnement consistant en une table de hachage (`Data.Map`) mutable qui associe à chaque variable de la netlist une valeur (initialement zéro).

À chaque étape de la simulation :

- les variables d'entrée sont lues au clavier et mises à jour dans l'environnement ;
- pour chaque équation `x = e` de la netlist (supposée triée), on évalue l'expression `e` et on affecte la valeur obtenue à `x` dans l'environnement ;
- les variables de sortie sont affichées.
