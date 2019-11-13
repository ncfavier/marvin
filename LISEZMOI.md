# [scapin](https://git.monade.li/scapin)

## Acte I : simulateur

### Compilation

Le simulateur est réalisé en Haskell (GHC), et dépend des paquets [base](https://hackage.haskell.org/package/base), [containers](https://hackage.haskell.org/package/containers) et [array](https://hackage.haskell.org/package/array).

Le Makefile fourni *devrait* fonctionner si ces paquets sont installés globalement (sous Arch Linux, compiler avec `GHCFLAGS=-dynamic`).

### Utilisation

```
./simulator [-n steps] netlist
```

La ROM (resp. la RAM) est initialisée avec le contenu du fichier `rom.bin` (resp. `ram.bin`) s'il existe, et complétée par des zéros.

### Commentaires

J'ai choisi de représenter la ROM et la RAM comme des tableaux (Data.Array) de booléens, afin de ne pas privilégier une taille de mot donnée.

Les entiers sont interprétés et stockés de manière gros-boutiste.

La simulation utilise un environnement consistant en une table de hachage (Data.Map) mutable associant à chaque variable de la netlist une valeur (initialement zéro).

À chaque étape de la simulation, les variables d'entrée sont lues au clavier et mises à jour dans l'environnement ; puis, pour chaque équation `x = e` de la netlist (supposée triée), on évalue l'expression `e` en utilisant l'ancien environnement et l'environnement actuel, et on affecte la valeur obtenue à `x`. Enfin, les variables de sortie sont affichées.
