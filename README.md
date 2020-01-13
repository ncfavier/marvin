# [scapin](https://git.monade.li/scapin)

## Simulateur

### Instructions

Le simulateur est réalisé en Haskell (GHC), et dépend des bibliothèques [`base`](https://hackage.haskell.org/package/base), [`bytestring`](https://hackage.haskell.org/package/bytestring), [`containers`](https://hackage.haskell.org/package/containers) et [`array`](https://hackage.haskell.org/package/array).

Le Makefile fourni *devrait* fonctionner si ces bibliothèques sont installés globalement (sous Arch Linux, compiler avec `GHCFLAGS=-dynamic`).

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

## Microprocesseur (non définitif)

### Architecture

Le microprocesseur comporte une ALU, une RAM, une ROM, ainsi que les registres 32 bits suivants :

- `A` et `B` sont utilisés dans l'ALU pour les opérations mathématiques et logiques ;
- `C`, `D`, `E` et `F` sont des registres ordinaires ;
- `PC` est le compteur d'instruction ;
- `I` contient l'instruction en cours d'exécution ;
- `M` contient l'adresse mémoire en cours de lecture ou d'écriture.

La communication entre les composants se fait via un bus (un multiplexeur relié à chaque composant et contrôlé par un signal de contrôle).

Des emplacements en mémoire seront réservés pour l'entrée-sortie : synchronisation avec l'horloge système, et sortie vers un afficheur digital.

### Jeu d'instructions

Les opérandes pourront être immédiates ou absolues.

- `add`, `sub` : addition, soustraction dans `A`
- `mul`, `div`, `mod` : multiplication, division, modulo dans `A`
- `and`, `or`, `xor`, `not` : opérations logiques dans `A`
- `cmp` : comparaison
- `jmp` : saut
- `jeq`, `jne`, … : sauts conditionnels
- `lda` : RAM → `A`
- `sta` : `A` → RAM
- `hlt` : arrêt
