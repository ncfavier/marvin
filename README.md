# [scapin](https://git.monade.li/scapin)

Ce projet est réalisé en Haskell (GHC), et dépend des bibliothèques `base`, `containers`, `array`, `mtl`, `megaparsec` et `time`.

## Simulateur

    simulator [-n steps] netlist

La ROM et la RAM sont représentées comme des tableaux (`Data.Array`) de booléens, afin de ne pas privilégier une taille de mot donnée.

Les entiers sont interprétés et stockés de manière gros-boutiste.

Le simulateur maintient un environnement consistant en une table de hachage (`Data.Map`) mutable qui associe à chaque variable de la netlist une valeur (initialement zéro).

À chaque étape de la simulation :

- les variables d'entrée sont lues au clavier et mises à jour dans l'environnement ;
- pour chaque équation `x = e` de la netlist (supposée triée), on évalue l'expression `e` et on affecte la valeur obtenue à `x` dans l'environnement ;
- les variables de sortie sont affichées.

## Microprocesseur

La taille de mot utilisée est de 42 bits.

Le microprocesseur comporte une ALU, une RAM, une ROM, ainsi que les registres `A`, `B` et `PC` (compteur d'instruction).

La communication entre les composants se fait via un bus (un multiplexeur relié à chaque composant).

Chaque instruction occupe exactement trois emplacements mémoire : un *opcode* et deux opérandes (optionnelles).

Les différentes parties du microprocesseur sont orchestrées par divers signaux de contrôle, chacun correspondant à un bit de l'opcode en cours d'exécution.

Les instructions suivantes sont disponibles :

- `add`, `sub` : addition, soustraction dans `A`
- `mul`, `div`, `mod` : multiplication, division, modulo dans `A`
- `and`, `or`, `xor`, `not` : opérations logiques dans `A`
- `cmp` : comparaison
- `jmp` : saut
- `jeq`, `jne`, … : sauts conditionnels
- `lda` : RAM → `A`
- `sta` : `A` → RAM

Chaque opérande peut faire référence à un entier, un registre, ou un emplacement mémoire.

Le microprocesseur est écrit en minijazz (`proc.mj`), et compilé à l'aide d'une version légèrement modifiée de minijazz : le type des fonctions `reg` et `mux` permet de les utiliser avec des signaux de taille quelconque.

## Assembleur

Le module `Assembler` fournit un programme `assembler` permettant de compiler depuis un langage assembleur simpliste vers une image RAM contenant un programme exécutable par le microprocesseur.

L'assembleur comprend trois types d'instructions :

- les affectations : `x = v` ;
- les étiquettes : `label:` ;
- les instructions : `ins [op1 [op2]]`.

Les commentaires de ligne débutent par le symbole `;`.

Le format de l'image RAM est : un entier par ligne, représentant un mot de 42 bits.

## Horloge

    clock [OPTIONS...] NETLIST
      -a           --async          run in async mode
      -i TIMEDATE  --init=TIMEDATE  initial timedate

L'exécutable `clock` fourni par le module `Clock` est une interface pour le simulateur qui se charge de la synchronisation avec l'horloge système et de l'affichage de l'horloge dans un format lisible. L'option `-a` permet d'ignorer l'horloge système et de faire tourner l'horloge le plus rapidement possible ; l'option `-i` permet d'initialiser l'horloge à un instant donné (format `%Y-%m-%d %H:%M:%S`).

La RAM (resp. la ROM) est initialisée avec le contenu de l'image `ram.bin` (resp. `rom.bin`) si elle existe, et complétée par des zéros.

Des emplacements en mémoire sont utilisés pour l'entrée-sortie :

- les adresses 1024 - 1029 contiennent les secondes, minutes, heures, jours, mois et années ;
- l'adresse 1030 sert à la synchronisation : le simulateur écrit 1 à cet emplacement quand une seconde est passée (ou à chaque étape, en mode asynchrone).

Le programme assembleur de l'horloge se trouve dans `clock.asm` et doit être compilé vers `ram.bin` avec la commande suivante :

    ./assembler clock.asm > ram.bin

L'horloge peut ensuite être lancée avec la commande :

    ./clock proc.net

Et arrêtée avec `Ctrl + C`.

## Tout à la fois

La cible `runclock` du Makefile fourni permet de tout compiler et de lancer l'horloge en mode synchrone.

La variable `ASYNC` peut être mise à 1 pour le mode asynchrone (`make ASYNC=1 runclock`).
