# [marvin](https://git.monade.li/marvin)

Ce projet est réalisé en Haskell (GHC 8.6.5), et dépend des bibliothèques `base`, `containers`, `array`, `mtl`, `megaparsec` et `time`.

Il contient les modules suivants :

- `Netlist` : définition et parsage des netlists ;
- `Simulator` : simulateur de netlists ;
- `Dialog` (exécutable `dialog`) : interface « dialogue » du simulateur : lit les variables d'entrée au clavier et affiche les variables de sortie ;
- `Clock` (exécutable `clock`) : interface « horloge » du simulateur : ne lit aucune variable, affiche l'heure et la date dans un format lisible, gère l'initialisation et la synchronisation ;
- `Assembler` (exécutable `assembler`) : compile un programme assembleur vers une image RAM chargeable par le simulateur.

## Simulateur

    dialog [-n STEPS] NETLIST

La ROM et la RAM sont représentées comme des tableaux de booléens, afin de ne pas privilégier une taille de mot donnée.

Les entiers sont interprétés et stockés de manière gros-boutiste.

On maintient un environnement associant une valeur à chaque variable de la netlist (initialement zéro).

La netlist n'est pas supposée triée : si une variable `a` dépend d'une variable `b`, on calculera `b` avant `a`. À chaque étape, on calcule uniquement les variables suivantes (et leurs dépendances) :

- les variables de sortie ;
- les variables `x` apparaissant dans une équation de la forme `... = REG x` ;
- les variables `x` apparaissant dans une équation de la forme `x = RAM ...`.

Cela permet de ne pas calculer les variables non utilisées, et je conjecture que c'est suffisant pour effectuer tous les effets de bord.

La RAM (resp. la ROM) est initialisée avec le contenu de l'image `ram.img` (resp. `rom.img`) si elle existe, et complétée par des zéros.

Le format pour les images RAM/ROM est le suivant :

- un entier par ligne ;
- le premier entier est la taille de mot `n` ;
- le deuxième entier est la taille `m` de l'image en mots ;
- le reste interprété comme une suite de mots de `n` bits, concaténés et éventuellement complétés pour atteindre `m` mots.

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

Le microprocesseur est écrit en minijazz (`proc.mj`), et compilé vers la netlist `proc.net`.

Il dépend d'une version légèrement modifiée de minijazz (fournie dans le dépôt) : le type des fonctions `reg` et `mux` permet de les utiliser avec des signaux de taille quelconque.

## Assembleur

    assembler FILE

L'assembleur permet de compiler depuis un langage assembleur simpliste vers une image RAM contenant un programme exécutable par le microprocesseur.

Il supporte trois types d'instructions :

- les affectations : `x = v` ;
- les étiquettes : `label:` ;
- les instructions machine : `ins [op1 [op2]]`.

L'image RAM est écrite sur la sortie standard.

## Horloge

    clock [OPTIONS...] NETLIST
      -a           --async          run in async mode
      -i TIMEDATE  --init=TIMEDATE  initial timedate

L'option `-a` permet d'ignorer l'horloge système et de faire tourner l'horloge le plus rapidement possible ; l'option `-i` permet d'initialiser l'horloge à un instant donné (format `%Y-%m-%d %H:%M:%S`).

Certains emplacements en mémoire sont utilisés pour l'entrée-sortie :

- les adresses 1024 - 1029 contiennent les secondes, minutes, heures, jours, mois et années ;
- l'adresse 1030 sert à la synchronisation : le simulateur ajoute 1 à cet emplacement pour chaque seconde écoulée, l'horloge soustrait 1 à chaque itération et s'arrête quand il atteint 0 (en mode asynchrone, on fixe cet emplacement à 1).

Le programme assembleur de l'horloge se trouve dans `clock.asm`, et doit être compilé vers `ram.img`.

L'horloge tient compte des années bissextiles.

## TL;DR

La cible `runclock` du Makefile fourni permet de tout compiler et de lancer l'horloge en mode synchrone.

La variable `async` peut être définie pour activer le mode asynchrone (`make runclock async=1`).
