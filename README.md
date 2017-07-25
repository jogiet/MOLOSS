MOLOSS : Guide d’utilisation sommaire
=====================================

Pour la compilation
-------------------

On compile avec la commande `make` . On obtient un exécutable `moloss`.

De plus :

- `make tst` compile un exécutable `test.native` .

- `make rapport` compile un rapport.

- `make doc` compile une documantation du projet au format HTML. Cette doc ne contient pas les oracles de minisat et mSAT !

Avant de compiler, il faut s’assurer que vous êtes bien sur un système unix (fait des appels systèmes unix) et que vous disposiez des packages suivants :

-   [menhir], pour l’analyseur syntaxique

-   [msat], comme oracle

-   [minisat], comme oracle

Pour l'exécution
----------------

La version actuelle est celle présentée dans le papier, sans la logique hybride ni les opérateurs **A** et **E** des modalités globales, plus les extensions correspondantes aux procédures de décisions des axiomes.

L’exécutable prend en argument un fichier et affiche la satisfiabilité de la formule, ou bien un fichier et affiche la satisfiabilité de sa négation, car on s’intéresse à la validité du problème[1]. On peut aussi donner des options :

- `--all` : fait résoudre la formule par tous les oracles SAT (l’aorcle par défaut est minisat).

- `--z3`, `--mSAT` résout la formule avec l’oracle spécifié.

- `--time`: affiche à la fin le temps pris pour l’exécution, ce qui inclut celui nécessaire au parsing de la formule.

- `--get-model`  : affiche les différentes assertions faites par le solveur dans l’oracle, et le cadre de Kripke si la formule est satisfiable.

- `--direct`   : ne résout pas la formule avec les procédures de décision mais envoie la formule premier ordre à z3[2].

On peut aussi lancer un test sur des formules générées aléatoirement avec la commande :

`./test.native <nb_de_tests> <profondeur> <logique?>`


[1] Les bench de TANCS2000 sont conçues pour faire croîtrele modèle (de l’ordre de 1500 mondes). n’est donc pas performant sur ces bench car il construit Plus bande “horizontale” K4. le modèle.

[2] À ne pas confondre avec qui appelle z3 comme oracle SAT, mais applique les procédures de décision de .

  [menhir]: https://opam.ocaml.org/packages/menhir/
  [msat]: https://opam.ocaml.org/packages/msat/
  [minisat]: https://opam.ocaml.org/packages/minisat/


