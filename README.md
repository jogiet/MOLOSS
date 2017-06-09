MOLOSS
======


MOLOSS est un projet d'implémentation de solver de logique modale basé
sur des solveus SMT (d'où le nom : MOdal LOgic Solver via Smt)
sur ce [papier](https://members.loria.fr/SMerz/papers/wirsing2015.pdf).
Quelques références sur la logique modale : 
* le [site de Stanford](https://plato.stanford.edu/entries/logic-modal/)
* le format [SMT-LIB](http://smtlib.cs.uiowa.edu/index.shtml) 

État du projet
--------------

### Les Fichiers

* `ast_m.ml` AST de formules de logique modale. Plus quelques essais sur
  les axiomes et théories canoniques en logique modale. TODO : à
  compléter pour refléter les système D/M/A ...
* `ast_fo.ml` AST de formules de logique du premier ordre. Idem que pour
  `ast_m.ml`
* `convertisseur.ml` contient les fonctions de conversions FO <-> Modal
  Logic
* `pprinter.ml` Un pretty printer pour les formules de logique modale et
  du premier ordre et un printer "in string".
* `test.ml` Un fichier de test pour le conversion FO <-> Modal
* `sign.ml` Un fichier contenant la signature probable du foncteur
  permettant la communication SMT <-> FO (ou boxed_FO)
* `absconc.ml` Un fichier contenant les fonction abs et conc comme
  décrit dans le papier de départ.
