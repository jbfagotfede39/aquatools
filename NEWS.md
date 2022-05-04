# NEWS - aquatools

## 0.0.143
### Ajouts
- `NEWS.md` : création
- `chronique.agregation` : ajout d'un export vers R possible même si export = T, afin de pouvoir récupérer certaines valeurs agrégées sans avoir à tout recalculer
- `chronique.contexte` : ajout du traitement des MOs
- `chronique.figure.classescalendaires` : création
- `chronique.figure.parametres` : 
  * ajout de classes de valeurs de référence pour calculs ultérieurs si nécessaire (`chronique.figure.classescalendaires`)
  * ajout de palette de couleurs pour classes de valeurs de référence (`chronique.figure.classescalendaires`)
- `chronique.ouverture` : 
  * création d'un format d'importation pour l'hydrologie
  * ajout d'un paramètre de feuille à lire dans le cas d'un fichier excel

### Corrections
- `chronique.correction` : refactoring d'une syntaxe qui était lourde, source de bug et qui n'apportait rien
- `chronique.figure.depassementscalendaires` : 
  * correction du résumé
  * nettoyage de paramètres inutiles et ajout pour mémoire du paramètre affichage vide, à développer
- `formatage.annee.neutre` : correction de l'intitulé du paramètre datedebutanneebiol en datedebutanneeneutre

### Modifications
- `BDD.ouverture` : ajout de la nouvelle machine de JB
- `chronique.figure` : refactoring très léger
- `chronique.figure.interannuelle` : modification de l'appel de `chronique.traitement` pour simplification

## 0.0.142
### Ajouts
- `chronique.contexte` : ajout du traitement des unités
- `chronique.figure.depassementscalendaires` : création
- `chronique.figure.cumul` : implantation de `formatage.annee.neutre`
- `formatage.anne.neutre` : création

### Corrections
- `chronique.contexte` : 
  * ajout d'une exception pour éliminer les NA
  * ajout de la création d'un champ année vide, qui avait été oublié

### Modifications
- `chronique.figure.parametres` : paramétrisation de l'unité afin de limiter les redondances et de pouvoir l'exploiter seule
