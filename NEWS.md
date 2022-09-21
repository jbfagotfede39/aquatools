# NEWS - aquatools

## 0.0.146
### Ajouts
- `df.comparaison.champs` : Création

### Corrections
- `poissons.poids` : 
  * Ajout d'un échappement de `NA` dans le cas de la présence d'une taille moyenne vide
  * Conversion d'une matrice en tibble pour jointure moins problématique selon configuration machine

### Modifications
- `formatage.ecosysteme` : 
  * Suppression des exceptions manuelles (source de plantage), à revoir avec les tables récentes fd.referentiels.dictionnaire_correction
- `poissons.lac.irsteaversteleos` : 
  * Renommage en `poissons.plansdeau.ofbversteleos`
  * Correction d'une coquille dans la documentation
  * Modification des noms de variables en snake case
  * Transformation des fonctions paste0 en glue
  * Choix du format de coordonnées
  * Nettoyage de NA qui s'affichaient si les champs étaient vides
  * Recalcul des lots I en lots G, avec avertissement de l'utilisateur

## 0.0.145
### Ajouts
- `chronique.ouverture` : ajout de l'importation des mesures hydrologiques issues de Hub'Eau
- `chronique.periode` : 
  * correction de l'intitulé de deux paramètres dans l'en-tête
  * correction d'erreurs de syntaxe dans les exemples
- `chronique.resultats.periode` :
  * Utilisation de `chronique.variables.renommage` (#31)

### Corrections
- `BDD.ouverture` : ajout d'une modalité pour l'identification du poste de JB en local
- `chronique.resultats.periode` :
  * Résolution d'un bug d'exécution
  * Nettoyage
- `poissons.poids` : déplacement du calcul des estimations (et surtout du `cf`) hors d'une condition

### Modifications
- `chronique.resultats.periode` :
  * Modification du séparateur : `;` devient `,`, afin d'harmoniser les différents séparateurs
  * Nettoyage

### Suppressions

## 0.0.144 - 2022-05-31
### Ajouts
- `chronique.figure.classescalendaires` : ajout partiel du traitement du paramètre `affichagevide`
- `chronique.cle` : ajout du paramètre `Unité` et de la configuration `STU`
- `chronique.figure.interannuelle` : ajout du réglage des Ymin et Ymax dans le cas de l'oxygénation
- `chronique.ouverture` : ajout de la modalité d'importation des données piézométriques `RuggedTROLL`
- `chronique.regroupement` : création
- `poissons.captures` : ajout du calcul de `poids_moy` pour les lots
- `poissons.poids` : 
  * déplacé depuis `afd39`
  * ajout de tests en entrée, avec limitation à une seule espèce et effectif minimal de 30 individus
  * ajout d'un réglage du pas de taille pour les données estimées
  * modification des données traitées en entrée pour affiner le calcul : taille et poids moyens pour les lots
  * ajout de l'extraction et du test de la p-value du modèle
  * ajout d'un traitement différencié : sortie d'un tableau de référence, ou bien complément d'un jeu de données incomplet, en fonction de la fourniture ou non en entrée de ce dernier
  * affichage dans les sorties de l'espèce et des milieux des données de référence

### Corrections
- `chronique.figure.cumul` : 
  * correction du paramètre `datedebutanneeneutre` dans l'appel de la fonction `formatage.annee.neutre`.
  * correction de la sortie de `chronique.agregation` suite à l'évolution du format de sortie de celle-ci
- `chronique.figure.interannuelle` : modification de l'appel de chronique.traitement

### Modifications
- `chronique.agregation` : 
  * modification de la syntaxe de la localisation des fichiers de sortie avec `glue`
  * léger refactoring afin de corriger problème créé par la dernière modification
  * modification format de sortie (liste -> tibble) vers R dans le cas de l'appel d'un seul paramètre (`ValJours` par exemple)
- `chronique.figure.classescalendaires` : ajout du ré-encodage des extremums dans le cas de l'oxygénation
- `chronique.figure.parametres` : augmentation de l'échelle des classes de valeurs dans le cas de l'oxygénation

## 0.0.143 - 2022-05-04
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

## 0.0.142 - 2022-03-22
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