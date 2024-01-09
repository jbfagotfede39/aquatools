# NEWS - aquatools

## 0.0.152
### Ajouts
- `formatage.annee.biologique` :
  * Ajout des formats `chmesgr_date` et `chmres_date`
  * Intégration d'une transformation des données de physico-chimie pour `PC.contexte`
  * En-tête du nom du champ en sortie correspondant à celui en entrée (`pcmes_date` -> `pcmes_anneebiol`)
- `PC.contexte` :
  * Ajout de la gestion des unités
  * Ajout de la gestion des années biologiques et nombre et liste des années biologiques
  * Création des champs manquants si nécessaire
- `PC.figure` :
  * Ajout d'un choix du thème graphique
- `PC.figure.parametres` : 
  * Création d'un affichage automatique par défaut des légendes, puis traitement des cas spécifiques comme des exceptions (permet de traiter tous les cas non intégrés au système)
  * Ajout de dépendances

### Corrections
- `chronique.variables.renommage` : 
  * Correction des exemples avec les bons noms de paramètres
  * Correction d'un `_` qui était ajouté en trop

### Modifications
- `PC.contexte` :
  * Refactoring important
  * Modification des noms de champs de sortie
  * Meilleure gestion des noms de milieux absents
- `PC.figure` :
  * Modification des exemples avec noms des jeux de données en snake_case
  * Modification de noms de variable en snake_case
  * Renommage du jeu de données `PC` en `data`
  * Versionnage des variables `data` et `contexte`
  * Amélioration de l'établissement du contexte
  * Modification des titres de légende et de l'axe X, avec affichage conditionnel
  * Utilisation de `PC.contexte`
- `PC.figure.parametres` : 
  * Mise en cohérence des noms de champs en entrée suite à évolution de `PC.contexte`
- `PC.parametres` : reformatage des noms de variables

## 0.0.151
### Ajouts
- `chronique.agregation` : ajout d'une option d'agrégation uniquement avec la station
- `chronique.cle` : ajout du format `TU`
- `chronique.ouverture` : ajout d'un forçage pour ne pas créer de noms de colonnes, car générés ensuite. C'était nécessaire pour importer des fichiers sans noms de colonnes, car `skipvalue = 0` faisait perdre la première ligne qui était considérée comme des titres
- `chronique.regroupement` : ajout d'une option `regroupement` permettant de regrouper (moyenne, somme et nombre) les données s'il en existe plusieurs pour une même modalité temporelle
- `formatage.annee.biologique` : ajout d'une dépendance
- `formatage.annee.neutre` : ajout d'une dépendance
- `PC.hubeau` : création
- `poissons.captures` : 
  * Ajout d'une option d'extraction à partir du codeOperation avec le paramètre `operation`
  * Petites corrections de code et de documentation (organisation)

### Modifications
- `BDD.ouverture` : 
  * Ajout de la connexion `appshiny` depuis la VM201 - #172
  * Correction complémentaire pour `automate` suite aux modifications précédentes

## 0.0.150
### Ajouts
- `DESCRIPTION` :
  * Ajout du champ `Maintainer`
  * Mise à jour des dépendances et du champ `License`
- `chronique.cle` : 
  * ajout du traitement des données de suivi
- `chronique.ouverture` : 
  * ajout de la création si nécessaire du champs `chsvi_actionafaire` lors de l'importation de données de suivi
  * ajout du renommage du champs `Profondeur` lors de l'importation de données de suivi
  * ajout d'un tri chronologique lors de l'importation de mesures
  * ajout des modalités d'ouverture de chroniques `mdy_hms` et `mdy_hm`
- `PC.renommage` : création

### Corrections
- `PC.figure` : 
  * Correction d'intitulés de champs pour qu'ils correspondent à ceux utilisés dans la base de données
  * Amélioration des exemples
- `ModeleRenduPeche.Rnw` et `ModeleRenduPecheCommente.Rnw` : correction d'une syntaxe devenue problématique, et qui était imprécise

### Modifications
- `BDD.ouverture` : 
  * Commentaire plus précis pour les différents versions serveur
  * Ajout de la connexion automate depuis la VM201 - #172
  * Modification du code afin d'aller chercher le mot de passe dans `.pgpass` dans les VM

## 0.0.149
### Ajouts
- `chronique.figure.classescalendaires` : ajout d'un test de vérification qu'on a bien des données journalières en entrée
- `PC.concentrationO2` : création
- `topographie.hypsometrie` : création

### Corrections
- `PC.saturationO2` : modification du mode de calcul car source initiale indisponible en ligne, remplacement par SANDRE (close #76)
- `poissons.plansdeau.ofbversteleos` : 
  * Ajout d'un ré-encodage des types de filets CEN et du test de bon traitement associé

## 0.0.148
### Ajouts
- `df.comparaison.champs` : ajout d'une comparaison des datatypes des dataframes

### Corrections
- `PC.lac.vuespatiale` : 
  * Correction du nom de variable `ContourPlandeau` en `contourplandeau` conformément à la déclaration en début de fonction
- `poissons.plansdeau.ofbversteleos` : 
  * Filtrage des lignes de captures potentiellement importées mais vides
  * Correction automatique des formats de date et d'heure de pose et de relève
  * Ajout d'un test d'aiguillage vers le regroupement des lots I en lots G
  * Ajout de la dépendance à `openxlsx`
  * Remplacement d'un `stop` par un `warning` concernant `Présence d'actions de pêche sans capture (ou capture vide) en face`

## 0.0.147
### Ajouts
- `chronique.figure.classescalendaires` : 
  * complément et optimisation du code pour l'option `affichagevide`
  * ajout de commentaires afin d'organiser le code
- `chronique.figure.depassementscalendaires` : 
  * création du code pour l'option `affichagevide`
  * nettoyage de l'aide
- `formatage.annee.neutre` : ajout d'un exemple avec filtrage de la période estivale uniquement
- `SIG.export` : ajout d'un exemple avec renommage du projet avec des undescores
- `SIG.conversion.dms2dec` : création

### Corrections
- `chronique.analyse` : 
  * suppression de la conversion en secondes qui faussait les résultats, car l'unité temporelle est maintenant automatiquement affichée dans la cellule
  * correction par complément du calcul des dates de début et de fin des épisodes de dépassement les plus longs, donc de la durée qui en est déduite : lorsqu'il y en a plusieurs de même durée, seul l'épisode le plus tardif est conservé (précédemment c'était le début du premier épisode et la fin du dernier épisode qui étaient considérées)
- `chronique.figure.interannuelle` : correction de l'ajustement des paramètres en fonction du typemesure, qui était forcé
- `poissons.plansdeau.ofbversteleos` : 
  * Ajout de volets dans le fichier excel exporté
  * Conversion en type numérique de la maille, de la taille individuelle/min/max, du poids et de l'effectif du lot pour les captures et coordonnées X et Y pour les settings
  * Modification de l'appel à `poissons.especes()` afin d'optimiser le temps de traitement
  * Correction d'une erreur dans la construction du champ des observations de `settings`

### Modifications
- `chronique.figure.depassementscalendaires` : inversion des couleurs par défaut, afin de coller à la thermie : on personnalisera uniquement pour la piézométrie

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
