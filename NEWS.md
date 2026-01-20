# NEWS - aquatools

## 0.0.186 - 2026-01-20
### Ajouts
- `chronique.ouverture` : 
  - amélioration du message d'erreur en cas d'absence de coordonnées dans l'importation des stations
  - ajout d'un message d'erreur en cas d'absence de la colonne `chsta_coderhj` dans l'importation des stations
- `SIG.conversion.l93dec2` : complément de l'exemple avec mapview()
- `SIG.conversion.31n2l93` : création
- `SIG.conversion.32n2l93` : complément de l'exemple avec mapview()
- `SIG.conversion.dec2l93` : 
  * correction de l'exmple afin de mettre les coordonnées dans le bon sens
  * complément de l'exemple avec mapview()

### Modifications
- `SIG.export` : amélioration afin que les champs `NAME` et `DESCRIPTION` soient bien complétés
  
### Corrections
- `chronique.recalage.valeur` : correction du test de valeur vide de la variable `decalage`
- `topographie.iam` : correction du traitement des `GGR` qui était lacunaire
- `SIG.conversion` : correction de l'ordre des coordonnées en sortie

## 0.0.185 - 2025-12-18
### Ajouts
- `iam_cotation` : ajout des regroupement (`BLO`/`BLS`, etc.)
- `palette_habitats` : création
- `habitats_exemple` : création
- `topographie.habitats.vue` : création
- `topographie.iam` : ajout du calcul de `tpiam_superficie` et `tpiam_longueur_mouillee`

### Modifications
- `iam_cotation` : renommage depuis `iam`
  
### Corrections
- `topographie.iam` : 
  * correction d'un bug introduit via l'ajout du message d'erreur de `0.0.184`
  * correction calcul IAM avec catégories multiples qui ne comptent que pour 1 substrat
  * correction calcul de l'indice de diversité qui générait des `NaN` dans le cas de surfaces très faibles (~ 0)

## 0.0.184 - 2025-12-10
### Ajouts
- `chronique.analyse` : ajout des `typemesure` `Nitrates` et `Conductivité`
- `chronique.ouverture` : ajout d'un cas de traitement pour les données piézométriques Hobo
- `formatage.time` : ajout des cas `pcmes_remarques` et `pcsvi_remarques`
- `formatage.date.heure` : ajout d'un paramètre `entete` qui permet de forcer l'entête s'il n'en existe pas dans le jeu de données initial
- `topographie.iam` : 
  * ajout d'exemples
  * amélioration du message d'erreur si écart trop important dans les surfaces entre tables
- `SIG.conversion` : Création
- `SIG.conversion.32n2l93` : Création

### Modifications
- `chronique.complete` : refactoring complet
- `PC.hubeau` : changement dy datatype de `resultat` de caractères vers numérique
- `SIG.conversion.dec2l93` : refactoring complet avec utilisation de `SIG.conversion`
- `SIG.conversion.l93dec2` : refactoring complet avec utilisation de `SIG.conversion`
  
### Corrections
- `chronique.ouverture` : correctif suite à modification dans la dernière version (`0.0.183`)

## 0.0.183 - 2025-10-23
### Ajouts
- `iam.RData` : création
- `topographie.iam` : création

## 0.0.182 - 2025-10-20
### Ajouts
- `chronique.classescalendaires` : création (en lien avec #72)
- `chronique.contexte` : ajout du renommage de la colonne de valeurs pour le cas de données agrégées afin de rendre possible le calcul des `valeur_min` et `valeur_max`
- `chronique.figure.interannuelle` : 
  * prise en compte possible de plusieurs stations
  * intégration de `chronique.contexte`
- `chronique.figure.pluviometrie` : création
- `chronique.ouverture` : ajout du format `typecapteur` `RBRsolo3`

### Modifications
- `chronique.figure.interannuelle` : refactoring
- `chronique.figure.parametres` : modification du `contextereference` afin qu'il intègre les champs `valeur_min` et `valeur_max`
  
### Corrections
- `chronique.figure.interannuelle` : correction d'une coquille dans les paramètres graphiques ajoutés en 0.0.182

## 0.0.181 - 2025-10-15
### Ajouts
- `topographie.correction.z` : création
- `scalimetrie.formatage.retrocalcul` : création
- `scalimetrie.import` : création
- `chronique.figure.interannuelle` : ajout des paramètres nitrates et conductivité
- `chronique.figure.parametres` : ajout des paramètres nitrates et conductivité

## 0.0.180 - 2025-10-07
### Ajouts
- `formatage.time` : ajout de cas de renommage en lien avec les champs de mesures physico-chimiques
- `PC.hubeau` : 
  * ajout d'un test de vérification si présence d'un contenu en retour
  * ajout d'une possibilité de récupérer tous les supports Sandre directement
  * ajout des paramètres `date_start` et `date_end`
- `PC.hubeau.operation.R` : création
- `now.ymd_hm` : création
- `now.ymd_hms` : création
- `poissons.plansdeau.ofbversteleos` : ajout d'un export d'une liste des actions de pêche sans capture (ou capture vide)
- `soleil.horaires` : création
- `topographie.profil` : 
  * ajout d'un exemple
  * ajout d'un test conditionnel en entrée
- `topographie.figure.profil.automatique` : ajout du paramètre `ligne_eau`
- `topographie.transect` : ajout d'un test conditionnel en entrée

### Modifications
- Retrait d'une dépendance
- `PC.hubeau` : refactoring pour passage de `httr` à `httr2`
- `SIG.export` : 
  * ajout d'un `overwrite = TRUE` manquant pour l'export excel
  * ajout d'un `append = FALSE` manquant pour l'export vers les fichiers SIG
- `topographie.figure.profil.automatique` : suppression d'une boucle instable et peu propre (refactoring)

### Suppressions
- `PC.hubeau` : suppression d'un test de vérification de paramètres sandre en entrée

## 0.0.179 - 2025-07-10
### Ajouts
- `formatage.time` : 
  * ajout du traitement de la colonne `chmes_referentiel_temporel`
  * ajout d'un test de vérification en sortie de traitement s'il reste des colonnes non re-traitées
- `hydrologie.hubeau` : 
  * ajout d'un renommage de champs pour les données élaborées
  * ajout d'un exemple

## 0.0.178 - 2025-06-25
### Ajouts
- `PC.lac.profil` : ajout d'un paramètre manquant dans la documentation

### Modifications
- `chronique.figure` : modification du style de `bw` vers `minimal`
- `chronique.figure.classescalendaires` : 
  * amélioration de l'affichage par période mensuelle
  * modification du style de `bw` vers `minimal`
- `chronique.figure.interannuelle` : 
  * modification du style de `bw` vers `minimal`
  * abréviations des mois en français sur les figures
  * affichage de tous les mois par des barres verticales
- `PC.lac.profil` : modification du style de `bw` vers `minimal`
- `SIG.flux.wfs` : retrait de la dépendance à `ows4R` qui était longue à charger et inutile

## 0.0.177 - 2025-06-23
### Ajouts
- `poissons.captures` : ajout du paramètre `profondeur_capture`
- `SIG.flux.wfs` : création
- `SIG.style.qgis` : création
  
### Modifications
- `hydrologie.hubeau` : correction du format de sortie en cas de retour vide, afin de ne pas bloquer la jointure dans un `map`
- `hydrologie.hubeau.stations` : correction du format de sortie en cas de retour vide, afin de ne pas bloquer la jointure dans un `map`

## 0.0.176 - 2025-06-10
### Ajouts
- `chronique.context` : Ajout du calcul de `valeur_min` et `valeur_max`, afin de pouvoir déduire le contexte pour les représentations graphiques
- `chronique.figure` : Abréviations des mois en français sur les figures
- `chronique.meteofrance.mesures` : 
  * ajout d'un test de vérification du format de station saisi
  * ajouts de tests unitaires
- `chronique.meteofrance.stations` : création
- `chronique.ouverture` : ajout de la création/ouverture de la colonne vide `chsta_codetigre1`
- `dev.fonctions.parametres` : création
- `hydrologie.hubeau.stations` : création
  
### Modifications
- `chronique.meteofrance.nettoyage` : modification du traitement des dates afin de gérer les dates uniquement ou bien dates et heures, en fonction du type de données

### Corrections
- `chronique.meteofrance.mesures` : correction d'une coquille dans la description
- `chronique.ouverture` : correction de création de colonnes vides TIGRE si nécessaire

## 0.0.175 - 2025-05-17
### Modifications
- MAJ de l'aide des jeux de données `chronique_exemple`, `chronique_structure` et `chronique_structure_hors_bdd`

### Corrections
- Correction de la `0.0.174` car champs `chmes_referentiel_temporel` mal ajouté dans les jeux de données de référence

## 0.0.174 - 2025-05-14
### Ajouts
- `BDD.ouverture` : ajout de la dépendance à `DBI` afin que le package soit chargé automatiquement
- `chronique.agregation` : ajout du calcul de `VSommeJ` et de `SommeSommeJ` pour les données de pluviométrie
- `chronique.ouverture` : 
  * Ajout de l'évaluation de champs qui manquait
  * Ajout de la gestion des champs :
    - `chmes_referentiel_temporel`
    - `chsta_codehydro`
    - `chsta_codemeteofrance`
    - `chsta_infl_ant_type`
    - `chsta_infl_nappe`

### Modifications
- `chronique.ouverture` : 
  * Remplacement de la structure de référence des capteurs et du suivi de terrain en dur dans le code par `capteurs_structure` et `suivis_structure`
- `PC.conductivite.25degres` : ajout d'un arrondi dans les valeurs de conductivité calculées
- Actualisation des jeux de données `chronique_exemple`, `chronique_structure` et `chronique_structure_hors_bdd` avec ajouts de champs :
  * `chmes_referentiel_temporel`
  * `chsta_codehydro`
  * `chsta_codemeteofrance`
  * `chsta_infl_ant_type`
  * `chsta_infl_nappe`
- Renommage dans jeux de données `chronique_exemple`, `chronique_structure` et `chronique_structure_hors_bdd` de `suivi_structure` par `suivis_structure` pour harmonisation

## 0.0.173 - 2025-04-23
### Ajouts
- `facteur_conversion_conductivite.RData` : création
- `PC.conductivite.25degres` : création
- `chronique.meteofrance.commande` : création
- `chronique.meteofrance.mesures` : création
- `chronique.meteofrance.nettoyage` : création

## 0.0.172 - 2025-04-22
### Ajouts
- `chronique.recalage.valeur` : ajout d'un ré-encodage du paramètre `decalage` en `NA` si la valeur envoyée vaut `0`, car sinon la fonction ne s'appliquait pas
- `formatage.heure.simple` : création
- `hydrologie.hubeau` : création
- `SIG.conversion.l93dec2` : création

### Modifications
- `chronique.ouverture` : 
  * correction des affectations de stations dans les modems VuLink, qui en réalité était un Cube
  * écriture en dur du nombre de lignes à sauter dans le traitement du VuLink
- `SIG.conversion.dec2l93` : nettoyage du code et de l'aide

## 0.0.171 - 2025-03-15
### Ajouts
- `chronique.ouverture` : ajout du traitement pour les données issues d'un modem In Situ Cube

### Corrections
- `chronique.ouverture` : 
  * correction du paramètre `separateur` en `separateur_colonnes` ce qui n'avait pas été réalisé partout à la version `0.0.168`
  * correction du traitement des données issues d'un `VuLink` car oubli de l'ordre des colonnes + distinction du capteur entre le modem et le capteur à proprement parler
  
## 0.0.170 - 2025-02-20
### Ajouts
- `chronique.rattachement.barometrie` : création à travers une réduction de `chronique.compensation.barometrie`
  
### Modifications
- `chronique.compensation.barometrie` :
  * extraction d'un bloc vers `chronique.rattachement.barometrie`
  * ajout d'une possibilité de compensation soit par un rattachement temporel via `chronique.rattachement.barometrie`, soit par interpolation linéaire via `zoo::na.approx`
  * MAJ des exemples
  * ajout d'un filtre manquant pour la modalité `compensé_avec_vide` qui générait de nombreuses lignes vides qui n'avaient pas lieu d'être

## 0.0.169 - 2025-02-19
### Ajouts
- `chronique.ouverture` : ajout du traitement des modems VuLink (travail de Malidé)

## 0.0.168 - 2025-02-18
### Ajouts
- `chronique.figure` : extension de la palette
- `chronique.ouverture` : 
  * ajout du paramètre `nomfichier`
  * ajout du paramètre `separateur_decimales`
  * ajout d'un choix automatique du type `baro` ou `piézo` si le mot clé est inclus dans le nom du fichier
  * ajout d'un test de vérification que le paramètre `Localisation` est bien unique (utile pour construction application shiny `othaup`)
- `hydrobio.hubeau` : ajout d'un paramètre `bbox`
- `PC.contexte` : calcul des dates différentes
- `PC.hubeau` : 
  * ajout d'un paramètre `bbox`
  * ajout d'un paramètre `communeinsee`
- `PC.lac.profil` : 
  * implémentation de `PC.contexte`
  * ajout du traitement de dates différentes pour une même année
  * forçage des traits verticaux pour les valeurs de saturation en O2 afin de rendre ça plus esthétique et lisible

### Modifications
- `chronique.ouverture` :
  * intégration du paramètre `skipvalue` dans l'import des données piézométriques Hobo
  * renommage du paramètre `separateur` en `separateur_colonnes` du fait de la création du paramètre `separateur_decimales`
- `PC.contexte` : calcul de l'année biologique à partir du 01-01, car plus logique de travailler en année civile. Il faudrait peut-être ajouter un paramètre de fonction à l'avenir si le cas inverse se présente
- `PC.hubeau` : refactoring de la construction de la requête, afin d'être plus souple, sur le modèle de `hydrobio.hubeau`

### Corrections
- `chronique.ouverture` : correction de l'utilisation de `adresse.switch` qui n'était pas fonctionnelle
- `PC.lac.profil` : 
  * Correction d'une erreur dans l'établissement de la palette
  * Ajout de la vérification de l'argument `couleurs` qui manquait

## 0.0.167 - 2025-02-06
### Ajouts
- `chronique.figure` : ajout du paramètre `Chlorophylle a`
- `chronique.figure.parametres` : ajout du paramètre `Chlorophylle a`
- `chronique.ouverture` : ajout de cas non traités à ce jour : 
  * WiSens chlorophylle a, 
  * absence de la colonne de concentration en O2 dissous avec les sondes WiSens, 
  * séparateurs différents pour les sondes WiSens
- `hydrobio.hubeau` : création
- `poissons.captures` : ajout d'une option d'extraction des observations

### Modifications
- `chronique.ouverture` : 
  * refactoring pour sondes WiSens
  * ajout d'une conversion de datatype pour l'importation de capteurs
- `poissons.captures` : refactoring des colonnes retenues

### Corrections
- `chronique.figure` : annulation de la francisation de la légende des mois (dont ajout de la dépendance) car fait planter RStudio (échappement de mémoire ?)
- `chronique.figure.classescalendaires` : annulation de la francisation de la légende des mois (dont ajout de la dépendance) car fait planter RStudio (échappement de mémoire ?)
- `chronique.figure.cumul` : annulation de la francisation de la légende des mois (dont ajout de la dépendance) car fait planter RStudio (échappement de mémoire ?)
- `chronique.figure.depassementscalendaires` :annulation de la francisation de la légende des mois (dont ajout de la dépendance) car fait planter RStudio (échappement de mémoire ?)
- `chronique.figure.interannuelle` : annulation de la francisation de la légende des mois (dont ajout de la dépendance) car fait planter RStudio (échappement de mémoire ?)

## 0.0.166 - 2025-01-15
### Ajouts
- `chronique.figure.interannuelle` : ajout d'un paramètre de choix de la palette, car il y a toujours un cas qui ne convient pas
- `formatage.annee.neutre` : ajout d'un dégroupage, car les groupes pouvaient générer des bugs et n'avaient pas lieu d'être
- `formatage.annee.biologique` : ajout d'un dégroupage, car les groupes pouvaient générer des bugs et n'avaient pas lieu d'être

### Modifications
- `chronique.figure` : francisation de la légende des mois (dont ajout de la dépendance)
- `chronique.figure.classescalendaires` : francisation de la légende des mois (dont ajout de la dépendance)
- `chronique.figure.cumul` : francisation de la légende des mois (dont ajout de la dépendance)
- `chronique.figure.depassementscalendaires` : francisation de la légende des mois (dont ajout de la dépendance)
- `chronique.figure.interannuelle` : refactoring :
  * Aération/nettoyage du code
  * Implémentation complète de `chronique.contexte` (close # 32)
  * Implémentation complète de `formatage.annee.neutre`
  * Francisation de la légende des mois (dont ajout de la dépendance)
  
## 0.0.165 - 2024-12-24
### Ajouts
- Ajout d'une `licence` et d'un `readme`
- Ajout de dépendances manquantes : `dygraphs`, `janitor`, `readODS`, `xts` (close #85)
- `SIG.conversion.dec2l93` : création
- `chronique.bruit.temps` : création
- `chronique.contexte` : ajout du calcul de `nmesure`
- `chronique.ouverture` : 
  * Ajout d'une bascule automatique si nécessaire de `chsta_codemo` vers `chsta_coderhj`
  * Ajout d'un test sur la longueur des commentaires importés, et remplacement si vide, afin d'obtenir les bons datatypes
- `formatage.time` : ajout de cas de traitement pour renommage en sortie
- `formatage.date.heure` : création - Permet de faire l'inverse de `formatage.time`

### Modifications
- `BDD.ouverture` : 
  * Complément avec machine professionnelle Malidé
  * Ajout d'une entrée pour le poste de JB suite à MAJ MacOS
- `chronique.cle` : modification de l'ordre des paramètres afin d'en faciliter l'utilisation
- `chronique.recalage.valeur` : reformatage
- `MI.SANDRE` : modification de la localisation des colonnes en début de jeu de données

### Corrections
- `chronique.figure.longitudinale` : suppression de la dépendance à `tcltk::tk_select.list`

## 0.0.164 - 2024-10-01
### Ajouts
- `chronique.ouverture` : 
  * Ajout d'un paramètre `separateur` de colonnes
  * Ajout du traitement des sondes WiSens DO
- `sapl.ouverture.adhesions` : création

### Modifications
- `chronique.ouverture` : Ajout d'un filtre afin de supprimer les lignes partiellement vides qui générent ensuite des warnings lors du parsing des dates/heures et qui empêchent le traitement du fichier

### Corrections
- Suppression de l'appel du package `babel` dans le fichier `ModeleRenduPeche.Rnw` qui ne fonctionne pas sur le poste d'Adrien
- Suppression de l'appel du package `babel` dans le fichier `ModeleRenduPecheCommente.Rnw` qui ne fonctionne pas sur le poste d'Adrien

## 0.0.163 - 2024-08-29
### Corrections
- `chronique.ouverture` : correctif 3 pour format de collecte heure lors de l'ouverture des données "Aquaread LeveLine 10m ABS" transmises par un modem "Aquaread AquaTel LeveLine"

## 0.0.162 - 2024-08-28
### Corrections
- `chronique.ouverture` : correctif 2 pour collecte nom de la station lors de l'ouverture des données "Aquaread LeveLine 10m ABS" transmises par un modem "Aquaread AquaTel LeveLine"

## 0.0.161 - 2024-08-28
### Ajouts
- `BDD.ouverture` : 
  * Ajout de la machine de Malidé dans une deuxième version
### Corrections
- `chronique.ouverture` : correctif pour utilisation plus générale de l'ouverture des données "Aquaread LeveLine 10m ABS" transmises par un modem "Aquaread AquaTel LeveLine"

## 0.0.160 - 2024-08-22
### Ajouts
- `chronique.ouverture` : ouverture des données "Aquaread LeveLine 10m ABS" transmises par un modem "Aquaread AquaTel LeveLine"

## 0.0.159 - 2024-06-03
### Ajouts
- `BDD.ouverture` : 
  * Ajout de la machine de Malidé
  * Retrait de la machine de Quentin

### Corrections
- `poissons.captures` : 
  * Refactoring du code de filtrage afin qu'il soit réalisé côté base de données
  * Ajout d'un test afin de savoir si le nom de station contient une apostrophe, ce qui posera problème en SQL

## 0.0.158 - 2024-05-23
### Ajouts
- `chronique.vue.dynamique` : création
- `formatage.time` : création
- `formatage.variables.renommage` : création
- `PC.variables.renommage` : création

### Corrections
- `chronique.resultats.periode` : harmonisation des noms de variables temporaires
- `topographie.figure.profil.automatique` : ajout d'un correctif permettant d'échapper l'affichage de la ligne d'eau s'il n'y a pas de mesure associée au niveau du transect

## 0.0.157 - 2024-05-02
### Ajouts
- `chronique.ouverture` : ajout de l'importation depuis des fichiers LibreOffice `.ods`
- `poissons.contexte` : création
- `poissons.cpue` : création
- `poissons.plansdeau.ofbversteleos` : 
  * ajout de la gestion des lots N et S/L concernant les effectifs totaux
  * ajout de l'utilisation de `formatage.noms_propres` afin de reformater automatiquement les noms de milieux incorrects
- `topographie.figure.profil` : ajout d'un paramètre de choix du nom du fichier enregistré
- `topographie.figure.profil.automatique` : création

### Modifications
- `chronique.ouverture` : modification du traitement pour prise en compte des colonnes `chmes_validation` et `chmes_mode_acquisition` si elles sont déjà présentes dans le jeu de données

### Corrections
- `topographie.profil` : correction d'un nom de variable fonctionnel avec le script historique, mais qui était faux (effet confondant)

## 0.0.156 - 2024-02-14
### Ajouts
- `PaletteAnnees.RData` : ajout d'années dans le jeu de données
- `palette_annees_aleatoires.RData` : création - permet d'avoir toujours la même couleur pour une année, mais sans que les années successives soient trop semblables
- `palette_annees_stratifie.RData` : création - permet d'avoir toujours la même couleur pour une année, mais avec des années successives plutôt coordonnées
- `PC.lac.profil` : ajout du paramètre `couleurs`

### Modifications
- `hydrovu.refactoring` : ajouts de cas complémentaires non identifiés précédemment
- `PC.lac.profil` : modification du traitement des dates afin qu'une même année soit toujours de la même couleur

## 0.0.155 - 2024-02-02
### Ajouts
- `formatage.heure.excel` : création
- `hydrovu.authentification` : création
- `hydrovu.donnees` : création
- `hydrovu.extraction` : création
- `hydrovu.refactoring` : création
- `hydrovu.sites` : création

## 0.0.154 - 2024-01-23
### Ajouts
- Utilisation de `usethis::use_lifecycle()` au sein du package
- `chronique.recalage.valeur` : création 

### Modifications
- `chronique.decalage` passée en `deprecated` suite à création de `chronique.recalage.temps`

### Corrections
- Fichiers d'aides automatiquement corrigés en markdown suite à utilisation de `usethis::use_lifecycle()`

## 0.0.153 - 2024-01-11
### Ajouts
- `topographie.profil` : création

### Modifications
- `topographie.figure.profil` : 
  * Changement du syle `theme_bw` vers `theme_minimal`
  * Modification des intitulés de variables
  * Mise en compatibilité avec `topographie.profil`
  * Suppression de l'extraction des données de profil en entrée, qu'il faudra envoyer de manière séparée afin de rendre les choses plus universelles
- `topographie.transect` : 
  * Explicitation de l'aide
  * Ajout de la gestion d'un éventuel identifiant de transect

## 0.0.152 - 2024-01-09
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

## 0.0.151 - 2024-01-09
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

## 0.0.150 - 2023-06-21
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

## 0.0.149 - 2023-02-28
### Ajouts
- `chronique.figure.classescalendaires` : ajout d'un test de vérification qu'on a bien des données journalières en entrée
- `PC.concentrationO2` : création
- `topographie.hypsometrie` : création

### Corrections
- `PC.saturationO2` : modification du mode de calcul car source initiale indisponible en ligne, remplacement par SANDRE (close #76)
- `poissons.plansdeau.ofbversteleos` : 
  * Ajout d'un ré-encodage des types de filets CEN et du test de bon traitement associé

## 0.0.148 - 2023-01-26
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

## 0.0.147 - 2022-12-21
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

## 0.0.146 - 2022-09-21
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

## 0.0.145 - 2022-08-23
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
