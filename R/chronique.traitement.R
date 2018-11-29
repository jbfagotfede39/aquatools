#' Exécuter le traitement de données de chroniques
#'
#' Permet d'exécuter plusieurs opérations de traitement des données de chronique d'une station
#' @name chronique.traitement
#' @keywords chronique
#' @param data Data.frame issu de chronique.mesures, pouvant contenir différentes stations
#' @param export Si \code{TRUE} (par défault), exporte les résultats/figures.  Si \code{FALSE}, ne les exporte pas.
#' @param typemesure Défini le type de données et modifie le traitement en fonction
#' @param projet Nom du projet
#' @import dplyr
#' @import lubridate 
#' @import sf
#' @import stringr
#' @import tidyverse
#' @export
#' @examples
#' chronique.traitement(data)
#' chronique.traitement(data, export = F)
#' DataTravail <- chronique.mesures("SUR0-9", "Thermie") %>% chronique.traitement()
#' DataTravail <- chronique.mesures("SUR0-9", "Thermie") %>% chronique.traitement(., export = F)
#' DataTravail <- tbl(db,"Mesures") %>% filter(CodeRDT == "DOU393-2" | CodeRDT == "DOU394-5") %>% collect() %>% chronique.traitement()

chronique.traitement <- function(  
  data = data,
  projet = as.character(NA),
  export = T,
  typemesure = c("Thermie", "Thermie barométrique", "Thermie piézométrique", "Barométrie", "Piézométrie", "Piézométrie brute", "Piézométrie compensée", "Oxygénation", "Hydrologie", "Pluviométrie")
  )
{

##### -------------- A FAIRE -------------- #####
# Export lexique
# Export fichier readme qui explique le fonctionnement + auteur + date + noms/versions (session_info) des packages 
# Essayer de supprimer le warning qui apparaît suite à # Analyse des données # (In bind_rows_(x, .id) : Unequal factor levels: coercing to character)
# -------------- A FAIRE -------------- #  

#### Évaluation des choix ####
  typemesure <- match.arg(typemesure)

#### Vérification des répertoires ####
if(export == TRUE){
  if(file.exists(paste0("./",projet)) == FALSE){
    dir.create(paste0("./",projet), showWarnings = FALSE, recursive = FALSE)
  }
  if(file.exists(paste0("./",projet, "/Sorties/")) == FALSE){
  dir.create(paste0("./",projet, "/Sorties/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Sorties/Données/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Sorties/Résultats/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Sorties/Stations/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Sorties/Vues/"), showWarnings = FALSE, recursive = FALSE)
}

if(file.exists(paste0("./",projet, "/Sorties/")) == TRUE & file.exists(paste0("./",projet, "/Sorties/Données/")) == FALSE){
    dir.create(paste0("./",projet, "/Sorties/Données/"), showWarnings = FALSE, recursive = FALSE)
}

if(file.exists(paste0("./",projet, "/Sorties/")) == TRUE & file.exists(paste0("./",projet, "/Sorties/Résultats")) == FALSE){
  dir.create(paste0("./",projet, "/Sorties/Résultats/"), showWarnings = FALSE, recursive = FALSE)
}
  
  if(file.exists(paste0("./",projet, "/Sorties/")) == TRUE & file.exists(paste0("./",projet, "/Sorties/Stations")) == FALSE){
    dir.create(paste0("./",projet, "/Sorties/Stations/"), showWarnings = FALSE, recursive = FALSE)
  }
  
if(file.exists(paste0("./",projet, "/Sorties/")) == TRUE & file.exists(paste0("./",projet, "/Sorties/Vues/")) == FALSE){
  dir.create(paste0("./",projet, "/Sorties/Vues/"), showWarnings = FALSE, recursive = FALSE)
}
}

#### Préparation des données ####
data <-
  data %>% 
  formatage.annee.biologique() # Calcul de l'année biologique

#### Analyse des données ####
if("chmes_typemesure" %in% colnames(data) == FALSE){
  DataTravail <- 
    data %>% 
    #group_by(chmes_anneebiol) %>% # Pour supprimer les années biologiques avec moins de 30 dates différentes
    #filter(n_distinct(chmes_date) > 30) %>% # Pour supprimer les années biologiques avec moins de 30 dates différentes
    #ungroup() %>% 
    group_by(chmes_coderhj, chmes_anneebiol) %>% 
    filter(n_distinct(chmes_date) > 30) %>% # Pour supprimer les années biologiques avec moins de 30 dates différentes
    do(chronique.analyse(data = .)) %>% # applique la fonction à chaque station pour chaque année
    ungroup()
}

if("chmes_typemesure" %in% colnames(data) == TRUE){
DataTravail <- 
  data %>% 
  #group_by(chmes_anneebiol) %>% # Pour supprimer les années biologiques avec moins de 30 dates différentes
  #filter(n_distinct(chmes_date) > 30) %>% # Pour supprimer les années biologiques avec moins de 30 dates différentes
  #ungroup() %>% 
  group_by(chmes_coderhj, chmes_anneebiol,chmes_typemesure) %>% 
  filter(n_distinct(chmes_date) > 30) %>% # Pour supprimer les années biologiques avec moins de 30 dates différentes
  do(chronique.analyse(data = .)) %>% # applique la fonction à chaque station pour chaque année
  ungroup()
}
# ça pourra crasher par ici lorsqu'on fera un essai mixant chmes_typemesure == "Thermie" avec un autre chmes_typemesure à cause de la jointure à réaliser et un nb de champ différent (absence de DegresJoursTRF$DateDebutDegresJours et la suite)

##### Sorties graphiques #####
## Chronique complète ##
if(all(export & "chmes_typemesure" %in% colnames(data)) == TRUE){
data %>%
  group_by(chmes_coderhj, chmes_anneebiol, chmes_typemesure) %>%
  do({chronique.figure(data = ., Titre = as.character(paste0(unique(unlist(.$chmes_coderhj))," - ",unique(unlist(.$chmes_anneebiol)))), duree = "Complet", typemesure = "Thermie", save=T, projet = projet, format=".png") # Fonctionne si plusieurs années
    distinct(., chmes_coderhj, chmes_anneebiol) # Pour permettre à la fonction do de sortir un dataframe, sinon erreur
    }) %>%
  ungroup()
}
if(all(export & "chmes_typemesure" %in% colnames(data)) != TRUE){
  warning("Vérification nécessaire")
  data %>%
    group_by(chmes_coderhj, chmes_anneebiol) %>%
    do({chronique.figure(data = ., Titre = as.character(paste0(unique(unlist(.$chmes_coderhj))," - ",unique(unlist(.$chmes_anneebiol)))), duree = "Complet", typemesure = "Thermie", save=T, projet = projet, format=".png") # Fonctionne si plusieurs années
      distinct(., chmes_coderhj, chmes_anneebiol) # Pour permettre à la fonction do de sortir un dataframe, sinon erreur
    }) %>%
    ungroup()
}

## Chronique incomplète ##
if(all(export & "chmes_typemesure" %in% colnames(data)) == TRUE){
data %>%
  group_by(chmes_coderhj, chmes_anneebiol, chmes_typemesure) %>%
  do({chronique.figure(data = ., Titre = as.character(paste0(unique(unlist(.$chmes_coderhj))," - ",unique(unlist(.$chmes_anneebiol)))), duree = "Relatif", typemesure = "Thermie", save=T, projet = projet, format=".png") # Fonctionne si plusieurs années
    distinct(., chmes_coderhj, chmes_anneebiol) # Pour permettre à la fonction do de sortir un dataframe, sinon erreur
    }) %>%
  ungroup()
}

if(all(export & "chmes_typemesure" %in% colnames(data)) != TRUE){
  warning("Vérification nécessaire")
  data %>%
    group_by(chmes_coderhj, chmes_anneebiol) %>%
    do({chronique.figure(data = ., Titre = as.character(paste0(unique(unlist(.$chmes_coderhj))," - ",unique(unlist(.$chmes_anneebiol)))), duree = "Relatif", typemesure = "Thermie", save=T, projet = projet, format=".png") # Fonctionne si plusieurs années
      distinct(., chmes_coderhj, chmes_anneebiol) # Pour permettre à la fonction do de sortir un dataframe, sinon erreur
    }) %>%
    ungroup()
}

##### Sorties agrégées #####
if(export == TRUE){
  data %>%
  group_by(chmes_coderhj) %>% 
  do({chronique.agregation(data = ., projet = projet, export = T)
    distinct(., chmes_coderhj) # Pour permettre à la fonction do de sortir un dataframe, sinon erreur
    }) %>% # applique la fonction à chaque station pour chaque année
  #purrr::map(chronique.agregation(data = ., export = T)) # applique la fonction à chaque station pour chaque année
  ungroup()
}

if(export == FALSE){
# Soit adapter le code du export = TRUE pour que les sorties directes fonctionnent, 
  # # Exemple d'essai 
  # data %>% 
  #   group_by(chmes_coderhj) %>% 
  #   nest() %>% 
  #   purrr::map(chronique.agregation(data = ., export = F))
# soit faire une agrégation batarde comme celle dans le rapport de Vogna 2018 avec des union, mais moins propre
}

##### Sortie stations #####
if(export == TRUE){
listeStations <- data %>% distinct(chmes_coderhj)

## Connexion à la BDD ##
dbD <- BDD.ouverture("Data")
listeStations <- sf::st_read(dbD, query = "SELECT * FROM fd_production.chroniques_stations;") %>% filter(chsta_coderhj %in% listeStations$chmes_coderhj) %>% collect() %>% select(chsta_coderhj:chsta_departement, chsta_coord_x:chsta_coord_type, chsta_fonctionnement:chsta_reseauthermietype)

SIG.export(listeStations, paste0("./",projet, "/Sorties/Stations/", format(now(), format="%Y-%m-%d"), "_Stations"))
}

##### Sortie résultats élaborés #####
if(export == TRUE){
DataTravailSIG <- listeStations %>% left_join(DataTravail %>% mutate(intervalMax = as.numeric(sub(",", ".", intervalMax))), by = c("chsta_coderhj" = "chmes_coderhj"))
SIG.export(DataTravailSIG, paste0("./",projet, "/Sorties/Résultats/", format(now(), format="%Y-%m-%d"), "_Résultats"))
}

##### Zippage #####
if(export == TRUE){
zip(zipfile = projet, files = paste0("./",projet))
}

return(DataTravail)

} # Fin de la fonction
