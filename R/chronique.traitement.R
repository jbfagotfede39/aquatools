#' Exécuter le traitement de données de chroniques
#'
#' Permet d'exécuter plusieurs opérations de traitement des données de chronique d'une station
#' @name chronique.traitement
#' @keywords chronique
#' @param data Data.frame issu de chronique.mesures, pouvant contenir différentes stations
#' @param export Si \code{TRUE} (par défault), exporte les résultats/figures.  Si \code{FALSE}, ne les exporte pas.
#' @param typemesure Défini le type de données et modifie le traitement en fonction
#' @param projet Nom du projet
#' @param dep39 Si \code{FALSE} (par défault), ne va pas rechercher les données de stations dans la base locale et donc export simplifié. Si \code{TRUE}, fait la jointure SIG.
#' @param archivage Si Aucun (par défault), ne va pas créer une archives .zip du répertoire de sortie. Si Partiel, créé une archive et conserve le répertoire. Si Complet, créé une archive et supprimer le répertoire.
#' @import dplyr
#' @import fs
#' @import lubridate 
#' @import openxlsx
#' @import purrr
#' @import sf
#' @import stringr
#' @import tidyverse
#' @import zip
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
  typemesure = c("Thermie", "Thermie barométrique", "Thermie piézométrique", "Barométrie", "Piézométrie", "Piézométrie brute", "Piézométrie compensée", "Oxygénation", "Hydrologie", "Pluviométrie"),
  dep39 = F,
  archivage = c("Aucun","Partiel","Complet") 
  )
{

##### -------------- A FAIRE -------------- #####
# Export lexique : le faire depuis un fichier contenu dans le package
# Export fichier readme qui explique le fonctionnement + auteur + date + noms/versions (session_info) des packages 
# Essayer de supprimer le warning qui apparaît suite à # Analyse des données # (In bind_rows_(x, .id) : Unequal factor levels: coercing to character)
# -------------- A FAIRE -------------- #  

#### Évaluation des choix ####
  typemesure <- match.arg(typemesure)
  archivage <- match.arg(archivage)

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
  dir.create(paste0("./",projet, "/Sorties/Vues/absolu-fixé/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Sorties/Vues/absolu-libre/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Sorties/Vues/relatif-fixé/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Sorties/Vues/relatif-libre/"), showWarnings = FALSE, recursive = FALSE)
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
  dir.create(paste0("./",projet, "/Sorties/Vues/absolu-fixé/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Sorties/Vues/absolu-libre/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Sorties/Vues/relatif-fixé/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Sorties/Vues/relatif-libre/"), showWarnings = FALSE, recursive = FALSE)
}
}
  
if(export == TRUE & dep39 == FALSE & file.exists(paste0("./",projet, "/Entrées/")) == FALSE){
  dir.create(paste0("./",projet, "/Entrées/"), showWarnings = FALSE, recursive = FALSE)
}

#### Préparation des données ####
data <-
  data %>% 
  formatage.annee.biologique() # Calcul de l'année biologique

#### Nettoyage des données ####
if("chmes_typemesure" %in% colnames(data) == FALSE){
  data <- 
    data %>% 
    group_by(chmes_coderhj, chmes_anneebiol) %>% 
    filter(n_distinct(chmes_date) > 30) %>% # Pour supprimer les années biologiques avec moins de 30 dates différentes
    ungroup()
}

if("chmes_typemesure" %in% colnames(data) == TRUE){
data <- 
  data %>% 
  group_by(chmes_coderhj, chmes_anneebiol,chmes_typemesure) %>% 
  filter(n_distinct(chmes_date) > 30) %>% # Pour supprimer les années biologiques avec moins de 30 dates différentes
  ungroup()
}
  
#### Analyse des données ####
DataTravail <- 
  data %>%
  group_split(chmes_coderhj, chmes_anneebiol,chmes_typemesure) %>% 
    purrr::map_dfr(~ chronique.analyse(.)) %>% 
    ungroup()
# ça pourra crasher par ici lorsqu'on fera un essai mixant chmes_typemesure == "Thermie" avec un autre chmes_typemesure à cause de la jointure à réaliser et un nb de champ différent (absence de TRF$DateDebutDegresJours et la suite)

##### Sorties graphiques #####
### Chronique complète ###
## Type de mesures spécifié ##
if(all(export & "chmes_typemesure" %in% colnames(data)) == TRUE){
# Y libre sans vmm30j
  data %>%
  group_split(chmes_coderhj, chmes_anneebiol,chmes_typemesure) %>% 
  purrr::map_dfr(~ chronique.figure(data = ., Titre = as.character(paste0(unique(.$chmes_coderhj)," - ",unique(.$chmes_anneebiol))), duree = "Complet", typemesure = "Thermie", complement = TRUE, Ymin=NA, Ymax=NA, save=T, projet = projet, format=".png"))
# Y libre avec vmm30j
  data %>%
    group_split(chmes_coderhj, chmes_anneebiol,chmes_typemesure) %>% 
    purrr::map_dfr(~ chronique.figure(data = ., Titre = as.character(paste0(unique(.$chmes_coderhj)," - ",unique(.$chmes_anneebiol))), duree = "Complet", typemesure = "Thermie", complement = TRUE, Ymin=NA, Ymax=NA, Vmm30j=T, save=T, projet = projet, format=".png"))
# Y fixé sans vmm30j
  data %>%
    group_split(chmes_coderhj, chmes_anneebiol,chmes_typemesure) %>% 
    purrr::map_dfr(~ chronique.figure(data = ., Titre = as.character(paste0(unique(.$chmes_coderhj)," - ",unique(.$chmes_anneebiol))), duree = "Complet", typemesure = "Thermie", complement = TRUE, Ymin=-1, Ymax=30, save=T, projet = projet, format=".png"))
# Y fixé avec vmm30j
  data %>%
    group_split(chmes_coderhj, chmes_anneebiol,chmes_typemesure) %>% 
    purrr::map_dfr(~ chronique.figure(data = ., Titre = as.character(paste0(unique(.$chmes_coderhj)," - ",unique(.$chmes_anneebiol))), duree = "Complet", typemesure = "Thermie", complement = TRUE, Ymin=-1, Ymax=30, Vmm30j=T, save=T, projet = projet, format=".png"))
  }

## Type de mesures non spécifié ##
if(all(export & "chmes_typemesure" %in% colnames(data)) != TRUE){
  warning("Vérification nécessaire car plusieurs typemesure donc ce paramètre n'est pas pris en compte dans les sorties graphiques")
# Y libre sans vmm30j
  data %>%
    group_split(chmes_coderhj, chmes_anneebiol) %>%
    purrr::map_dfr(~ chronique.figure(data = ., Titre = as.character(paste0(unique(unlist(.$chmes_coderhj))," - ",unique(unlist(.$chmes_anneebiol)))), duree = "Complet", typemesure = "Thermie", Ymin=NA, Ymax=NA, save=T, projet = projet, format=".png")) # Fonctionne si plusieurs années
# Y libre avec vmm30j
  data %>%
    group_split(chmes_coderhj, chmes_anneebiol) %>%
    purrr::map_dfr(~ chronique.figure(data = ., Titre = as.character(paste0(unique(unlist(.$chmes_coderhj))," - ",unique(unlist(.$chmes_anneebiol)))), duree = "Complet", typemesure = "Thermie", Ymin=NA, Ymax=NA, Vmm30j=T, save=T, projet = projet, format=".png")) # Fonctionne si plusieurs années
# Y fixé sans vmm30j
  data %>%
    group_split(chmes_coderhj, chmes_anneebiol) %>%
    purrr::map_dfr(~ chronique.figure(data = ., Titre = as.character(paste0(unique(unlist(.$chmes_coderhj))," - ",unique(unlist(.$chmes_anneebiol)))), duree = "Complet", typemesure = "Thermie", Ymin=-1, Ymax=30, save=T, projet = projet, format=".png")) # Fonctionne si plusieurs années
# Y fixé avec vmm30j
  data %>%
    group_split(chmes_coderhj, chmes_anneebiol) %>%
    purrr::map_dfr(~ chronique.figure(data = ., Titre = as.character(paste0(unique(unlist(.$chmes_coderhj))," - ",unique(unlist(.$chmes_anneebiol)))), duree = "Complet", typemesure = "Thermie", Ymin=-1, Ymax=30, Vmm30j=T, save=T, projet = projet, format=".png")) # Fonctionne si plusieurs années
}

### Chronique incomplète ###
## Type de mesures spécifié ##
if(all(export & "chmes_typemesure" %in% colnames(data)) == TRUE){
# Y libre sans vmm30j
data %>%
    group_split(chmes_coderhj, chmes_anneebiol, chmes_typemesure) %>%
    purrr::map_dfr(~ chronique.figure(data = ., Titre = as.character(paste0(unique(unlist(.$chmes_coderhj))," - ",unique(unlist(.$chmes_anneebiol)))), duree = "Relatif", typemesure = "Thermie", Ymin=NA, Ymax=NA, save=T, projet = projet, format=".png")) # Fonctionne si plusieurs années
# Y libre avec vmm30j
  data %>%
    group_split(chmes_coderhj, chmes_anneebiol, chmes_typemesure) %>%
    purrr::map_dfr(~ chronique.figure(data = ., Titre = as.character(paste0(unique(unlist(.$chmes_coderhj))," - ",unique(unlist(.$chmes_anneebiol)))), duree = "Relatif", typemesure = "Thermie", Ymin=NA, Ymax=NA, Vmm30j=T, save=T, projet = projet, format=".png")) # Fonctionne si plusieurs années
# Y fixé sans vmm30j
  data %>%
    group_split(chmes_coderhj, chmes_anneebiol, chmes_typemesure) %>%
    purrr::map_dfr(~ chronique.figure(data = ., Titre = as.character(paste0(unique(unlist(.$chmes_coderhj))," - ",unique(unlist(.$chmes_anneebiol)))), duree = "Relatif", typemesure = "Thermie", Ymin=-1, Ymax=30, save=T, projet = projet, format=".png")) # Fonctionne si plusieurs années
# Y fixé avec vmm30j
  data %>%
    group_split(chmes_coderhj, chmes_anneebiol, chmes_typemesure) %>%
    purrr::map_dfr(~ chronique.figure(data = ., Titre = as.character(paste0(unique(unlist(.$chmes_coderhj))," - ",unique(unlist(.$chmes_anneebiol)))), duree = "Relatif", typemesure = "Thermie", Ymin=-1, Ymax=30, Vmm30j=T, save=T, projet = projet, format=".png")) # Fonctionne si plusieurs années
}

## Type de mesures non spécifié ##
if(all(export & "chmes_typemesure" %in% colnames(data)) != TRUE){
  warning("Vérification nécessaire car plusieurs typemesure donc ce paramètre n'est pas pris en compte dans les sorties graphiques")
# Y libre sans vmm30j
  data %>%
    group_split(chmes_coderhj, chmes_anneebiol) %>%
    purrr::map_dfr(~ chronique.figure(data = ., Titre = as.character(paste0(unique(unlist(.$chmes_coderhj))," - ",unique(unlist(.$chmes_anneebiol)))), duree = "Relatif", typemesure = "Thermie", Ymin=NA, Ymax=NA, save=T, projet = projet, format=".png")) # Fonctionne si plusieurs années
# Y libre avec vmm30j
  data %>%
    group_split(chmes_coderhj, chmes_anneebiol) %>%
    purrr::map_dfr(~ chronique.figure(data = ., Titre = as.character(paste0(unique(unlist(.$chmes_coderhj))," - ",unique(unlist(.$chmes_anneebiol)))), duree = "Relatif", typemesure = "Thermie", Ymin=NA, Ymax=NA, Vmm30j=T, save=T, projet = projet, format=".png")) # Fonctionne si plusieurs années
# Y fixé sans vmm30j
  data %>%
    group_split(chmes_coderhj, chmes_anneebiol) %>%
    purrr::map_dfr(~ chronique.figure(data = ., Titre = as.character(paste0(unique(unlist(.$chmes_coderhj))," - ",unique(unlist(.$chmes_anneebiol)))), duree = "Relatif", typemesure = "Thermie", Ymin=-1, Ymax=30, save=T, projet = projet, format=".png")) # Fonctionne si plusieurs années
# Y fixé avec vmm30j
  data %>%
    group_split(chmes_coderhj, chmes_anneebiol) %>%
    purrr::map_dfr(~ chronique.figure(data = ., Titre = as.character(paste0(unique(unlist(.$chmes_coderhj))," - ",unique(unlist(.$chmes_anneebiol)))), duree = "Relatif", typemesure = "Thermie", Ymin=-1, Ymax=30, Vmm30j=T, save=T, projet = projet, format=".png")) # Fonctionne si plusieurs années
}

##### Sortie stations #####
if(export == TRUE & dep39 == TRUE){
listeStations <- data %>% distinct(chmes_coderhj)

## Connexion à la BDD ##
dbD <- BDD.ouverture("Data")
listeStations <- sf::st_read(dbD, query = "SELECT * FROM fd_production.chroniques_stations;") %>% filter(chsta_coderhj %in% listeStations$chmes_coderhj) %>% collect() %>% select(chsta_coderhj:chsta_departement, chsta_coord_x:chsta_coord_type, chsta_fonctionnement:chsta_reseauthermietype)

SIG.export(listeStations, paste0("./",projet, "/Sorties/Stations/", format(now(), format="%Y-%m-%d"), "_Stations"))
}

##### Sortie résultats élaborés #####
if(export == TRUE & dep39 == TRUE){
DataTravailSIG <- listeStations %>% left_join(DataTravail %>% mutate(intervalMax = as.numeric(sub(",", ".", intervalMax))), by = c("chsta_coderhj" = "chmes_coderhj"))
SIG.export(DataTravailSIG, paste0("./",projet, "/Sorties/Résultats/", format(now(), format="%Y-%m-%d"), "_Résultats"))
}

if(export == TRUE & dep39 == FALSE){
  DataTravail %>% 
    openxlsx::write.xlsx(paste0("./",projet, "/Sorties/Résultats/", format(now(), format="%Y-%m-%d"), "_Résultats.xlsx"), sheetName = "Feuille1", row.names = F, showNA = F)
}

##### Sorties agrégées ##### 
if(export == TRUE){
  data %>%
    group_split(chmes_coderhj) %>% # Permet d'éclater le dataframe en x dataframe, x étant le nb de modalités de chmes_coderhj
    #purrr::map_dfr(~ chronique.agregation(data = ., projet = projet, export = T)) # Fonctionne mais génère un message d'erreur : qqsoit le nb de chmes_coderhj : l'erreur intervient toujours après que le dernier fichier ait été généré (qqsoit le fichier) et arrête donc la fonction chronique.traitement
    purrr::map(~ chronique.agregation(data = ., projet = projet, export = T))
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

#### Notices ####
if(export == TRUE){
tbl(dbD, in_schema("fd_referentiels", "dictionnaire_glossaire")) %>% 
  filter(dicglo_type == "Chronique" | dicglo_codeunique == "Typemesure" | dicglo_codeunique == "Coderhj" | dicglo_codeunique == "Annee") %>% 
  collect() %>% 
  select(dicglo_acronyme, dicglo_nomcomplet, dicglo_definition) %>% 
  rename(Abréviation = dicglo_acronyme,
         Définition = dicglo_nomcomplet,
         Complément = dicglo_definition
         ) %>% 
  openxlsx::write.xlsx(paste0("./",projet, "/Sorties/Glossaire.xlsx"), sheetName = "Glossaire", row.names = F, showNA = F)
}
  
#### Informations de session ####
  if(export == TRUE){
Session <- devtools::session_info()

write(paste0("Généré le ", now(), " avec le package Aquatools (https://github.com/jbfagotfede39/aquatools/). \n"),file=paste0("./",projet, "/Sorties/session_info.txt"))
write(capture.output(Session),file=paste0("./",projet, "/Sorties/session_info.txt"),append=TRUE)
}
  
##### Zippage #####
if(archivage != "Aucun"){
  zip::zipr(zipfile = paste0(projet,".zip"), files = paste0("./",projet))
}
if(archivage == "Complet"){
  fs::dir_delete(paste0("./",projet))
}

return(DataTravail)

} # Fin de la fonction
