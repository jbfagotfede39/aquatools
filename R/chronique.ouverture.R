#' Ouverture de fichiers de chroniques
#'
#' Cette fonction permet d'ouvrir de manière semi-automatisée des fichiers de chroniques
#' @name chronique.ouverture
#' @param Type Type de données en entrée (Mesures, Suivis, Stations)
#' @param typemesure Défini le type de données (Thermie, Piézométrie, etc.)
#' @param Localisation Localisation relative du fichier (à partir de /NAS-DATA/)
#' @param skipvalue Nombre de lignes à sauter en début de fichier (1 par défaut pour les mesures)
#' @param typedate Format des dates pour les mesures (ymd par défaut, dmy, mdy, dmy_hms)
#' @keywords chronique
#' @import lubridate
#' @import tidyverse
#' @export
#' @examples
#' DataToAdd <- chronique.ouverture("Mesures", "Thermie", "/NAS-DATA/Chroniques/Saisie_JB/2018-12-17_Suivi_Ain_amont_FJPPMA_été_2018/10880567.txt")
#' DataToAdd <- chronique.ouverture("Stations", "Thermie", "/NAS-DATA/Chroniques/Saisie_JB/2019-01-11_PDPG/Stations sondesPDPG.xlsx")
#' chronique.ouverture("Suivi", "Thermie", "/NAS-DATA/Chroniques/Saisie_JB/2019-01-18_PDPG/Saisie_cahier_terrain_PDPG_V2.xlsx")

chronique.ouverture <- function(
  Type = c("Mesures", "Suivis", "Stations"),
  typemesure = c("Thermie", "Thermie barométrique", "Thermie piézométrique", "Barométrie", "Piézométrie", "Piézométrie brute", "Piézométrie compensée", "Oxygénation", "Hydrologie", "Pluviométrie"),
  Localisation = as.character(NA),
  skipvalue = 1,
  typedate = "ymd"
)
{
  
##### -------------- A FAIRE -------------- #####
# Essayer de supprimer l'affichage des tests d'exécution dans le cas des mesures.
# Remettre le deuxième match.arg : typemesure <- match.arg(typemesure)
# -------------- A FAIRE -------------- #

#### Évaluation des choix ####
Type <- match.arg(Type)
#typemesure <- match.arg(typemesure) # car deux match.arg semblent mettre le micmac

#### Mesures ####
if(Type == "Mesures"){
## Chargement des données ##
# if(testit::has_warning(read_delim(adresse.switch(Localisation), skip = 2, col_names = c("Date","Heure", "Valeur","b"), delim=";")) == FALSE) dataaimporter <- read_delim(adresse.switch(Localisation), skip = 2, col_names = c("Date","Heure", "Valeur","b"), delim=";")
# if(exists("dataaimporter") == FALSE){
#   if(testit::has_warning(read_delim(adresse.switch(Localisation), skip = 2, col_names = c("Date","Heure", "Valeur","b", "c"), delim=";")) == FALSE) dataaimporter <- read_delim(adresse.switch(Localisation), skip = 2, col_names = c("Date","Heure", "Valeur","b", "c"), delim=";")}
# if(exists("dataaimporter") == FALSE){
#   if(testit::has_warning(read_delim(adresse.switch(Localisation), skip = 2, col_names = c("Date","Heure", "Valeur","b", "c", "d"), delim=";")) == FALSE) dataaimporter <- read_delim(adresse.switch(Localisation), skip = 2, col_names = c("Date","Heure", "Valeur","b", "c", "d"), delim=";")}
# if(exists("dataaimporter") == FALSE){
#   if(testit::has_warning(read_delim(adresse.switch(Localisation), skip = 2, col_names = c("Date","Heure", "Valeur","b", "c", "d", "e"), delim=";")) == FALSE) dataaimporter <- read_delim(adresse.switch(Localisation), skip = 2, col_names = c("Date","Heure", "Valeur","b", "c", "d", "e"), delim=";")}
# if(exists("dataaimporter") == FALSE){
#   if(testit::has_warning(read_delim(adresse.switch(Localisation), skip = 2, col_names = c("Date","Heure", "Valeur","b", "c", "d", "e", "f"), delim=";")) == FALSE) dataaimporter <- read_delim(adresse.switch(Localisation), skip = 2, col_names = c("Date","Heure", "Valeur","b", "c", "d", "e", "f"), delim=";")}

  # Suppression de la partie antérieure suite à la version 1.3.1 de readr qui affiche en warnin les erreurs de parsing
dataaimporter <- read_delim(adresse.switch(Localisation), skip = skipvalue, delim=";", col_types = "ctc")
names(dataaimporter)[1] <- c('Date')
names(dataaimporter)[2] <- c('Heure')
names(dataaimporter)[3] <- c('Valeur')

if(exists("dataaimporter") == FALSE) stop("Scénario d'importation à développer")

## Nettoyage ##
dataaimporter <-
  dataaimporter %>% 
  select(Date, Heure, Valeur)

if(testit::has_warning(ymd(dataaimporter$Date)) == FALSE & typedate == "ymd") dataaimporter$Date <- ymd(dataaimporter$Date)
if(testit::has_warning(dmy(dataaimporter$Date)) == FALSE & typedate == "dmy") dataaimporter$Date <- dmy(dataaimporter$Date)
if(testit::has_warning(mdy(dataaimporter$Date)) == FALSE & typedate == "mdy") dataaimporter$Date <- mdy(dataaimporter$Date)
#if(testit::has_warning(ymd_hms(dataaimporter$Date)) == FALSE) dataaimporter$Date <- ymd_hms(dataaimporter$Date)
if(testit::has_warning(dmy_hms(dataaimporter$Date)) == FALSE & typedate == "dmy_hms") dataaimporter$Date <- dmy_hms(dataaimporter$Date)

dataaimporter <-
  dataaimporter %>% 
  mutate(Date = format(Date, format="%Y-%m-%d")) %>% 
  mutate(Date = ymd(Date)) %>% 
  mutate(Heure = as.character(Heure)) %>% 
  mutate(Valeur = str_replace(Valeur, " °C", "")) %>% 
  mutate(Valeur = str_replace(Valeur, "°C", "")) %>% 
  mutate(Valeur = as.numeric(sub(",", ".", Valeur))) %>% 
  mutate(Valeur = round(Valeur,3)) %>% 
  filter(is.na(Valeur) != T)
  
## Transformation des champs ##
dataaimporter <- 
  dataaimporter %>% 
  rename_all(list(~stringi::stri_trans_general(., "latin-ascii"))) %>% # Pour remplacer les caractères accentués par les mêmes sans accents
  rename_all(list(~paste0("chmes_",.))) %>% 
  rename_all(list(~gsub("[[:punct:]]", "_", .))) %>% 
  rename_all(list(~tolower(.)))
}

#### Suivis ####
if(Type == "Suivis"){
## Chargement des données ##
dbD <- BDD.ouverture("Data")
SuiviTerrain <- tbl(dbD, in_schema("fd_production", "chroniques_suiviterrain")) %>% collect(n = 2) %>% arrange(chsvi_coderhj)

dataaimporter <- read_excel(adresse.switch(Localisation), sheet = 1)

## Renommage des champs ##
dataaimporter <- 
  dataaimporter %>% 
  rename_at(vars(matches("Valeur manuelle")), funs(str_replace(., "Valeur manuelle", "chsvi_valeur"))) %>%
  rename_at(vars(matches("Valeur")), funs(str_replace(., "Valeur", "chsvi_valeur"))) %>%
  rename_at(vars(matches("Tmanuelle")), funs(str_replace(., "Tmanuelle", "chsvi_valeur"))) %>%
  rename_at(vars(matches("MO")), funs(str_replace(., "MO", "chsvi_mo"))) %>%
  rename_at(vars(matches("Operateurs")), funs(str_replace(., "Operateurs", "chsvi_operateurs"))) %>%
  rename_at(vars(matches("Opérateurs")), funs(str_replace(., "Opérateurs", "chsvi_operateurs"))) %>%
  rename_at(vars(matches("CodeRDT")), funs(str_replace(., "CodeRDT", "chsvi_coderhj"))) %>%
  rename_at(vars(matches("Station")), funs(str_replace(., "Station", "chsvi_coderhj"))) %>%
  rename_at(vars(matches("TypeSuivi")), funs(str_replace(., "TypeSuivi", "chsvi_typesuivi"))) %>%
  rename_at(vars(matches("Date")), funs(str_replace(., "Date", "chsvi_date"))) %>%
  rename_at(vars(matches("Heure")), funs(str_replace(., "Heure", "chsvi_heure"))) %>%
  rename_at(vars(matches("Capteur")), funs(str_replace(., "Capteur", "chsvi_capteur"))) %>%
  rename_at(vars(matches("Unite")), funs(str_replace(., "Unite", "chsvi_unite"))) %>%
  rename_at(vars(matches("Action")), funs(str_replace(., "Action", "chsvi_action"))) %>%
  rename_at(vars(matches("Fonctionnement")), funs(str_replace(., "Fonctionnement", "chsvi_fonctionnement"))) %>%
  rename_at(vars(matches("Qualité")), funs(str_replace(., "Qualité", "chsvi_qualite"))) %>%
  rename_at(vars(matches("Qualite")), funs(str_replace(., "Qualite", "chsvi_qualite"))) %>%
  rename_at(vars(starts_with("Remarque")), funs(str_replace(., "Remarque", "chsvi_remarques"))) %>%
  rename_at(vars(matches("AFaire")), funs(str_replace(., "AFaire", "chsvi_actionafaire"))) %>%
  rename_at(vars(matches("ToDo")), funs(str_replace(., "ToDo", "chsvi_actionafaire"))) %>%
  rename_at(vars(matches("A faire Printemps 2019")), funs(str_replace(., "A faire Printemps 2019", "chsvi_actionafaire"))) %>%
  select(-contains("SuiviTerrainID")) %>% 
  #rename( = ``) %>% 
  mutate('_modif_utilisateur' = NA) %>% 
  mutate('_modif_type' = NA) %>% 
  mutate('_modif_date' = NA)
  
## Complément des données ##
dataaimporter <- 
  dataaimporter %>% 
  mutate(chsvi_typesuivi = ifelse("chsvi_typesuivi" %in% names(.) & typemesure == "Thermie", chsvi_typesuivi, "Thermie")) %>% 
  mutate(chsvi_unite = ifelse("chsvi_unite" %in% names(.) & typemesure == "Thermie", chsvi_unite, "°C")) %>% 
  #mutate(chsvi_typesuivi = ifelse("chsvi_typesuivi" %in% names(.) & typemesure == "Piézométrie", chsvi_typesuivi, "Piézométrie")) %>% # Ne convient pas car écrase l'ancienne valeur
  #mutate(chsvi_unite = ifelse("chsvi_unite" %in% names(.) & typemesure == "Piézométrie", chsvi_unite, as.character(NA))) %>% # Ne convient pas car écrase l'ancienne valeur
  mutate(chsvi_qualite = ifelse("chsvi_qualite" %in% names(.), chsvi_qualite, as.character(NA))) %>% 
  mutate(id = as.numeric(""))

if(typemesure != "Thermie") stop(paste0("Complément des données non développés pour le type de mesures ",typemesure))
  
## Formatage des données ##
# if(testit::has_warning(dataaimporter %>% mutate(chsta_date = format(dmy(chsvi_date), format="%Y-%m-%d"))) == FALSE){
#   dataaimporter <- 
#     dataaimporter %>% 
#     mutate(chsvi_date = format(dmy(chsvi_date), format="%Y-%m-%d"))
# }
# 
# if(testit::has_warning(dataaimporter %>% mutate(chsta_date = format(ymd(chsvi_date), format="%Y-%m-%d"))) == FALSE){
#   dataaimporter <- 
#     dataaimporter %>% 
#     mutate(chsvi_date = format(ymd(chsvi_date), format="%Y-%m-%d"))
# }

## Filtrage des données ##
dataaimporter <- 
  dataaimporter %>% 
  filter(!is.na(chsvi_date)) #%>% 
  # mutate(chsvi_heure = format(ymd_hms(chsvi_heure), format="%H:%M:%S"))
  
## Modification de l'ordre des champs ##
dataaimporter <- 
  dataaimporter %>% 
  select(match(colnames(SuiviTerrain),names(.)))

}

#### Stations ####
if(Type == "Stations"){

## Chargement des données ##
dataaimporter <- read_excel(adresse.switch(Localisation), sheet = 1)

## Transformation ##
dataaimporter <- 
  dataaimporter %>% 
  rename_at(vars(contains("CodeRDT")), funs(str_replace(., "CodeRDT", "coderhj"))) %>%
  rename_at(vars(contains("X")), funs(str_replace(., "X", "coord_x"))) %>%
  #rename_at(vars(contains("XLIIE")), funs(str_replace(., "XLIIE", "coord_x"))) %>%
  rename_at(vars(contains("Y")), funs(str_replace(., "Y", "coord_y"))) %>%
  #rename_at(vars(contains("YLIIE")), funs(str_replace(., "YLIIE", "coord_y"))) %>%
  rename_at(vars(contains("LIIE")), funs(str_replace(., "LIIE", ""))) %>%
  rename_at(vars(contains("TypeCoord")), funs(str_replace(., "TypeCoord", "coord_type"))) %>%
  rename_at(vars(contains("CommuneINSEE")), funs(str_replace(., "CommuneINSEE", "commune"))) %>%
  rename_at(vars(contains("Ecosystème")), funs(str_replace(., "Ecosystème", "milieu"))) %>%
  rename_at(vars(contains("Code hydro")), funs(str_replace(., "Code hydro", "milieucodehydro"))) %>%
  rename_at(vars(contains("ReseauThermie")), funs(str_replace(., "ReseauThermie", "reseauthermietype"))) %>%
  rename_at(vars(contains("Thermie")), funs(str_replace(., "Thermie", "suivithermie"))) %>%
  rename_at(vars(contains("Piezo")), funs(str_replace(., "Piezo", "suivipiezo"))) %>%
  rename_at(vars(contains("Hydro")), funs(str_replace(., "Hydro", "suivihydro"))) %>%
  rename_at(vars(contains("Type")), funs(str_replace(., "Type", "suivihydro"))) %>%
  rename_at(vars(contains("O2")), funs(str_replace(., "O2", "suivio2"))) %>%
  rename_at(vars(contains("Pluvio")), funs(str_replace(., "Pluvio", "suivipluvio"))) %>%
  rename_at(vars(contains("CodCONT")), funs(str_replace(., "CodCONT", "codecontextepdpg"))) %>%
  rename_at(vars(contains("Sous_bassi")), funs(str_replace(., "Sous_bassi", "sousbassin"))) %>%
  rename_at(vars(contains("DistSource")), funs(str_replace(., "DistSource", "distancesource"))) %>%
  rename_at(vars(contains("Tmm30j")), funs(str_replace(., "Tmm30j", "temperaturemax"))) %>%
  rename_at(vars(contains("Sectionmou")), funs(str_replace(., "Sectionmou", "sectionmouillee"))) %>%
  rename_at(vars(contains("LLitMin")), funs(str_replace(., "LLitMin", "largeurlitmineur"))) %>%
  rename_at(vars(matches("LLitEti")), funs(str_replace(., "LLitEti", "largeurlitetiage"))) %>%
  rename_at(vars(starts_with("NTT")), funs(str_replace(., "NTT", "typetheorique"))) %>%
  rename_at(vars(matches("0NGFéchelle")), funs(str_replace(., "0NGFéchelle", "zcapteur"))) %>%
  rename_at(vars(contains("ProfSondeE")), funs(str_replace(., "ProfSondeE", "profsonde"))) %>%
  rename_at(vars(contains("SurfaceBV")), funs(str_replace(., "SurfaceBV", "surfacebassinversant"))) %>%
  rename_at(vars(contains("Remarque")), funs(str_replace(., "Remarque", "remarques"))) %>%
  rename_all(funs(str_to_lower(.))) %>% 
  rename_all(funs(str_replace(., "[[:punct:]]", "_"))) %>% 
  rename_all(funs(str_replace(., "chsta_", ""))) %>% # car parfois déjà présent devant certains noms de colonnes dans excel
  rename_all(funs(str_c("chsta_",.))) %>% 
  mutate(chsta_coord_x = ifelse(is.na(chsta_coord_x) & "chsta_coord_xl93" %in% names(.), chsta_coord_xl93, chsta_coord_x)) %>% 
  mutate(chsta_coord_y = ifelse(is.na(chsta_coord_y) & "chsta_coord_yl93" %in% names(.), chsta_coord_yl93, chsta_coord_y)) %>% 
  mutate(chsta_coord_x = ifelse(is.na(chsta_coord_x) & "chsta_coord_xliie" %in% names(.), chsta_coord_xliie, chsta_coord_x)) %>% 
  mutate(chsta_coord_y = ifelse(is.na(chsta_coord_y) & "chsta_coord_yliie" %in% names(.), chsta_coord_yliie, chsta_coord_y)) %>% 
  select(-(matches("chsta_stationid"))) %>% 
  select(-(matches("chsta_coord_xliie"))) %>% 
  select(-(matches("chsta_coord_yliie"))) %>% 
  select(-(matches("chsta_coord_xl93"))) %>% 
  select(-(matches("chsta_coord_yl93"))) %>% 
  select(-(matches("chsta_coord_xamont"))) %>% 
  select(-(matches("chsta_coord_yaval"))) %>% 
  rowwise() %>% 
  mutate(chsta_distancesource_confluencedrainprincipal = ifelse("chsta_distancesource_confluencedrainprincipal" %in% names(.), chsta_distancesource_confluencedrainprincipal, NA)) %>% 
  mutate(chsta_milieucodehydro = ifelse("chsta_milieucodehydro" %in% names(.), chsta_milieucodehydro, NA)) %>% 
  mutate(chsta_bassin = ifelse("chsta_bassin" %in% names(.), chsta_bassin, NA)) %>% 
  mutate(chsta_sousbassin = ifelse("chsta_sousbassin" %in% names(.), chsta_sousbassin, NA)) %>% 
  mutate(chsta_commune = ifelse("chsta_commune" %in% names(.), chsta_commune, NA)) %>% 
  mutate(chsta_departement = ifelse("chsta_departement" %in% names(.), chsta_departement, NA)) %>% 
  mutate(chsta_pays = ifelse("chsta_pays" %in% names(.), chsta_pays, NA)) %>% 
  mutate(chsta_transmission = ifelse("chsta_transmission" %in% names(.), chsta_transmission, NA)) %>% 
  mutate(chsta_fonctionnement = ifelse("chsta_fonctionnement" %in% names(.), chsta_fonctionnement, NA)) %>% 
  mutate(chsta_suivithermie = ifelse("chsta_suivithermie" %in% names(.), chsta_suivithermie, NA)) %>% 
  mutate(chsta_codemo = ifelse("chsta_codemo" %in% names(.), chsta_codemo, NA)) %>% 
  mutate(chsta_codesie = ifelse("chsta_codesie" %in% names(.), chsta_codesie, NA)) %>% 
  mutate(chsta_mo = ifelse("chsta_mo" %in% names(.), chsta_mo, NA)) %>% 
  mutate(chsta_coord_type = ifelse("chsta_coord_type" %in% names(.), chsta_coord_type, NA)) %>% 
  mutate(chsta_reseauthermietype = ifelse("chsta_reseauthermietype" %in% names(.), chsta_reseauthermietype, NA)) %>% 
  mutate(chsta_suivipiezo = ifelse("chsta_suivipiezo" %in% names(.), chsta_suivipiezo, NA)) %>% 
  mutate(chsta_suivio2 = ifelse("chsta_suivio2" %in% names(.), chsta_suivio2, NA)) %>% 
  mutate(chsta_suivipluvio = ifelse("chsta_suivipluvio" %in% names(.), chsta_suivipluvio, NA)) %>% 
  mutate(chsta_altitude = ifelse("chsta_altitude" %in% names(.), chsta_altitude, NA)) %>% 
  mutate(chsta_distancesource = ifelse("chsta_distancesource" %in% names(.), chsta_distancesource, NA)) %>% 
  mutate(chsta_temperaturemax = ifelse("chsta_temperaturemax" %in% names(.), chsta_temperaturemax, NA)) %>% 
  mutate(chsta_sectionmouillee = ifelse("chsta_sectionmouillee" %in% names(.), chsta_sectionmouillee, NA)) %>% 
  mutate(chsta_durete = ifelse("chsta_durete" %in% names(.), chsta_durete, NA)) %>% 
  mutate(chsta_largeurlitmineur = ifelse("chsta_largeurlitmineur" %in% names(.), chsta_largeurlitmineur, NA)) %>% 
  mutate(chsta_largeurlitetiage = ifelse("chsta_largeurlitetiage" %in% names(.), chsta_largeurlitetiage, NA)) %>% 
  mutate(chsta_pente = ifelse("chsta_pente" %in% names(.), chsta_pente, NA)) %>% 
  mutate(chsta_typetheorique = ifelse("chsta_typetheorique" %in% names(.), chsta_typetheorique, NA)) %>% 
  mutate(chsta_carteign = ifelse("chsta_carteign" %in% names(.), chsta_carteign, NA)) %>% 
  mutate(chsta_rive = ifelse("chsta_rive" %in% names(.), chsta_rive, NA)) %>% 
  mutate(chsta_ancrage = ifelse("chsta_ancrage" %in% names(.), chsta_ancrage, NA)) %>% 
  mutate(chsta_acces = ifelse("chsta_acces" %in% names(.), chsta_acces, NA)) %>% 
  mutate(chsta_detailsloc = ifelse("chsta_detailsloc" %in% names(.), chsta_detailsloc, NA)) %>% 
  mutate(chsta_description = ifelse("chsta_description" %in% names(.), chsta_description, NA)) %>% 
  mutate(chsta_remarques = ifelse("chsta_remarques" %in% names(.), chsta_remarques, NA)) %>% 
  mutate(chsta_ordretournee = ifelse("chsta_ordretournee" %in% names(.), chsta_ordretournee, NA)) %>% 
  mutate(chsta_impacts = ifelse("chsta_impacts" %in% names(.), chsta_impacts, NA)) %>% 
  mutate(chsta_profsonde = ifelse("chsta_profsonde" %in% names(.), chsta_profsonde, NA)) %>% 
  mutate(chsta_substrats = ifelse("chsta_substrats" %in% names(.), chsta_substrats, NA)) %>% 
  mutate(chsta_distberge = ifelse("chsta_distberge" %in% names(.), chsta_distberge, NA)) %>% 
  mutate(chsta_numphoto = ifelse("chsta_numphoto" %in% names(.), chsta_numphoto, NA)) %>% 
  mutate(chsta_zbouchon = ifelse("chsta_zbouchon" %in% names(.), chsta_zbouchon, NA)) %>% 
  mutate(chsta_typez = ifelse("chsta_typez" %in% names(.), chsta_typez, NA)) %>% 
  mutate(chsta_hcapteurbouchon = ifelse("chsta_hcapteurbouchon" %in% names(.), chsta_hcapteurbouchon, NA)) %>% 
  mutate(chsta_codecontextepdpg = ifelse("chsta_codecontextepdpg" %in% names(.), chsta_codecontextepdpg, NA)) %>% 
  mutate(chsta_url = ifelse("chsta_url" %in% names(.), chsta_url, NA)) %>% 
  mutate(chsta_module = ifelse("chsta_module" %in% names(.), round(as.numeric(chsta_module),3), NA)) %>% 
  mutate(chsta_qmna5 = ifelse("chsta_qmna5" %in% names(.), chsta_qmna5, NA)) %>% 
  mutate(chsta_q2 = ifelse("chsta_q2" %in% names(.), chsta_q2, NA)) %>% 
  mutate(chsta_q5 = ifelse("chsta_q5" %in% names(.), chsta_q5, NA)) %>% 
  mutate(chsta_q10 = ifelse("chsta_q10" %in% names(.), chsta_q10, NA)) %>% 
  mutate(chsta_q20 = ifelse("chsta_q20" %in% names(.), chsta_q20, NA)) %>% 
  mutate(chsta_q30 = ifelse("chsta_q30" %in% names(.), chsta_q30, NA)) %>% 
  mutate(chsta_q50 = ifelse("chsta_q50" %in% names(.), chsta_q50, NA)) %>% 
  mutate(chsta_q100 = ifelse("chsta_q100" %in% names(.), chsta_q100, NA)) %>% 
  mutate(chsta_q300 = ifelse("chsta_q300" %in% names(.), chsta_q300, NA)) %>% 
  mutate(chsta_surfacebassinversant = ifelse("chsta_surfacebassinversant" %in% names(.), chsta_surfacebassinversant, NA)) %>% 
  ungroup() %>% 
  # select(-(matches("chsta_afaire"))) %>%
  # select(-(matches("chsta_numcarte"))) %>%
  # select(-(matches("chsta_projection"))) %>%
  # select(-(matches("chsta_type"))) %>%
  # select(-(matches("chsta_profondeur"))) %>%
  select(chsta_coderhj,
         chsta_codemo,
         chsta_codesie,
         chsta_mo,
         chsta_milieu,
         chsta_milieucodehydro,
         chsta_bassin,
         chsta_sousbassin,
         chsta_commune,
         chsta_departement,
         chsta_codecontextepdpg,
         chsta_pays,
         chsta_coord_x,
         chsta_coord_y,
         chsta_coord_type,
         chsta_fonctionnement,
         chsta_transmission,
         chsta_suivithermie,
         chsta_reseauthermietype,
         chsta_suivipiezo,
         chsta_suivihydro,
         chsta_suivio2,
         chsta_suivipluvio,
         chsta_altitude,
         chsta_distancesource,
         chsta_distancesource_confluencedrainprincipal,
         chsta_temperaturemax,
         chsta_sectionmouillee,
         chsta_durete,
         chsta_largeurlitmineur,
         chsta_largeurlitetiage,
         chsta_pente,
         chsta_typetheorique,
         chsta_surfacebassinversant,
         chsta_carteign,
         chsta_rive,
         chsta_ancrage,
         chsta_acces,
         chsta_detailsloc,
         chsta_description,
         chsta_url,
         chsta_remarques,
         chsta_ordretournee,
         chsta_impacts,
         chsta_profsonde,
         chsta_substrats,
         chsta_distberge,
         chsta_numphoto,
         chsta_zcapteur,
         chsta_zbouchon,
         chsta_typez,
         chsta_hcapteurbouchon,
         chsta_module,
         chsta_qmna5,
         chsta_q2,
         chsta_q5,
         chsta_q10,
         chsta_q20,
         chsta_q30,
         chsta_q50,
         chsta_q100,
         chsta_q300
         ) %>% 
  mutate(id = NA) %>% 
  mutate('_modif_utilisateur' = NA) %>% 
  mutate('_modif_type' = NA) %>% 
  mutate('_modif_date' = NA) %>% 
  select(id, everything(), '_modif_utilisateur', '_modif_type', '_modif_date')

## Test ##
if(dataaimporter %>% filter(is.na(chsta_coord_x)) %>% nrow() > 0) stop("Présence de stations sans coordonnées")
}
  
#### Sortie des résultats ####
return(dataaimporter)
  
} # Fin de la fonction
