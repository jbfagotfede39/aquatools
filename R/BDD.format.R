#' Mise au format des données pour ajout BDD
#'
#' Cette fonction permet de formater les données pour les ajouter aux bases de données respectives
#' @name BDD.format
#' @param data Chronique à valider
#' @keywords data
#' @import dplyr
#' @import lubridate
#' @import sf
#' @import stringr
#' @export
#' @examples
#' BDD.format(data)

###### À faire #####
# Ajout d'un test pour les suivi de chronique : si le champ fonctionnement contient perdue, alors le champ action ne peut être que disparue, sinon stop et signalement
# 
####################

BDD.format <- function(
  data = data,
  traitementforce = FALSE,
  Type = c("MI", "Chroniques", "PC")
  )
{
  
  ##### Évaluation des choix #####
  Type <- match.arg(Type)

  ###### Contexte ######
  if(traitementforce == "TRUE"){Type <- match.arg(Type)} # Évaluation des choix
  if(Type == "MI") warning("Attention le type par défaut est MI")
  
  ###### MI ######
  Testtraitementforce <- 0
  if(traitementforce == TRUE & Type == "MI") Testtraitementforce <- 1
  if(traitementforce == FALSE & Type == "MI") Testtraitementforce <- 1
  if(Testtraitementforce == 1){
  ## Connexion à la BDD ##
  dbMI <- BDD.ouverture("Macroinvertébrés")
  
  ## Récupération des données ##
  HabitatsReference <- head(tbl(dbMI,"HabitatsReference"), 10) %>% collect()
  Habitats <- head(tbl(dbMI,"Habitats"), 10) %>% collect()
  Prelevements <- head(tbl(dbMI,"Prelevements"), 10) %>% collect()
  Captures <- head(tbl(dbMI,"Captures"), 10) %>% collect()
  
  # Travail sur les habitats #
  if(all(colnames(data) %in% colnames(Habitats))) {
    
    # Ajout des ID
    data$HabitatID <- row_number(data$OperationID) + as.numeric(tbl(dbMI,"Habitats") %>% summarise(max = max(HabitatID, na.rm = TRUE)) %>% collect()) # Pour incrémenter les HabitatID à partir du dernier
  }
  
  # Travail sur les prélèvements #
  if(all(colnames(data) %in% colnames(Prelevements))) {

    # Transformation des formats
    data$OperationID <- as.integer(data$OperationID)
    data$NumEchMAG20 <- as.integer(data$NumEchMAG20)
    data$NumEchCommun <- as.integer(data$NumEchCommun)
    
    # Ajout des ID
    data$PrelevementID <- row_number(data$OperationID) + max(Prelevements$PrelevementID, na.rm = TRUE) # Pour incrémenter les MesureID à partir du dernier
  }
  
  # Travail sur les captures #
  if(all(colnames(data) %in% colnames(Captures))) {

    # Transformation des formats
    data$CaptureID <- as.integer(data$CaptureID)
    data$PrelevementID <- as.integer(data$PrelevementID)
    data$Abondance <- as.integer(data$Abondance)
    
    # Ajout des ID
    data$CaptureID <- row_number(data$PrelevementID) + as.numeric(tbl(dbMI,"Captures") %>% summarise(max = max(CaptureID, na.rm = TRUE)) %>% collect()) # Pour incrémenter les CaptureID à partir du dernier
  } # Fin de travail sur les captures
  } # Fin de travail sur les MI
  
  ###### Chroniques ######
  Testtraitementforce <- 0
  if(traitementforce == TRUE & Type == "Chroniques") Testtraitementforce <- 1
  if(traitementforce == FALSE & Type == "Chroniques") Testtraitementforce <- 1
  if(Testtraitementforce == 1){
    
  ## Connexion à la BDD ##
  dbD <- BDD.ouverture("Data")
  
  ## Récupération des données ##
  Stations <- structure(list(id = 301L, chsta_coderhj = "CHYh1", chsta_codemo = "H1", 
                             chsta_codesie = NA_character_, chsta_mo = "FDCJ", chsta_milieu = "CHY", 
                             chsta_milieucodehydro = NA, chsta_bassin = NA, chsta_sousbassin = NA, 
                             chsta_commune = "39201", chsta_departement = "39", chsta_codecontextepdpg = NA_character_, 
                             chsta_pays = "France", chsta_coord_x = 915084, chsta_coord_y = 6618704, 
                             chsta_coord_type = "L93", chsta_fonctionnement = "En cours", 
                             chsta_transmission = "false", chsta_suivithermie = "true", 
                             chsta_reseauthermietype = NA_character_, chsta_suivipiezo = "true", 
                             chsta_suivihydro = "true", chsta_suivio2 = NA_character_, 
                             chsta_suivipluvio = NA_character_, chsta_altitude = NA_real_, 
                             chsta_distancesource = NA_real_, chsta_distancesource_confluencedrainprincipal = NA, 
                             chsta_temperaturemax = NA_integer_, chsta_sectionmouillee = NA_integer_, 
                             chsta_durete = NA_integer_, chsta_largeurlitmineur = NA_integer_, 
                             chsta_largeurlitetiage = NA_integer_, chsta_pente = NA_integer_, 
                             chsta_typetheorique = NA_character_, chsta_surfacebassinversant = NA_real_, 
                             chsta_carteign = NA_character_, chsta_rive = NA_character_, 
                             chsta_ancrage = NA_character_, chsta_acces = NA_character_, 
                             chsta_detailsloc = NA_character_, chsta_description = NA_character_, 
                             chsta_url = NA_character_, chsta_remarques = NA_character_, 
                             chsta_ordretournee = NA_integer_, chsta_impacts = NA_character_, 
                             chsta_profsonde = NA_real_, chsta_substrats = NA_character_, 
                             chsta_distberge = NA_real_, chsta_numphoto = NA_character_, 
                             chsta_zcapteur = NA_real_, chsta_zbouchon = NA_real_, chsta_typez = NA_real_, 
                             chsta_hcapteurbouchon = NA_character_, chsta_module = NA_real_, 
                             chsta_qmna5 = NA_real_, chsta_q2 = NA_real_, chsta_q5 = NA_real_, 
                             chsta_q10 = NA_real_, chsta_q20 = NA_real_, chsta_q30 = NA_real_, 
                             chsta_q50 = NA_real_, chsta_q100 = NA_real_, chsta_q300 = NA_real_, 
                             `_modif_utilisateur` = "jb", `_modif_type` = "U", `_modif_date` = structure(1561709154.12295, class = c("POSIXct", 
                                                                                                                                     "POSIXt"), tzone = ""), geom = structure(list(structure(c(915084, 
                                                                                                                                                                                               6618704), class = c("XY", "POINT", "sfg"))), n_empty = 0L, class = c("sfc_POINT", 
                                                                                                                                                                                                                                                                    "sfc"), precision = 0, bbox = structure(c(xmin = 915084, 
                                                                                                                                                                                                                                                                                                              ymin = 6618704, xmax = 915084, ymax = 6618704), class = "bbox"), crs = structure(list(
                                                                                                                                                                                                                                                                                                                epsg = 2154L, proj4string = "+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"), class = "crs"))), row.names = 1L, class = c("sf", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "data.frame"), sf_column = "geom", agr = structure(c(id = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_coderhj = NA_integer_, chsta_codemo = NA_integer_, chsta_codesie = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_mo = NA_integer_, chsta_milieu = NA_integer_, chsta_milieucodehydro = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_bassin = NA_integer_, chsta_sousbassin = NA_integer_, chsta_commune = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_departement = NA_integer_, chsta_codecontextepdpg = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_pays = NA_integer_, chsta_coord_x = NA_integer_, chsta_coord_y = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_coord_type = NA_integer_, chsta_fonctionnement = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_transmission = NA_integer_, chsta_suivithermie = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_reseauthermietype = NA_integer_, chsta_suivipiezo = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_suivihydro = NA_integer_, chsta_suivio2 = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_suivipluvio = NA_integer_, chsta_altitude = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_distancesource = NA_integer_, chsta_distancesource_confluencedrainprincipal = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_temperaturemax = NA_integer_, chsta_sectionmouillee = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_durete = NA_integer_, chsta_largeurlitmineur = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_largeurlitetiage = NA_integer_, chsta_pente = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_typetheorique = NA_integer_, chsta_surfacebassinversant = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_carteign = NA_integer_, chsta_rive = NA_integer_, chsta_ancrage = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_acces = NA_integer_, chsta_detailsloc = NA_integer_, chsta_description = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_url = NA_integer_, chsta_remarques = NA_integer_, chsta_ordretournee = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_impacts = NA_integer_, chsta_profsonde = NA_integer_, chsta_substrats = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_distberge = NA_integer_, chsta_numphoto = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_zcapteur = NA_integer_, chsta_zbouchon = NA_integer_, chsta_typez = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_hcapteurbouchon = NA_integer_, chsta_module = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_qmna5 = NA_integer_, chsta_q2 = NA_integer_, chsta_q5 = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_q10 = NA_integer_, chsta_q20 = NA_integer_, chsta_q30 = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_q50 = NA_integer_, chsta_q100 = NA_integer_, chsta_q300 = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   `_modif_utilisateur` = NA_integer_, `_modif_type` = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   `_modif_date` = NA_integer_), class = "factor", .Label = c("constant", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "aggregate", "identity")))
  Capteurs <- structure(list(id = 13L, chcap_proprietaire = "CD39", chcap_typecapteur = "Thermie", 
                             chcap_modelecapteur = NA_character_, chcap_numerocapteur = "10316505", 
                             chcap_etat = NA_character_, chcap_projet = "RDP", chcap_originecapteur = "Achat", 
                             chcap_datedebut = NA_character_, chcap_datefin = NA_character_, 
                             chcap_remarques = NA_character_, `_modif_utilisateur` = NA_character_, 
                             `_modif_type` = NA_character_, `_modif_date` = structure(NA_real_, class = c("POSIXct", 
                                                                                                          "POSIXt"), tzone = "")), row.names = 1L, class = c("tbl_df", 
                                                                                                                                                             "tbl", "data.frame"))
  Mesures <- structure(list(id = 32968L, chmes_coderhj = "BON", chmes_capteur = "350938", 
                            chmes_date = structure(15126, class = "Date"), chmes_heure = "14:00:00", 
                            chmes_valeur = 11.09, chmes_unite = "°C", chmes_typemesure = "Thermie", 
                            chmes_validation = "Validé", chmes_mode_acquisition = "Mesuré", 
                            chmes_mode_integration = "Ajout manuel", `_modif_utilisateur` = "JB", 
                            `_modif_type` = "I", `_modif_date` = structure(1541660441.85543, class = c("POSIXct", 
                                                                                                       "POSIXt"), tzone = "")), row.names = 1L, class = c("tbl_df", 
                                                                                                                                                          "tbl", "data.frame"))
  SuiviTerrain <- structure(list(id = 1151L, chsvi_mo = "SMISA", chsvi_coderhj = "BCB4-1amont", 
                                 chsvi_typesuivi = "Thermie", chsvi_operateurs = "PIZZETTI", 
                                 chsvi_date = "2018-03-26", chsvi_heure = "10:20:00", chsvi_capteur = "20136662", 
                                 chsvi_valeur = 7.4, chsvi_unite = "°C", chsvi_action = "Relève", 
                                 chsvi_fonctionnement = "OK", chsvi_qualite = NA_character_, 
                                 chsvi_actionafaire = NA_character_, chsvi_remarques = "Piles impossible à changer problème paramétrage ordi et impossible de mettre en place nouvelles sonde car impossible d'en lancer une suite  ce problème", 
                                 `_modif_utilisateur` = "JB", `_modif_type` = "I", `_modif_date` = structure(1537779116.5002, class = c("POSIXct", 
                                                                                                                                        "POSIXt"), tzone = "")), row.names = 1L, class = c("tbl_df", 
                                                                                                                                                                                           "tbl", "data.frame"))
  #Capteurs <- tbl(dbD, in_schema("fd_production", "chroniques_capteurs")) %>% collect(n = 1)
  #Stations <- sf::st_read(dbD, query = "select * from fd_production.chroniques_stations limit 1;")
  #Mesures <- tbl(dbD, in_schema("fd_production", "chroniques_mesures")) %>% collect(n = 1)
  #SuiviTerrain <- tbl(dbD, in_schema("fd_production", "chroniques_suiviterrain")) %>% collect(n = 1)
  
  # Mesures #
  if(all(colnames(data) %in% colnames(Mesures))) {
    
    # Arrondi des valeurs
    data$chmes_valeur <- round(as.numeric(data$chmes_valeur),3) # On arrondi à 3 chiffres après la virgule
    
    # Complément des heures #
    data$chmes_heure <- ifelse(nchar(data$chmes_heure) == 5, paste0(data$chmes_heure, ":00"), data$chmes_heure) 
    
    # Transformation des formats
    data$id <- as.integer(data$id)
    data$chmes_date <- as.character(data$chmes_date)
    
    # Ajout des ID
    data$id <- row_number(data$chmes_valeur) + as.numeric(tbl(dbD,in_schema("fd_production", "chroniques_mesures")) %>% summarise(max = max(id, na.rm = TRUE)) %>% collect()) # Pour incrémenter les id à partir du dernier
    if(dim(filter(data, is.na(id)))[1] > 0 & dim(filter(data, is.na(chmes_validation)))[1] == 0) data$id <- row_number(data$chmes_validation) + as.numeric(tbl(dbD,in_schema("fd_production", "chroniques_mesures")) %>% summarise(max = max(id, na.rm = TRUE)) %>% collect())
    if(dim(filter(data, is.na(id)))[1] > 0) stop("Tous les id ne sont pas complétés")
  }
  
  # SuiviTerrain #
  #if(all(colnames(data %>% select(-('_modif_utilisateur':'_modif_date'))) %in% colnames(SuiviTerrain))) {
  if(all(colnames(data) %in% colnames(SuiviTerrain))) {
    
    # Travail sur les MO #
    data <-
      data %>% 
      mutate(chsvi_mo = ifelse(chsvi_mo == "Fédé 39", "FJPPMA", chsvi_mo))
    
    # Travail sur les stations #
    data$chsvi_coderhj <- str_replace(data$chsvi_coderhj, " ", "") # On efface les espaces en trop dans les noms de station
    data$chsvi_coderhj <- str_to_upper(data$chsvi_coderhj, locale = "fr") # On met les noms de station en majuscules
    data <- 
      data %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "VOUCHARTREUSE", "VOUchartreuse", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "VOUGRINGALETPORT", "VOUgringaletport", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "VOUMERCANTINE", "VOUmercantine", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "VOUPATORNAY", "VOUpatornay", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "VOUSURCHAUFFANT", "VOUsurchauffant", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "VOGBARO", "VOGbaro", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "BONBARO", "BONbaro", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "BONLAC", "BONlac", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "BONSEUIL", "BONseuil", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "BONZH", "BONzh", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "BONAVAL", "BONaval", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "BRTBARO", "BRTbaro", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "GCLBARO", "GCLbaro", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "GCLLAC", "GCLlac", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "GCLVANNE", "GCLvanne", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "GCLPLONGEOIR", "GCLplongeoir", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "GCLZHAVAL", "GCLzhaval", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "GCLZHENTRE2", "GCLzhentre2", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "ILABARO", "ILAbaro", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "ILABLOC", "ILAbloc", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "ILALAC", "ILAlac", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "LVALAC", "LVAlac", chsvi_coderhj)) %>% 
      #mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "", "", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "VEU8-8ATMO", "VEU8-8atmo", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "BIE32-4AVAL", "BIE32-4aval", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "LEM-2-2", "LEM2-2", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "NCZ6-2TRÉMONTAGNE", "NCZ6-2", chsvi_coderhj))
    
    # Travail sur les heures #
    if(any(!is.na(data$chsvi_heure))){
    data$chsvi_heure <- str_replace(data$chsvi_heure, "h", ":") # On remplace le h par :
    data$chsvi_heure <- str_replace(data$chsvi_heure, "H", ":") # On remplace le H par :
    data <- 
      data %>% 
      mutate(chsvi_heure = ifelse(grepl("Oubli", chsvi_heure), NA_character_, chsvi_heure)) %>% 
      mutate(chsvi_heure = ifelse(grepl("oubli", chsvi_heure), NA_character_, chsvi_heure)) %>% 
      mutate(count = str_count(.$chsvi_heure, ":")) %>% 
      mutate(chsvi_heure = ifelse(!is.na(chsvi_heure) & count == 1, paste0(chsvi_heure, ":00"), chsvi_heure)) %>% 
      select(-count)
    if(testit::has_warning(format(ymd_hms(paste(data$chsvi_date,"-",data$chsvi_heure)), format="%H:%M:%S")) == FALSE) data$chsvi_heure <- format(ymd_hms(paste(data$chsvi_date,"-",data$chsvi_heure)), format="%H:%M:%S") # Afin de ré-écrire les heures proprement
    if(testit::has_warning(format(ymd_hms(data$chsvi_heure), format="%H:%M:%S")) == FALSE) data$chsvi_heure <- format(ymd_hms(data$chsvi_heure), format="%H:%M:%S") # Afin de ré-écrire les heures proprement
    }

    # Travail sur les dates #
    if(testit::has_warning(ymd(data$chsvi_date)) == TRUE & testit::has_warning(dmy(data$chsvi_date)) == FALSE) data$chsvi_date <- as.character(format(dmy(data$chsvi_date), format="%Y-%m-%d"))
    if(testit::has_warning(ymd(data$chsvi_date)) == TRUE & testit::has_warning(dmy(data$chsvi_date)) == TRUE){ # dans le cas où les formats de date sont mélangés
      data <-
        data %>% 
        mutate(chsvi_datebis = chsvi_date) %>% 
        mutate(chsvi_date = format(ymd(data$chsvi_date), format="%Y-%m-%d")) %>% 
        mutate(chsvi_date = ifelse(is.na(chsvi_date), format(dmy(data$chsvi_date), format="%Y-%m-%d"), chsvi_date)) %>% 
        select(-chsvi_datebis)
      }
    
    
    # Travail sur les valeurs manuelles #
    data$chsvi_valeur <- str_replace(data$chsvi_valeur, "°C", "") # On efface le °C
    data$chsvi_valeur <- str_replace(data$chsvi_valeur, "°c", "") # On efface le °c
    data <- data %>% mutate(chsvi_valeur = ifelse(chsvi_valeur == "-", NA, chsvi_valeur)) # On met des NA pour les valeurs absentes
    data <- data %>% mutate(chsvi_valeur = ifelse(chsvi_valeur == "Impossible", NA, chsvi_valeur)) # On met des NA pour les valeurs absentes
    if(class(data$chsvi_valeur) == "character") data$chsvi_valeur <- as.numeric(sub(",", ".", data$chsvi_valeur))
    data$chsvi_valeur <- round(as.numeric(data$chsvi_valeur),2) # On arrondi à 1 chiffre après la virgule
    
    # Transformation des numéros de capteurs
    data$chsvi_capteur <- str_replace(data$chsvi_capteur, "\\..*", "") # On supprime d'éventuels .0 à la fin
    data$chsvi_capteur <- str_replace(data$chsvi_capteur, "O", "0") # On supprime d'éventuels O par des 0
    
    # Transformation des unités
    data$chsvi_unite <- str_replace(data$chsvi_unite, "degré Celsius", "°C")

    # Transformation des actions
    data$chsvi_action <- dplyr::recode(data$chsvi_action,
                                 "disparue" = "Disparue",
                                 "Sonde disparue" = "Disparue",
                                 "releve" = "Relève",
                                 "relève" = "Relève",
                                 "Relève et repose" = "Relève",
                                 "Relevé" = "Relève",
                                 "pose" = "Pose",
                                 "Repose" = "Pose",
                                 "dépose" = "Dépose"
    )
    
    # Vérification des types d'action
    if(dim(filter(data, chsvi_action == "changement de pile"))[1] > 0){
      data <- 
        data %>% 
        mutate(chsvi_remarques = ifelse(chsvi_action == "changement de pile", paste0(chsvi_remarques, " - Changement de pile"), chsvi_remarques)) %>% 
        mutate(chsvi_action = ifelse(chsvi_action == "changement de pile", "Relève", chsvi_action)) %>% 
        mutate(chsvi_remarques = ifelse(chsvi_remarques == "NA - Changement de pile", "Changement de pile", chsvi_remarques))
    }
    if(dim(filter(data, grepl("Dépose", chsvi_remarques)))[1] > 0){
    data <- 
      data %>% 
      mutate(chsvi_action = ifelse(grepl("Dépose", chsvi_remarques), "Dépose", chsvi_action))
    }
    if(dim(filter(data, !(chsvi_action == "Disparue"|chsvi_action == "Pose"|chsvi_action == "Dépose"|chsvi_action == "Relève"|chsvi_action == "Mesure manuelle"|chsvi_action == "Entretien")))[1] > 0) stop("Action saisie de type inconnu")
    
    # Transformation des formats
    data$id <- as.integer(data$id)
    data$chsvi_date <- as.character(data$chsvi_date)
    
    # Ajout des ID
    data$id <- row_number(data$chsvi_coderhj) + as.numeric(dbGetQuery(dbD, "SELECT MAX(id) FROM fd_production.chroniques_suiviterrain;")) # Pour incrémenter les id à partir du dernier
    data <- data %>% arrange(id)

  }
  
  # Capteurs #
  if(all(colnames(data) %in% colnames(Capteurs))) {
    data$id <- row_number(data$chcap_numerocapteur) + as.numeric(dbGetQuery(dbD, "SELECT MAX(id) FROM fd_production.chroniques_capteurs;")) # Pour incrémenter les id à partir du dernier
  }
  
  # Stations #
  if(all(colnames(data) %in% colnames(Stations))) {
    data <-
      data %>% 
      mutate(id = row_number(data$chsta_coderhj) + as.numeric(dbGetQuery(dbD, "SELECT MAX(id) FROM fd_production.chroniques_stations;"))) %>% # Pour incrémenter les id à partir du dernier
      #arrange(id) %>% # Pour conserver le même ordre que celui dans le fichier de saisie
      mutate(chsta_mo = ifelse(chsta_mo == "FD39", "FJPPMA", chsta_mo)) %>% 
      mutate(chsta_mo = ifelse(chsta_mo == "CD39", "CD39_CR_Ain", chsta_mo)) %>% 
      mutate(chsta_mo = ifelse(chsta_mo == "CD39-FJPPMA", "CD39_CR_Ain - FJPPMA", chsta_mo)) %>% 
      mutate(chsta_mo = ifelse(chsta_mo == "ONEMA", "AFB", chsta_mo)) %>% 
      mutate(chsta_mo = ifelse(chsta_mo == "CD39_CR_Ain - ONEMA", "CD39_CR_Ain - AFB", chsta_mo)) %>% 
      mutate(chsta_numphoto = ifelse(is.na(chsta_numphoto), "acompleter.png", chsta_numphoto))
    
  }
  
  # Résultats #
  if(length(colnames(data)) > 22) {
    if(colnames(data)[45] == "Percentile90diurneAB"){
      data <- 
        data %>% 
        rename_all(list(~ stringi::stri_trans_general(., "latin-ascii"))) %>% # Pour remplacer les caractères accentués par les mêmes sans accents
        rename_all(list(~ paste0("chres_", .))) %>%
        rename_all(list(~ gsub("[[:punct:]]", "_", .))) %>%
        rename_all(list(~ tolower(.))) %>% 
        mutate(chres_aquatoolsversion = packageVersion("aquatools") %>% as.character()) %>% 
        mutate(id = row_number(chres_vmaxmoy24h) + as.numeric(dbGetQuery(dbD, "SELECT MAX(id) FROM fd_production.chroniques_resultats;"))) %>% # Pour incrémenter les id à partir du dernier
        mutate(`_modif_utilisateur` = NA_character_) %>% 
        mutate(`_modif_type` = NA_character_) %>% 
        mutate(`_modif_date` = NA) %>% 
        select(id, everything(), `_modif_utilisateur`, `_modif_type`,`_modif_date`)
    }
  } # fin de travail sur les résultats
  
  DBI::dbDisconnect(dbD)
  
  } # Fin de travail sur les chroniques
  
  ##### PC #####
  Testtraitementforce <- 0
  if(traitementforce == TRUE & Type == "PC") Testtraitementforce <- 1
  if(traitementforce == FALSE & Type == "PC") Testtraitementforce <- 1
  if(Testtraitementforce == 1){
  ## Connexion à la BDD ##
  dbPC <- BDD.ouverture("Physico-chimie")
  
  ## Récupération des données ##
  PC <- head(tbl(dbPC,"PC"), 10) %>% collect()
  Operations <- head(tbl(dbPC,"Operations"), 10) %>% collect()
  SuiviBDD <- head(tbl(dbPC,"SuiviBDD"), 10) %>% collect()
  
  ## Travail sur les mesures de PC ##
  if(all(colnames(data) %in% colnames(PC))) {
    
    # Transformation des formats
    data$Date <- as.character(data$Date) # Car sinon transformation automatique des formats de date
    
    # Ajout des ID
    data$MesureID <- row_number(data$Valeur) + as.numeric(tbl(dbPC,"PC") %>% summarise(max = max(MesureID, na.rm = TRUE)) %>% collect()) # Pour incrémenter les MesureID à partir du dernier
    if(dim(filter(data, is.na(MesureID)))[1] > 0) stop("Tous les id ne sont pas complétés")
  } # Fin de travail sur les mesures de PC
  } # Fin de travail sur PC
  
  ##### Commun #####
data <- as.data.frame(data)

  return(data)
  
} # Fin de la fonction
