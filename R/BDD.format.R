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
# 
# 
####################

BDD.format <- function(data)
{
  ###### MI ######
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
  }
  
  ###### Chroniques ######
  ## Connexion à la BDD ##
  dbC <- BDD.ouverture("Chroniques")
  dbD <- BDD.ouverture("Data")
  
  ## Récupération des données ##
  Stations <- sf::st_read(dbD, query = "select * from fd_production.chroniques_stations limit 3;")
  Capteurs <- tbl(dbD, in_schema("fd_production", "chroniques_capteurs")) %>% collect()
  Mesures <- tbl(dbD, in_schema("fd_production", "chroniques_mesures")) %>% collect(n = 5)
  SuiviTerrain <- tbl(dbD, in_schema("fd_production", "chroniques_suiviterrain")) %>% collect(n = 5)
  # SuiviTerrain <- data.frame("id" = c(NA),
  #                            "chsvi_mo" = c(NA),
  #                            "chsvi_coderhj" = c(NA),
  #                            "chsvi_typesuivi" = c(NA),
  #                            "chsvi_operateurs" = c(NA),
  #                            "chsvi_date" = c(NA),
  #                            "chsvi_heure" = c(NA),
  #                            "chsvi_capteur" = c(NA),
  #                            "chsvi_valeur" = c(NA),
  #                            "chsvi_unite" = c(NA),
  #                            "chsvi_action" = c(NA),
  #                            "chsvi_fonctionnement" = c(NA),
  #                            "chsvi_qualite" = c(NA),
  #                            "chsvi_actionafaire" = c(NA),
  #                            "chsvi_remarques" = c(NA),
  #                            "_modif_utilisateur" = c(NA),
  #                            "_modif_type" = c(NA),
  #                            "_modif_date" = c(NA),
  #                            stringsAsFactors = FALSE)
  
  # Mesures #
  if(all(colnames(data) %in% colnames(Mesures))) {
    
    # Arrondi des valeurs
    data$chmes_valeur <- round(as.numeric(data$chmes_valeur),3) # On arrondi à 3 chiffres après la virgule
    
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
    
    # Travail sur les stations #
    data$chsvi_coderhj <- str_replace(data$chsvi_coderhj, " ", "") # On efface les espaces en trop dans les noms de station
    
    # Travail sur les heures #
    if(all(!is.na(data$chsvi_heure))){ # Afin de n'appliquer les commandes que dans le cas où il n'y a pas que des NA dans les heures
    data$chsvi_heure <- str_replace(data$chsvi_heure, "h", ":") # On remplace le h par :
    data$chsvi_heure <- str_replace(data$chsvi_heure, "H", ":") # On remplace le H par :
    if(all(str_count(data$chsvi_heure, ":") == 1)) data$chsvi_heure <- str_c(data$chsvi_heure, ":00") # On ajoute les secondes à la fin s'il n'y a qu'une seule fois :
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
    
    # Transformation des actions
    data$chsvi_action <- dplyr::recode(data$chsvi_action,
                                 "disparue" = "Disparue",
                                 "Sonde disparue" = "Disparue",
                                 "releve" = "Relève",
                                 "relève" = "Relève",
                                 "pose" = "Pose",
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
    if(dim(filter(data, !(chsvi_action == "Disparue"|chsvi_action == "Pose"|chsvi_action == "Dépose"|chsvi_action == "Relève")))[1] > 0) stop("Action saisie de type inconnu")
    
    # Transformation des formats
    data$id <- as.integer(data$id)
    data$chsvi_date <- as.character(data$chsvi_date)
    
    # Ajout des ID
    data$id <- row_number(data$chsvi_coderhj) + as.numeric(dbGetQuery(dbD, "SELECT MAX(id) FROM fd_production.chroniques_suiviterrain;")) # Pour incrémenter les id à partir du dernier
    data <- data %>% arrange(id)

  }
  
  # Capteurs #
  if(all(colnames(data) %in% colnames(Capteurs))) {
    data$id <- row_number(data$chcap_numerocapteur) + max(Capteurs$id, na.rm = TRUE) # Pour incrémenter les id à partir du dernier
  }
  
  # Résultats #
  if(length(colnames(data)) > 22) {
    if(colnames(data)[22] == "ValRemarqJours.DateVMoyJMaxPer") {
      colnames(data) <- 
        data %>% 
        colnames() %>% 
        paste0("chres_",.) %>% 
        gsub("[[:punct:]]", "_", .) %>% 
        tolower()
      data <-
        data %>% 
        mutate(chres_aquatoolsversion = packageVersion("aquatools"))
    }
  }
  
  ##### PC #####
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
  }
  
  ##### Commun #####
data <- as.data.frame(data)

  return(data)
  
} # Fin de la fonction
