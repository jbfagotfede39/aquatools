#' Mise au format des données pour ajout BDD
#'
#' Cette fonction permet de formater les données pour les ajouter aux bases de données respectives
#' 
#' @param data Chronique à valider
#' @keywords data
#' @import dplyr
#' @import stringr
#' @import lubridate
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
  db <- BDD.ouverture("Macroinvertébrés")
  
  ## Récupération des données ##
  HabitatsReference <- head(tbl(db,"HabitatsReference"), 10) %>% collect()
  Habitats <- head(tbl(db,"Habitats"), 10) %>% collect()
  Prelevements <- head(tbl(db,"Prelevements"), 10) %>% collect()
  Captures <- head(tbl(db,"Captures"), 10) %>% collect()
  
  # Travail sur les habitats #
  if(all(colnames(data) %in% colnames(Habitats))) {
    
    # Ajout des ID
    data$HabitatID <- row_number(data$OperationID) + as.numeric(tbl(db,"Habitats") %>% summarise(max = max(HabitatID, na.rm = TRUE)) %>% collect()) # Pour incrémenter les HabitatID à partir du dernier
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
    data$CaptureID <- row_number(data$PrelevementID) + as.numeric(tbl(db,"Captures") %>% summarise(max = max(CaptureID, na.rm = TRUE)) %>% collect()) # Pour incrémenter les CaptureID à partir du dernier
  }
  
  ###### Chroniques ######
  ## Connexion à la BDD ##
  db <- BDD.ouverture("Chroniques")
  
  ## Récupération des données ##
  Stations <- head(tbl(db,"Stations"), 10) %>% collect()
  Capteurs <- head(tbl(db,"Capteurs"), 10) %>% collect()
  SuiviBDD <- head(tbl(db,"SuiviBDD"), 10) %>% collect()
  SuiviTerrain <- head(tbl(db,"SuiviTerrain"), 10) %>% collect()
  Mesures <- head(tbl(db,"Mesures"), 10) %>% collect()
  
  # Mesures #
  if(all(colnames(data) %in% colnames(Mesures))) {
    
    # Arrondi des valeurs
    data$Valeur <- round(as.numeric(data$Valeur),3) # On arrondi à 3 chiffres après la virgule
    
    # Transformation des formats
    data$MesureID <- as.integer(data$MesureID)
    data$Date <- as.character(data$Date)
    
    # Ajout des ID
    data$MesureID <- row_number(data$Valeur) + as.numeric(tbl(db,"Mesures") %>% summarise(max = max(MesureID, na.rm = TRUE)) %>% collect()) # Pour incrémenter les MesureID à partir du dernier
    if(dim(filter(data, is.na(MesureID)))[1] > 0 & dim(filter(data, is.na(Validation)))[1] == 0) data$MesureID <- row_number(data$Validation) + as.numeric(tbl(db,"Mesures") %>% summarise(max = max(MesureID, na.rm = TRUE)) %>% collect())
    if(dim(filter(data, is.na(MesureID)))[1] > 0) stop("Tous les id ne sont pas complétés")
  }
  
  # SuiviTerrain #
  if(all(colnames(data) %in% colnames(SuiviTerrain))) {
    
    # Travail sur les stations #
    data$CodeRDT <- str_replace(data$CodeRDT, " ", "") # On efface les espaces en trop dans les noms de station
    
    # Travail sur les heures #
    if(all(!is.na(data$Heure))){ # Afin de n'appliquer les commandes que dans le cas où il n'y a pas que des NA dans les heures
    data$Heure <- str_replace(data$Heure, "h", ":") # On remplace le h par :
    if(all(str_count(data$Heure, ":") == 1)) data$Heure <- str_c(data$Heure, ":00") # On ajoute les secondes à la fin s'il n'y a qu'une seule fois :
    data$Heure <- format(ymd_hms(paste(data$Date,"-",data$Heure)), format="%H:%M:%S") # Afin de ré-écrire les heures proprement
    }
    
    # Travail sur les valeurs manuelles #
    data <- data %>% mutate(Valeur = ifelse(Valeur == "-", NA, Valeur)) # On met des NA pour les valeurs absentes
    data$Valeur <- round(as.numeric(data$Valeur),2) # On arrondi à 1 chiffre après la virgule
    
    # Transformation des numéros de capteurs
    data$Capteur <- str_replace(data$Capteur, "\\..*", "") # On supprime d'éventuels .0 à la fin
    
    # Transformation des actions
    data$Action <- dplyr::recode(data$Action,
                                 "disparue" = "Disparue",
                                 "releve" = "Relève",
                                 "relève" = "Relève",
                                 "pose" = "Pose"
    )
    
    # Transformation des formats
    data$SuiviTerrainID <- as.integer(data$SuiviTerrainID)
    data$Date <- as.character(data$Date)
    
    # Ajout des ID
    data$SuiviTerrainID <- row_number(data$CodeRDT) + as.numeric(tbl(db,"SuiviTerrain") %>% summarise(max = max(SuiviTerrainID, na.rm = TRUE)) %>% collect()) # Pour incrémenter les SuiviTerrainID à partir du dernier

    # Vérification des types d'action
    if(dim(filter(data, !(Action == "Disparue"|Action == "Pose"|Action == "Dépose"|Action == "Relève")))[1] > 0) stop("Action saisie de type inconnu")
  }
  
  ##### PC #####
  ## Connexion à la BDD ##
  db <- BDD.ouverture("Physico-chimie")
  
  ## Récupération des données ##
  PC <- head(tbl(db,"PC"), 10) %>% collect()
  Operations <- head(tbl(db,"Operations"), 10) %>% collect()
  SuiviBDD <- head(tbl(db,"SuiviBDD"), 10) %>% collect()
  
  ## Travail sur les mesures de PC ##
  if(all(colnames(data) %in% colnames(PC))) {
    
    # Transformation des formats
    data$Date <- as.character(data$Date) # Car sinon transformation automatique des formats de date
    
    # Ajout des ID
    data$MesureID <- row_number(data$Valeur) + as.numeric(tbl(db,"PC") %>% summarise(max = max(MesureID, na.rm = TRUE)) %>% collect()) # Pour incrémenter les MesureID à partir du dernier
    if(dim(filter(data, is.na(MesureID)))[1] > 0) stop("Tous les id ne sont pas complétés")
  }
  
  ##### Commun #####
data <- as.data.frame(data)

  return(data)
  
} # Fin de la fonction
