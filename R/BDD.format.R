#' Mise au format des données pour ajout BDD
#'
#' Cette fonction permet de formater les données pour les ajouter aux bases de données respectives
#' 
#' @param data Chronique à valider
#' @keywords data
#' @import dplyr DBI
#' @export
#' @examples
#' BDD.format(data)

###### À faire #####
# 
####################

BDD.format <- function(data)
{
  ###### MI ######
  ## Connexion à la BDD ##
  db <- BDD.ouverture("Macroinvertébrés")
  
  ## Récupération des données ##
  HabitatsReference <- dbReadTable(db, "HabitatsReference")
  Prelevements <- dbReadTable(db, "Prelevements")
  Captures <- dbReadTable(db, "Captures")
  
  # Travail sur les prélèvements #
  if(all(colnames(data) %in% colnames(Prelevements))) {

    # Transformation des formats
    data$OperationID <- as.integer(data$OperationID)
    data$NumEchMAG20 <- as.integer(data$NumEchMAG20)
    data$NumEchCommun <- as.integer(data$NumEchCommun)
    
    # Ajout des ID
    data$PrelevementID <- row_number(data$OperationID) + max(Prelevements$PrelevementID) # Pour incrémenter les MesureID à partir du dernier
  }
  
# Travail sur les captures #
  
  if(all(colnames(data) %in% colnames(Captures))) {

    # Transformation des formats
    data$CaptureID <- as.integer(data$CaptureID)
    data$PrelevementID <- as.integer(data$PrelevementID)
    data$Effectif <- as.integer(data$Effectif)
    
    # Ajout des ID
    data$CaptureID <- row_number(data$PrelevementID) + max(Captures$CaptureID) # Pour incrémenter les CaptureID à partir du dernier
  }
  
  ###### Chroniques ######
  ## Connexion à la BDD ##
  db <- BDD.ouverture("Chroniques")
  
  ## Récupération des données ##
  Stations <- dbReadTable(db, "Stations")
  Capteurs <- dbReadTable(db, "Capteurs")
  SuiviTerrain <- dbReadTable(db, "SuiviTerrain")
  Mesures <- dbReadTable(db, "Mesures")
  
  # Mesures #
  if(all(colnames(data) %in% colnames(Mesures))) {
    
    # Transformation des formats
    data$MesureID <- as.integer(data$MesureID)
    data$Date <- as.character(data$Date)
    
    # Ajout des ID
    data$MesureID <- row_number(data$Valeur) + max(Mesures$MesureID) # Pour incrémenter les CaptureID à partir du dernier
  }
  
  # SuiviTerrain #
  if(all(colnames(data) %in% colnames(SuiviTerrain))) {
    
    # Transformation des numéros de capteurs
    data$Capteur <- sub(".0$", "", data$Capteur) # On supprime d'éventuels .0 à la fin
  }

  ##### Commun #####
data <- as.data.frame(data)

  return(data)
  
} # Fin de la fonction