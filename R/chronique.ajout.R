#' Ajout de données brutes de chronique
#'
#' Permet d'ajouter des données brutes de chronique présentant des valeurs aberrantes. ATTENTION : ne gère pas les ID
#' @param data Chronique à modifier
#' @param cCodeRDT
#' @param cCapteur
#' @param cDate
#' @param cHeure
#' @param cUnité \code{°C} (par défault)
#' @param cTypeMesure \code{Thermie} (par défault)
#' @param cNvelleValeur Valeur à ajouter
#' @param cValidation \code{Validé} (par défault)
#' @param cModeAcquisition \code{Estimé} (par défault)
#' @keywords chronique
#' @import dplyr lubridate stringr
#' @export
#' @examples
#' chronique.ajout(data, cCodeRDT = "BONbaro", cCapteur = "P0352", cDate = "2014-12-27", cHeure = "19:00:00", cUnité = "°C", cTypeMesure = "Thermie", cNvelleValeur = 15, cValidation = "Validé", cModeAcquisition = "Estimé")

chronique.ajout <- function(data, cCodeRDT = "BONbaro", cCapteur = "P0352", cDate = "2014-12-27", cHeure = "19:00:00", cUnité = "°C", cTypeMesure = "Thermie", cNvelleValeur = 15, cValidation = "Validé", cModeAcquisition = "Estimé")
{
  
  #cCodeRDT <- "BONbaro"; cCapteur <- "P0352"; cDate <- "2014-12-27"; cHeure <- "19:00:00"; cUnité = "°C"; cTypeMesure <- "Thermie"; cNvelleValeur <- 15; cValidation <- "Validé"; cModeAcquisition <- "Estimé"
  #data <- DataToAdd
  
  ##### Transformation des formats #####
  cDate <- ymd(cDate)
  data$Date <- ymd(data$Date)
  
  ##### Copie d'une ligne existante #####
  LigneAjout <- data[1,]
  
  ##### Modification des valeurs #####
  LigneAjout$CodeRDT <- cCodeRDT
  LigneAjout$Capteur <- cCapteur
  LigneAjout$Date <- cDate
  LigneAjout$Heure <- cHeure
  LigneAjout$Unité <- cUnité
  LigneAjout$TypeMesure <- cTypeMesure
  LigneAjout$Valeur <- as.numeric(cNvelleValeur)
  LigneAjout$Validation <- cValidation
  LigneAjout$ModeAcquisition <- cModeAcquisition
  
  ##### Remise en commun des morceaux #####
  data <-
    dplyr::union(data, LigneAjout)
  
  ##### Rangement dans l'ordre chronologique #####
  data <-
    data %>% 
    arrange(Date, Heure)
  
  ##### Retour des données #####
  return(data)
  
} # Fin de la fonction