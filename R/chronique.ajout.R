#' Ajout de données brutes de chronique
#'
#' Permet d'ajouter des données brutes de chronique présentant des valeurs aberrantes. ATTENTION : ne gère pas les ID
#' @name chronique.ajout
#' @param data Chronique à modifier
#' @param cCodeRDT
#' @param cCapteur
#' @param cDate
#' @param cHeure
#' @param cUnite \code{°C} (par défault)
#' @param cTypeMesure \code{Thermie} (par défault)
#' @param cNvelleValeur Valeur à ajouter
#' @param cValidation \code{Validé} (par défault)
#' @param cModeAcquisition \code{Estimé} (par défault)
#' @keywords chronique
#' @import dplyr lubridate stringr
#' @export
#' @examples
#' chronique.ajout(data, cCodeRDT = "BONbaro", cCapteur = "P0352", cDate = "2014-12-27", cHeure = "19:00:00", cUnite = "°C", cTypeMesure = "Thermie", cNvelleValeur = 15, cValidation = "Validé", cModeAcquisition = "Estimé")

chronique.ajout <- function(data, cCodeRDT = "BONbaro", cCapteur = "P0352", cDate = "2014-12-27", cHeure = "19:00:00", cUnite = "°C", cTypeMesure = "Thermie", cNvelleValeur = 15, cValidation = "Validé", cModeAcquisition = "Estimé")
{
  
  #cCodeRDT <- "BONbaro"; cCapteur <- "P0352"; cDate <- "2014-12-27"; cHeure <- "19:00:00"; cUnite = "°C"; cTypeMesure <- "Thermie"; cNvelleValeur <- 15; cValidation <- "Validé"; cModeAcquisition <- "Estimé"
  #data <- DataToAdd
  
  ##### Transformation des formats #####
  cDate <- ymd(cDate)
  data$chmes_date <- ymd(data$chmes_date)
  
  ##### Copie d'une ligne existante #####
  LigneAjout <- data[1,]
  
  ##### Modification des valeurs #####
  LigneAjout$chmes_coderhj <- cCodeRDT
  LigneAjout$chmes_capteur <- cCapteur
  LigneAjout$chmes_date <- cDate
  LigneAjout$chmes_heure <- cHeure
  LigneAjout$chmes_unite <- cUnite
  LigneAjout$chmes_typemesure <- cTypeMesure
  LigneAjout$chmes_valeur <- as.numeric(cNvelleValeur)
  LigneAjout$chmes_mode_integration <- "Ajout manuel"
  LigneAjout$chmes_validation <- cValidation
  LigneAjout$chmes_mode_acquisition <- cModeAcquisition
  LigneAjout$`_modif_utilisateur` <- NA
  LigneAjout$`_modif_type` <- NA
  LigneAjout$`_modif_date` <- NA
  
  ##### Remise en commun des morceaux #####
  data <-
    dplyr::union(data, LigneAjout)
  
  ##### Rangement dans l'ordre chronologique #####
  data <-
    data %>% 
    arrange(chmes_date, chmes_heure)
  
  ##### Retour des données #####
  return(data)
  
} # Fin de la fonction