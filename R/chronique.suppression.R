#' Suppression de données brutes de chronique
#'
#' Permet de supprimer des données brutes de chronique présentant des valeurs aberrantes. ATTENTION : ne gère pas les ID
#' @name chronique.suppression
#' @param data Chronique à modifier
#' @param cCodeRDT
#' @param cCapteur
#' @param cDate
#' @param cHeure
#' @param cTypeMesure \code{Thermie} (par défault)
#' @param cValeur Valeur à supprimer
#' @keywords chronique
#' @import dplyr lubridate stringr
#' @export
#' @examples
#' chronique.suppression(data, cCodeRDT = "BONbaro", cCapteur = "P0352", cDate = "2014-12-27", cHeure = "19:00:00", cTypeMesure = "Thermie", cValeur = 15, cValidation = "Validé")

chronique.suppression <- function(data, cCodeRDT = "BONbaro", cCapteur = "P0352", cDate = "2014-12-27", cHeure = "19:00:00", cTypeMesure = "Thermie", cValeur = 15, cValidation = "Validé")
{
  
  
##### -------------- A FAIRE -------------- #####
# 
# -------------- A FAIRE -------------- #

  #cCodeRDT <- "VAL1-9"; cCapteur <- "10316431"; cDate <- "2013-10-01"; cHeure <- "00:00:00"; cTypeMesure <- "Thermie"; cValeur <- 13.076; cValidation <- "Validé"
  #data <- DataToAdd
  
  ##### Transformation des formats #####
  cDate <- ymd(cDate)
  data$Date <- ymd(data$Date)
  
  ##### Vérification de l'existence de la ligne #####
  LigneFausse <-
    data %>% 
    filter(CodeRDT == cCodeRDT & Capteur == cCapteur & Date == cDate & Heure == cHeure & TypeMesure == cTypeMesure & Valeur == cValeur & Validation == cValidation)
  
  if(dim(LigneFausse)[1] == 0) stop("Pas de ligne correspondante")
  if(dim(LigneFausse)[1] >1) stop("Plusieurs lignes correspondantes")
  if(dim(LigneFausse)[1] == 1 && LigneFausse$Validation == "Rejeté") stop("Une ligne correspondante mais déjà rejetée")
  
  
  ##### Filtrage de la ligne en question #####
  data <-
    data %>% 
    filter(!(CodeRDT == cCodeRDT & Capteur == cCapteur & Date == cDate & Heure == cHeure & TypeMesure == cTypeMesure & Valeur == cValeur & Validation == cValidation))
  
  ##### Rangement dans l'ordre chronologique #####
  data <-
    data %>% 
    arrange(Date, Heure)
  
  ##### Retour des données #####
  return(data)
  
} # Fin de la fonction