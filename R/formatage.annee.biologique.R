#' Reformatage de dates
#'
#' Reformate les dates par lot de fichiers (répertoire /Regroupement/Bruts vers /Regroupement/) en format "2014-10-10"
#' 
#' @param data Jeu de données contenant une colonne Date au format Date
#' @keywords donnees
#' @import lubridate
#' @export
#' @examples
#' formatage.annee.biologique(data)

formatage.annee.biologique <- function(data)
  {

  #library(aquatools);library(lubridate)
  #data <- chronique.mesures("HER0-6", "Thermie")
  #str(data)
  #data$Date <- ymd(data$Date)
  
  data$AnneeBiol <- ifelse(month(data$Date) < 10,year(data$Date),year(data$Date)+1)
  
return(data)

} # Fin de la fonction