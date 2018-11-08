#' Reformatage de dates en année biologique
#'
#' Reformate les dates selon l'année biologique (en rapport au 1 octobre de chaque année) dans une colonne AnneeBiol
#' @name formatage.annee.biologique
#' @param data Jeu de données contenant une colonne Date au format Date
#' @keywords donnees
#' @import lubridate
#' @export
#' @examples
#' formatage.annee.biologique(data)

formatage.annee.biologique <- function(data)
  {

##### TODO LIST #####
# 
#####################
  
  #library(aquatools);library(lubridate)
  #data <- chronique.mesures("HER0-6", "Thermie")
  #str(data)
  #data$Date <- ymd(data$Date)
  
  data$AnneeBiol <- ifelse(month(data$Date) < 10,year(data$Date),year(data$Date)+1)
  
return(data)

} # Fin de la fonction