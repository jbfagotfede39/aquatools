#' Reformatage de dates en année biologique
#'
#' Reformate les dates selon l'année biologique (en rapport au 1 octobre de chaque année) dans une colonne chmes_anneebiol
#' @name formatage.annee.biologique
#' @param data Jeu de données contenant une colonne chmes_date au format Date
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
  #data$chmes_date <- ymd(data$chmes_date)
  
  data$chmes_anneebiol <- ifelse(month(data$chmes_date) < 10,year(data$chmes_date),year(data$chmes_date)+1)
  
return(data)

} # Fin de la fonction