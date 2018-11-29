#' Reformatage de dates en année biologique
#'
#' Reformate les dates selon l'année biologique (en rapport au 1 octobre de chaque année) dans une colonne chmes_anneebiol
#' @name formatage.annee.biologique
#' @param data Jeu de données contenant une colonne chmes_date ou chsvi_date au format Date
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
  
if("chmes_date" %in% colnames(data)){
  data$chmes_anneebiol <- ifelse(month(data$chmes_date) < 10,year(data$chmes_date),year(data$chmes_date)+1)
  }
  

if("chsvi_date" %in% colnames(data)){
  data$chsvi_anneebiol <- ifelse(month(data$chsvi_date) < 10,year(data$chsvi_date),year(data$chsvi_date)+1)
  }
  
return(data)

} # Fin de la fonction