#' Chroniques existantes
#'
#' Cette fonction permet de visualiser les chroniques existantes
#' 
#' @param Data Jeu de données issu de la matrice "Mesures" des chroniques
#' @keywords chronique
#' @import dplyr lubridate
#' @export
#' @examples
#' chronique.duree(Mesures)

##### TODO LIST #####
# 
#####################

chronique.duree <- function(Mesures)
  
{

#Mesures <-
  #Mesures %>% 
  #filter(grepl("DRO", CodeRDT)) %>% 
  #filter(TypeMesure == "Thermie")

  Mesures$Date <- ymd(Mesures$Date)

gg <- ggplot(Mesures, aes(x=Date, y = CodeRDT, color = Validation))
gg <- gg + geom_line()
gg

  ## Affichage des résultats ##
  return(gg)
}