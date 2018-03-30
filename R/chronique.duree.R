#' Chroniques existantes
#'
#' Cette fonction permet de visualiser les chroniques existantes
#' 
#' @param Data Jeu de données issu de la matrice "Mesures" des chroniques
#' @keywords chronique
#' @import dplyr lubridate forcats
#' @export
#' @examples
#' chronique.duree(Mesures)

##### TODO LIST #####
# Impossible de trouver comment classer l'axe des Y (2018-03-27)
#####################

chronique.duree <- function(Mesures)
  
{
  Mesures <-
    Mesures %>% 
    mutate(Date = ymd(Date))

gg <- ggplot(Mesures, aes(x=Date, y = fct_reorder(CodeRDT, Date), color = Validation))
gg <- gg + geom_line()
gg <- gg + labs(y = "Station") # Pour changer le titre
gg <- gg + theme_bw()
gg

  ## Affichage des résultats ##
  return(gg)
}