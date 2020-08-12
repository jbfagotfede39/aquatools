#' Chroniques existantes
#'
#' Cette fonction permet de visualiser les chroniques existantes et leur statut de validation
#' @name chronique.duree
#' @param Data Jeu de données issu de la matrice "Mesures" des chroniques
#' @keywords chronique
#' @import lubridate
#' @import tidyverse
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
    mutate(chmes_date = ymd(chmes_date))

gg <- ggplot(Mesures, aes(x=chmes_date, y = fct_reorder(chmes_coderhj, chmes_date), color = chmes_validation))
gg <- gg + geom_line()
gg <- gg + labs(y = "Station", x = "Date", colour = "Validation") # Pour changer le titre
gg <- gg + theme_bw()
gg

  ## Affichage des résultats ##
  return(gg)
}