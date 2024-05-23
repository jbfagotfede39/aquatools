#' Affichage dynamique d'une chronique
#'
#' Cette fonction permet de représenter de manière dynamique une chronique
#' @name chronique.vue.dynamique
#' @param data Jeu de données à représenter
#' @keywords chronique
#' @import dygraphs
#' @import tidyverse
#' @import xts
#' @export
#' @examples
#' chronique.vue.dynamique(data)

chronique.vue.dynamique <- function(
  data = data
)
{
  
  ##### Nettoyage & reformatage #####
  data_v2 <-
    data_to_add %>%
    formatage.time() %>% 
    xts(x = .$chmes_valeur, order.by = .$time) %>% 
    dygraph() %>% 
    dyRangeSelector()

  ##### Sortie #####
  return(data_v2)

} # Fin de la fonction