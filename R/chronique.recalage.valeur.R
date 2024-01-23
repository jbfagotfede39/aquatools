#' Recalage des valeurs de données brutes de chronique
#'
#' Permet de recaler les valeurs de données brutes de chronique. ATTENTION : ne gère pas les ID
#' @name chronique.recalage.valeur
#' @param data Chronique à modifier, avec un champ chmes_date, un champ chmes_heure et un champ chmes_valeur
#' @param decalage Valeur de décalage. Si \code{vide} (par défaut), utilise la première valeur de la série pour atteindre 0
#' @keywords chronique
#' @import tidyverse
#' @export
#' @examples
#' chronique.recalage.valeur(data)
#' chronique.recalage.valeur(data, decalage = 258.58)
#' chronique.recalage.valeur(decalage = -57)

chronique.recalage.valeur <- function(data, 
                               decalage = NA_real_)
{

  ##### Nettoyage & reformatage #####
  data_v2 <-
    data %>% 
    arrange(chmes_date, chmes_heure)
  
  ##### Calcul #####
  if(is.na(decalage)) decalage <- data_v2 %>% filter(row_number() == 1) %>% pull(chmes_valeur)
  
  data_v3 <-
    data_v2 %>% 
    mutate(chmes_valeur = chmes_valeur - decalage)
  
  ##### Sortie #####
  return(data_v3)
  
} # Fin de la fonction