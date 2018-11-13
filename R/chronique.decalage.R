#' Décalage de données brutes de chronique
#'
#' Permet de corriger des données brutes de chronique présentant des valeurs de date et heure aberrantes. ATTENTION : ne gère pas les ID
#' @name chronique.decalage
#' @param data Chronique à modifier, avec un champ chmes_date, un champ chmes_heure et un champ chmes_valeur
#' @param decalage Valeur de décalage à calculer par soustration des deux dates au format lubridate
#' @keywords chronique
#' @import dplyr 
#' @import lubridate
#' @export
#' @examples
#' ymd_hms("2017-07-05 11:40:00")-ymd_hms("2010-05-24 21:44:00")
#' chronique.decalage(data, decalage = 2598.581)

chronique.decalage <- function(data, decalage = 2598.581)
{

  ##### Calcul du décalage #####
data <-
  data %>% 
    mutate(chmes_date2 = ymd_hms(paste0(chmes_date, " ", chmes_heure)) + ddays(decalage)) %>% 
    mutate(chmes_date = round_date(chmes_date2, "hour")) %>% 
    select(-chmes_date2) %>% 
    mutate(chmes_heure = format(chmes_date, format="%H:%M:%S")) %>% 
    mutate(chmes_date = format(chmes_date, format="%Y-%m-%d"))
  
  ##### Retour des données #####
  return(data)
  
} # Fin de la fonction