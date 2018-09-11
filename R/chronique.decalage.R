#' Décalage de données brutes de chronique
#'
#' Permet de corriger des données brutes de chronique présentant des valeurs de date et heure aberrantes. ATTENTION : ne gère pas les ID
#' @param data Chronique à modifier, avec un champ Date, un champ Heure et un champ Valeur
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
    mutate(Date2 = ymd_hms(paste0(Date, " ", Heure)) + ddays(decalage)) %>% 
    mutate(Date = round_date(Date2, "hour")) %>% 
    select(-Date2) %>% 
    mutate(Heure = format(Date, format="%H:%M:%S")) %>% 
    mutate(Date = format(Date, format="%Y-%m-%d"))
  
  ##### Retour des données #####
  return(data)
  
} # Fin de la fonction