#' Décalage de données brutes de chronique
#'
#' Permet de corriger des données brutes de chronique présentant des valeurs de date et heure aberrantes. ATTENTION : ne gère pas les ID
#' @name chronique.decalage
#' @param data Chronique à modifier, avec un champ chmes_date, un champ chmes_heure et un champ chmes_valeur
#' @param decalage Valeur de décalage (en jours, positif pour avancer ou négatif pour reculer) à calculer par soustration des deux dates au format lubridate
#' @param recalcul Les dates/heures présentes doivent-elles être entièrement re-calculées, et si oui, depuis le début ou la fin avec l'écart considéré (\code{non} (par défaut), \code{debut} ou \code{fin})
#' @keywords chronique
#' @import dplyr 
#' @import lubridate
#' @export
#' @examples
#' ymd_hms("2017-07-05 11:40:00")-ymd_hms("2010-05-24 21:44:00")
#' chronique.decalage(data, decalage = 2598.581)
#' chronique.decalage(decalage = -57)
#' chronique.decalage(decalage = -57, recalcul == "debut") # On met un décalage pour que la date et l'heure de la première ligne soit correct
#' chronique.decalage(decalage = -57, recalcul == "fin") # On met un décalage pour que la date et l'heure de la dernière ligne soit correct

chronique.decalage <- function(data, 
                               decalage = 2598.581, 
                               recalcul = c('non', 'debut', 'fin'))
{

  #### Évaluation des choix ####
  recalcul <- match.arg(recalcul)
  
  ##### Calcul du décalage #####
data <-
  data %>% 
    mutate(chmes_date2 = ymd_hms(paste0(chmes_date, " ", chmes_heure)) + ddays(decalage)) %>% 
    mutate(chmes_date = round_date(chmes_date2, "hour")) %>% 
    select(-chmes_date2) %>% 
    mutate(chmes_heure = format(chmes_date, format="%H:%M:%S")) %>% 
    mutate(chmes_date = format(chmes_date, format="%Y-%m-%d"))

  ##### Éventuel recalcul complet des dates ####
if(recalcul != "non"){
  data <-
    data %>% 
    mutate(time = ymd_hms(paste0(chmes_date, " ", chmes_heure)))
  
  if(recalcul == "debut"){
    datedebut <- data %>% mutate(time = ymd_hms(paste0(chmes_date, " ", chmes_heure))) %>% filter(row_number() == 1) %>% select(time) %>% pull() %>% round_date("hour")
    
    data <-
      data %>% 
      add_column(Time = seq(datedebut, datedebut %m+% hours(nrow(.)-1), by = '1 hour'))
  }
  
  if(recalcul == "fin"){
    datefin <- data %>% mutate(time = ymd_hms(paste0(chmes_date, " ", chmes_heure))) %>% filter(row_number() == max(row_number())) %>% select(time) %>% pull() %>% round_date("hour")
    
    data <-
      data %>% 
      add_column(Time = seq(datefin %m-% hours(nrow(.)-1), datefin, by = '1 hour'))
  }
  
  data <-
    data %>% 
    mutate(chmes_heure = format(Time, format="%H:%M:%S")) %>% 
    mutate(chmes_date = format(Time, format="%Y-%m-%d")) %>% 
    select(-Time, -time)
}
  
  ##### Retour des données #####
  return(data)
  
} # Fin de la fonction