#' Création de bruit temporel dans données brutes de chronique
#'
#' Permet de créer du bruit temporel dans des données brutes de chroniques
#' @name chronique.bruit.temps
#' @param data Chronique à modifier, avec un champ chmes_date, un champ chmes_heure et un champ chmes_valeur
#' @param bruit_max Durée maximale de bruit (positif et négatif, en minutes) - \code{60} par défaut
#' @param recalcul Les dates/heures présentes doivent-elles être entièrement re-calculées, et si oui, depuis le début ou la fin avec l'écart considéré (\code{non} (par défaut), \code{debut} ou \code{fin})
#' @keywords chronique
#' @import tidyverse
#' @export
#' @examples
#' mesures_2 %>% chronique.bruit.temps()
#' mesures_2 %>% chronique.bruit.temps(30)

chronique.bruit.temps <- function(data, 
                                  bruit_max = 60
                                  )
{
  
  ##### Calcul du décalage #####
  data_2 <- 
    data %>% 
    formatage.time() %>% 
    rowwise() %>% 
    mutate(time_2 = time + minutes(sample(c(-1,1), 1) * sample.int(bruit_max, 1)), .after = "time") %>%
    ungroup() %>% 
    mutate(chmes_heure = format(time_2, format="%H:%M:%S"), .after = "time_2") %>%
    select(-time, -time_2)
  
  ##### Retour des données #####
  return(data_2)
  
} # Fin de la fonction