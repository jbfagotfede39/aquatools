#' Calcul des horaires de soleil
#'
#' Fait le calcul des horaires de présence de soleil, dont horaires autorisés de pêche
#' @name soleil.horaires
#' @param latitude Latitude concernée
#' @param longitude Longitude concernée
#' @param date_start Date de début de la période concernée
#' @param date_end Date de fin de la période concernée
#' @import lubridate
#' @import suncalc
#' @import tibble
#' @import tidyverse
#' @export
#' @examples 
#' soleil.horaires(48.85331560269666, 2.3456499560163597, "2026-01-01", "2026-12-31")
#' soleil.horaires(46.84015684582083, 5.7054853214538115, "2026-01-01", "2026-12-31")

soleil.horaires <- function(  
    latitude = NA_real_,
    longitude = NA_real_,
    date_start = NA_character_,
    date_end = NA_character_
    )
{

  #### Test de cohérence ####
  if(is.na(latitude) == T) stop("Une latitude doit être fournie")
  if(is.na(longitude) == T) stop("Une longitude doit être fournie")
  if(is.na(date_start) == T) stop("Une date_start doit être fournie")
  if(is.na(date_end) == T) stop("Une date_end doit être fournie")
  
  #### Calcul ####
  dates <- seq(ymd(date_start), ymd(date_end), by = "day")
  
  resultats_v1 <-
    dates %>% 
    getSunlightTimes(latitude, longitude, tz = "Europe/Paris") %>% 
    as_tibble()
  
  resultats_v2 <-
    resultats_v1 %>% 
    mutate(peche_debut = sunrise - minutes(30)) %>% 
    mutate(peche_fin = sunset + minutes(30))
  
  resultats_v3 <-
    resultats_v2 %>% 
    mutate(jour = format(date, format = "%a"), .after = date) %>% 
    mutate(jour = str_sub(jour, 0, 1))
  
  data_sortie <- resultats_v3
  
  #### Sortie ####
  return(data_sortie)

} # Fin de la fonction