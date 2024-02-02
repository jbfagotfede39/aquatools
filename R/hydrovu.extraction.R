#' Extraction en masse des données de l'API HydroVu
#'
#' Cette fonction permet d'extraire en masse les données disponibles l'API HydroVu
#' @name hydrovu.extraction
#' @param token Identifiant de l'utilisateur autorisé sur l'API
#' @param id Id du site recherché sur l'API
#' @param time_start Date et heure de début de la chronique recherchée (format epoch \code{1683885600} ou ISO 8601 \code{2023-03-12 12:00:00})
#' @param time_end Date et heure de fin de la chronique recherchée (format epoch \code{1683885600} ou ISO 8601 \code{2023-03-12 12:00:00})
#' @keywords chronique
#' @import tidyverse
#' @export 
#' @examples
#' tibble(chmes_coderhj = "4722100874248192", time = seq(ymd_hms('2022-01-01 00:00:00'), ymd_hms('2022-10-31 00:00:00'), by='2 hour')) %>% head(25) %>% group_split(chmes_coderhj, time) %>% map(~ hydrovu.extraction(token, .$chmes_coderhj, .$time)) %>% list_rbind() 

hydrovu.extraction <- function(
    token = NA_character_,
    id = NA_character_,
    time_start = NA,
    time_end = NA
  )
{

  #### Calcul ####
  data <-
    hydrovu.donnees(token, id, time_start, time_end) %>%
    hydrovu.refactoring()

  #### Sortie ####
  return(data)
  
} # Fin de la fonction