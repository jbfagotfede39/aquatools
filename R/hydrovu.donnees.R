#' Collecte des données de l'API HydroVu
#'
#' Cette fonction permet de collecter les données disponibles sur l'API HydroVu
#' @name hydrovu.donnees
#' @param token Identifiant de l'utilisateur autorisé sur l'API
#' @param id Id du site recherché sur l'API
#' @param time_start Date et heure de début de la chronique recherchée (format epoch \code{1683885600} ou ISO 8601 \code{2023-03-12 12:00:00})
#' @param time_end Date et heure de fin de la chronique recherchée (format epoch \code{1683885600} ou ISO 8601 \code{2023-03-12 12:00:00})
#' @keywords chronique
#' @import checkmate
#' @import glue
#' @import httr
#' @import tidyverse
#' @export 
#' @examples
#' hydrovu.donnees(token, "4722100874248192")
#' "4722100874248192" %>% hydrovu.donnees(token, .)
#' hydrovu.authentification(id_user, id_key) %>% hydrovu.donnees("4722100874248192")
#' hydrovu.authentification(id_user, id_key) %>% hydrovu.donnees("4722100874248192", 1683885600, 1683892800)
#' hydrovu.donnees("4722100874248192", 1683885600, 1683892800) %>% # Ok
#' hydrovu.donnees("4722100874248192", "2023-05-12 10:00:00", 1683892800) %>% 
#' hydrovu.donnees("4722100874248192", 1683880600, "2023-05-12 12:00:00") %>%
#' hydrovu.donnees("4722100874248192", "2023-05-12 10:00:00", "2023-05-12 12:00:00")

hydrovu.donnees <- function(
    token = NA_character_,
    id = NA_character_,
    time_start = NA,
    time_end = NA
  )
{
  #### Test de cohérence ####
  if(exists("token") == FALSE) stop("Pas de token fourni")
  if(checkmate::checkR6(token) == FALSE) stop("Token fourni pas au format R6")
  if(is.na(id)) stop("Pas d'id_user fourni")
  
  #### Nettoyage & reformatage ####
  if(is.na(time_start)) warning("Pas de time_start ou time_start vide")
  if(is.numeric(time_start) == TRUE) time_start_2 <- time_start
  if(is.numeric(time_start) == FALSE) time_start_2 <- as.numeric(as.POSIXct(time_start))
  
  if(is.na(time_end) & !is.na(time_start)) time_end <- time_start_2 + 86400 # 24 heures
  if(is.numeric(time_end) == TRUE) time_end_2 <- time_end
  if(is.numeric(time_end) == FALSE) time_end_2 <- as.numeric(as.POSIXct(time_end))
  
  if(!is.na(time_start_2) && time_start_2 < 1200000000) stop("time_start pas au bon format") # 1 200 000 000
  if(!is.na(time_end_2) && time_end_2 < 1200000000) stop("time_end pas au bon format") # 1 200 000 000
  
  if(!is.na(time_start_2) && !is.na(time_end_2) && time_end_2 < time_start_2) stop("time_start après time_end")
  
  #### Calcul ####
  if(is.na(time_start) & is.na(time_end)) url_to_go <- glue("https://www.hydrovu.com/public-api/v1/locations/{id}/data")
  if(!is.na(time_start) & is.na(time_end)) url_to_go <- glue("https://www.hydrovu.com/public-api/v1/locations/{id}/data?startTime={time_start_2}")
  if(is.na(time_start) & !is.na(time_end)) url_to_go <- glue("https://www.hydrovu.com/public-api/v1/locations/{id}/data?endTime={time_end_2}")
  if(!is.na(time_start) & !is.na(time_end)) url_to_go <- glue("https://www.hydrovu.com/public-api/v1/locations/{id}/data?startTime={time_start_2}&endTime={time_end_2}")

  data <-
    url_to_go %>% 
    httr::GET(url = ., 
              user_agent("Testing Oauth with httr"),
              config(token = token)#,
              #add_headers(`x-isi-start-page` = nextpage)
    ) %>% 
    content()

  #### Sortie ####
  return(data)
  
} # Fin de la fonction