#' Collecte des sites de l'API HydroVu
#'
#' Cette fonction permet de collecter les sites disponibles sur l'API HydroVu
#' @name hydrovu.sites
#' @param token Identifiant de l'utilisateur autorisé sur l'API
#' @keywords chronique
#' @import checkmate
#' @import glue
#' @import httr
#' @export 
#' @examples
#' hydrovu.sites(token)
#' hydrovu.authentification(id_user, id_key) %>% hydrovu.sites()

hydrovu.sites <- function(
    token = NA_character_
  )
{
  #### Test de cohérence ####
  if(exists("token") == FALSE) stop("Pas de token fourni")
  if(checkmate::checkR6(token) == FALSE) stop("Token fourni pas au format R6")
  
  #### Calcul ####
  sites <-
    "https://www.hydrovu.com/public-api/v1/locations/list" %>% 
    httr::GET(url = ., 
              user_agent("Testing Oauth with httr"),
              config(token = token)#,
              #add_headers(`x-isi-start-page` = nextpage)
    ) %>% 
    content() %>% 
    unlist() %>% 
    enframe() %>% 
    mutate(chsta_coderhj = ifelse(grepl("id", name), value, NA_character_)) %>% 
    fill(chsta_coderhj, .direction = "down") %>% 
    filter(name != "id") %>% 
    pivot_wider(id_cols = c(chsta_coderhj), names_from = "name", values_from = "value") %>% 
    arrange(name)

  #### Sortie ####
  return(sites)
  
} # Fin de la fonction