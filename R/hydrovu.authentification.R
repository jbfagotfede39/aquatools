#' Authentification à l'API HydroVu
#'
#' Cette fonction permet de s'identifier à l'API HydroVu
#' @name hydrovu.authentification
#' @param id_user Identifiant de l'utilisateur autorisé sur l'API
#' @param id_key Clé de l'utilisateur autorisé sur l'API
#' @keywords chronique
#' @import glue
#' @import httr
#' @export 
#' @examples
#' hydrovu.authentification(id_user, id_key)
#' token <- hydrovu.authentification(id_user, id_key)
#' hydrovu.authentification("jbf", keyring::key_get("hydrovu", "jbf"))
#' "jbf" %>% hydrovu.authentification(., keyring::key_get("hydrovu", .))
#' token <- "jbf" %>% hydrovu.authentification(., keyring::key_get("hydrovu", .))

hydrovu.authentification <- function(
    id_user = NA_character_,
    id_key = NA_character_
  )
{
  #### Test de cohérence ####
  if(is.na(id_user)) stop("Pas d'id_user fourni")
  if(is.na(id_key)) stop("Pas d'id_key fourni")
  
  #### Calcul ####
  # Mise en place du endpoint #
  api_endpoint <- 
    httr::oauth_endpoint(
      authorize =  "https://www.hydrovu.com/public-api/oauth/authorize",
      access = "https://www.hydrovu.com/public-api/oauth/token"
    )
  
  # Création de l'application qui permettrat de télécharger les données #
  app_auth <- httr::oauth_app("hydrovu", key = id_user, secret = id_key)
  
  # Création du token qui servira d'authentification #
  token_api = httr::oauth2.0_token(api_endpoint,
                                   app_auth,
                                   scope = "https://www.hydrovu.com/public-api/oauth/token",
                                   client_credentials=TRUE,
                                   cache = FALSE,
                                   config_init = user_agent("Testing Oauth with httr"))
  
  #### Sortie ####
  return(token_api)
  
} # Fin de la fonction