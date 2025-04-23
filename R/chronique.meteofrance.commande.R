#' Collecte d'une commande de données Météo-France
#'
#' Cette fonction permet de collecter les données Météo-France à partir d'un numéro de commande
#' @name chronique.meteofrance.commande
#' @param id_commande Identifiant de commande, issu de la fonction \code{chronique.meteofrance.mesures}
#' @param token Token \code{API Key} créé dans son compte personnel du site \url{https://portail-api.meteofrance.fr}
#' @param temporisation Délai (en secondes) de temporisation après l'envoi de la commande, afin de réduire les erreurs 204 de l'API suite à une demande de téléchargement trop rapide
#' @import glue
#' @import httr2
#' @import tidyverse
#' @export
#' @examples
#' chronique.meteofrance.commande("2025003064730", token)
#' chronique.meteofrance.mesures("Quotidien", "39159002", "2025-01-01T00:00:00Z", "2025-04-01T00:00:00Z", token) %>% chronique.meteofrance.commande(token) %>% chronique.meteofrance.nettoyage()

chronique.meteofrance.commande <- function(
    id_commande = NA_character_,
    token = NA_character_,
    temporisation = 2
    
)
{
  #### Évaluation des choix ####
  # type <- match.arg(type)
  
  #### Test de cohérence ####
  if(is.na(id_commande)) stop("Aucun id_commande fourni")
  if(is.na(token)) stop("Aucun token fourni")
  
  #### Collecte des données ####
  Sys.sleep(temporisation) # Nécessaire car si on enchaîne les deux directement dans un piping sans ce délai on obtient une erreur 204 de l'API : "Production encore en attente ou en cours"
  
  ##### Construction de l'URL #####
  requete_collecte <-
    request("https://public-api.meteofrance.fr/public/DPClim/v1/commande/fichier") %>% 
    req_method("GET") %>%
    req_url_query("id-cmde" = id_commande) %>%
    req_headers(
      accept = "*/*",
      apikey = token,
    )
  
  ##### Collecte à proprement parler #####
  resultat <- 
    requete_collecte %>% 
    req_perform()
  
  #### Vérification ####
  if(resultat$status_code != 201) stop(glue("Erreur {resultat$status_code} : problème au moment de la requête de passage de la commande"))
  
  #### Nettoyage & reformatage ####
  ##### Extraction #####
  data_to_add <- 
    resultat %>% 
    resp_body_string() %>% 
    read_csv2()

  #### Sortie des données ####
  return(data_to_add)

} # Fin de la fonction
