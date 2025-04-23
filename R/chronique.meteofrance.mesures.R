#' Commande de données Météo-France
#'
#' Cette fonction permet de commander des données Météo-France via l'API
#' @name chronique.meteofrance.mesures
#' @param type Type de mesure recherche : données à pas de temps (\code{Quotidien}) par défaut, \code{6_minutes}, \code{Horaire}, \code{Décadaire}, \code{Mensuel})
#' @param station Identifiant Météo-France de la station concernée
#' @param date_debut_obs Date de début des mesures à collecter (exprimée en TU), au format ISO 8601 avec TZ UTC : \code{AAAA-MM-JJT00:00:00Z}
#' @param date_fin_obs Date de fin des mesures à collecter (exprimée en TU), au format ISO 8601 avec TZ UTC : \code{AAAA-MM-JJT00:00:00Z}
#' @param token Token \code{API Key} créé dans son compte personnel du site \url{https://portail-api.meteofrance.fr}
#' @import glue
#' @import httr2
#' @import tidyverse
#' @export
#' @examples
#' chronique.meteofrance.mesures("39159002", "2025-01-01T00:00:00Z", "2025-04-01T00:00:00Z", token)
#' chronique.meteofrance.mesures("Horaire", "39159002", "2025-01-01 00:00:00", "2025-04-01T00:00:00Z", token)
#' chronique.meteofrance.mesures("Quotidien", "39159002", "2025-01-01T00:00:00Z", now() - days(1), token)
#' chronique.meteofrance.mesures("Mensuel", "39159002", "2025-01-01 00:00:00", now() - months(1), token)

chronique.meteofrance.mesures <- function(
    type = c("6_minutes", "Horaire", "Quotidien", "Décadaire", "Mensuel"),
    station = NA_character_,
    date_debut_obs = NA_character_,
    date_fin_obs = NA_character_,
    token = NA_character_
    
)
{
  #### Évaluation des choix ####
  type <- match.arg(type)
  
  #### Test de cohérence ####
  if(is.na(type)) stop("Aucun type de mesure fourni")
  if(type == "6_minutes") warning("Attention : au pas de temps 6 minutes, les timecodes de début et de fin doivent être des multiples de 6 minutes")
  if(is.na(station)) stop("Aucune station fournie")
  if(is.na(date_debut_obs)) stop("Aucun timecode de début de série fourni")
  if(is.na(date_fin_obs)) stop("Aucun timecode de fin de série fourni")
  if(is.na(token)) stop("Aucun token fourni")

  #### Collecte des données ####
  ##### Nettoyage & reformatage #####
  date_debut_obs <- format_ISO8601(ymd_hms(date_debut_obs)) %>% glue("Z") # Permet une saisie plus souple
  date_fin_obs <- format_ISO8601(ymd_hms(date_fin_obs)) %>% glue("Z") # Permet une saisie plus souple

  ##### Correspondance des paramètres #####
  type_mesures <-
    tribble(~type_sale, ~type_propre,
            "6_minutes", "infrahoraire-6m",
            "Horaire", "horaire",
            "Quotidien", "quotidienne",
            "Décadaire", "decadaire",
            "Mensuel", "mensuelle"
            )
  type_mesures_retenu <- type_mesures %>% filter(type_sale == type) %>% pull(type_propre)
  
  ##### Construction de l'URL #####
  requete_commande <-
    request("https://public-api.meteofrance.fr/public/DPClim/v1/commande-station/") %>% 
    req_method("GET") %>% 
    req_url_path_append(glue("{type_mesures_retenu}?")) %>% 
    req_url_query("id-station" = station) %>% 
    req_url_query("date-deb-periode" = date_debut_obs) %>% 
    req_url_query("date-fin-periode" = date_fin_obs) %>% 
    req_headers(
      accept = "*/*",
      apikey = token
    )
  
  ##### Collecte à proprement parler #####
  resultat <- 
    requete_commande %>% 
    req_perform()
  
  #### Vérification ####
  if(resultat$status_code != 202) stop(glue("Erreur {resultat$status_code} : problème au moment de la requête de passage de la commande"))
  
  #### Nettoyage & reformatage ####
  id_commande <- 
    resultat %>% 
    resp_body_json() %>% 
    pluck(1) %>% 
    pluck(1)
  
  #### Sortie des données ####
  return(id_commande)

} # Fin de la fonction
