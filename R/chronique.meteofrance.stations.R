#' Liste des stations Météo-France
#'
#' Cette fonction permet de lister les stations Météo-France via l'API
#' @name chronique.meteofrance.stations
#' @param type Fréquence de mesure recherchée : données à pas de temps (\code{Quotidien}) par défaut, \code{6_minutes}, \code{Horaire})
#' @param departement_insee Code Insee du département recherché
#' @param typemesure Type de mesure recherché : \code{Précipitations} par défaut, \code{Température}, \code{Humidité}, \code{Vent}, \code{Pression}, \code{Rayonnement}, \code{Insolation}, \code{État de la mer} et \code{ETP quotidienne}
#' @param stations_actives Filtrage des stations actives \code{TRUE} par défaut, \code{FALSE}
#' @param token Token \code{API Key} créé dans son compte personnel du site \url{https://portail-api.meteofrance.fr}
#' @import glue
#' @import httr2
#' @import jsonlite
#' @import tidyverse
#' @export
#' @examples
#' chronique.meteofrance.stations("Quotidien", 39, "Précipitations", token)

chronique.meteofrance.stations <- function(
    type = c("6_minutes", "Horaire", "Quotidien"),
    departement_insee = NA_integer_,
    typemesure = c("Précipitations", "Température", "Humidité", "Vent", "Pression", "Rayonnement", "Insolation", "État de la mer", "ETP quotidienne"),
    stations_actives = TRUE,
    token = NA_character_
    
)
{
  #### Évaluation des choix ####
  type <- match.arg(type)
  typemesure <- match.arg(typemesure)
  
  #### Test de cohérence ####
  if(is.na(type)) stop("Aucun type de mesure fourni")
  if(is.na(departement_insee)) stop("Aucune département fourni")
  if(is.na(typemesure)) stop("Aucun typemesure fourni")
  if(is.na(token)) stop("Aucun token fourni")

  #### Collecte des données ####
  ##### Correspondance des paramètres #####
  type_frequence <-
    tribble(~type_sale, ~type_propre,
            "6_minutes", "infrahoraire-6m",
            "Horaire", "horaire",
            "Quotidien", "quotidienne"
            )
  type_frequence_retenu <- type_frequence %>% filter(type_sale == type) %>% pull(type_propre)

  typemesure_liste <-
    tribble(~typemesure_sale, ~typemesure_propre,
            "Précipitations", "precipitation",
            "Température", "temperature",
            "Humidité", "humidite",
            "Vent", "vent",
            "Pression", "pression",
            "Rayonnement", "Pression",
            "Insolation", "insolation",
            "État de la mer", "etat_mer",
            "ETP quotidienne", "evapo"
            )
  typemesure_retenu <- typemesure_liste %>% filter(typemesure_sale == typemesure) %>% pull(typemesure_propre)
  
  ##### Construction de l'URL #####
  requete_commande <-
    request("https://public-api.meteofrance.fr/public/DPClim/v1/liste-stations/") %>% 
    req_method("GET") %>% 
    req_url_path_append(glue("{type_frequence_retenu}?")) %>% 
    req_url_query("id-departement" = departement_insee) %>% 
    req_url_query("parametre" = typemesure_retenu) %>%
    req_headers(
      accept = "*/*",
      apikey = token
    )
  
  ##### Collecte à proprement parler #####
  resultat <- 
    requete_commande %>% 
    req_perform()
  
  #### Vérification ####
  if(resultat$status_code != 200) stop(glue("Erreur {resultat$status_code} : problème au moment de la requête de passage de la commande"))
  
  #### Nettoyage & reformatage ####
  ##### Mise en forme #####
  data_to_add <- 
    resultat %>% 
    httr2::resp_body_string() %>% 
    jsonlite::fromJSON()
  
  ##### Filtrage #####
  if(stations_actives == F) data_to_add_v2 <- data_to_add
  if(stations_actives == T){
    data_to_add_v2 <-
      data_to_add %>% 
      filter(posteOuvert == "TRUE")
  }
  
  #### Sortie des données ####
  return(data_to_add_v2)

} # Fin de la fonction
