#' Collecte des stations hydrologiques depuis Hub'eau
#'
#' Cette fonction permet de collecter les stations de suivi hydrologique depuis Hub'eau
#' @name hydrologie.hubeau.stations
#' @param departement_insee Code INSEE du département concerné
#' @param commune_insee Code INSEE de la commune concernée
#' @param region_insee Code INSEE de la région concernée
#' @param bbox Zone géographique concernée, au format bbox généré par sf, en WGS84/EPSG 4326
#' @import glue
#' @import httr2
#' @import sf
#' @import tibble
#' @import tidyverse
#' @export
#' @examples
#' hydrologie.hubeau.stations(39)
#' hydrologie.hubeau.stations(departement_insee = 25)
#' hydrologie.hubeau.stations(commune_insee = 39435)
#' hydrologie.hubeau.stations(region_insee = 27)

hydrologie.hubeau.stations <- function(
    departement_insee = NA_character_,
    commune_insee = NA_character_,
    region_insee = NA_character_,
    bbox = NA
    
)
{
  #### Évaluation des choix ####
  # type <- match.arg(type)
  
  #### Test de cohérence ####
  ##### Aucun paramètre saisi #####
  if(all(is.na(departement_insee) & is.na(commune_insee) & is.na(region_insee) & is.na(bbox))) stop("Aucun paramètre fourni")
  ##### Plusieurs paramètres saisis #####
  # Compter le nombre de paramètres non NA
  params_non_na <- sum(!is.na(departement_insee),
                       !is.na(commune_insee),
                       !is.na(region_insee),
                       !is.na(bbox))
  
  # Arrêter la fonction si plus d'un paramètre n'est pas NA
  if (params_non_na > 1) {
    stop("Plus d'un paramètre de sélection saisi : il n'en faut qu'un unique")
  }

  #### Collecte des données ####
  ##### Construction de l'URL #####
  requete_commande <-
    request("https://hubeau.eaufrance.fr/api/v2/hydrometrie/referentiel/stations.csv?size=500") %>% 
    req_method("GET") %>% 
    # {if(!is.na(departement_insee)) req_url_path_append(., glue("code_departement={departement_insee}")) else .} %>% 
    {if(!is.na(departement_insee)) req_url_query(., "code_departement" = departement_insee) else .} %>% 
    {if(!is.na(commune_insee)) req_url_query(., "code_commune_station" = commune_insee) else .} %>% 
    {if(!is.na(region_insee)) req_url_query(., "code_region" = region_insee) else .} %>% 
    {if(!is.na(bbox)) req_url_query(., "bbox" = bbox) else .}
  
  ##### Collecte à proprement parler #####
  resultat <- 
    requete_commande %>% 
    req_perform()

  #### Vérification ####
  if(resultat$status_code != 200 & resultat$status_code != 206) stop(glue("Erreur {resultat$status_code} : problème dans la requête de passage de la commande"))
  if(resultat$status_code == 206) warning(glue("Erreur 206 : il reste des résultats"))
  
  #### Nettoyage & reformatage ####
  data_to_add <- 
    resultat %>% 
    resp_body_string() %>% 
    read_csv2()
  
  #### Traitement des données ####
  ##### Spatialisation #####
  data_to_add_2 <-
    data_to_add %>% 
    sf::st_as_sf(coords = c("coordonnee_x_station","coordonnee_y_station"), remove = FALSE) %>% 
    st_set_crs(2154)
  
  #### Sortie des données ####
  return(data_to_add_2)

} # Fin de la fonction
