#' Collecte de données de hydrologiques depuis Hub'eau
#'
#' Cette fonction permet de collecter de données d'hydrologie depuis Hub'eau
#' @name hydrologie.hubeau
#' @param type Type de mesure recherche : débits moyens journaliers QmnJ (\code{qmnj}) par défaut, débits moyens mensuels (QmM) (\code{qmm}), Hauteur instantanée maximale mensuelle (HIXM) (\code{hixm}), Hauteur instantanée maximale journalière (HIXnJ) (\code{hixnj}), Débit instantané minimal mensuel (QINM) (\code{qinm}), Débit instantané minimal journalier (QINnJ) (\code{qinnj}), Débit instantané maximal mensuel (QixM) (\code{qixm}), Débit instantané maximal journalier (QIXnJ) (\code{qixnj}), temps réel (\code{tr})
#' @param codesie Code SIE de la station concernée
#' @param date_debut_obs Date de début des mesures à collecter (exprimée en TU), les formats de date (ISO 8601) supportés : \code{yyyy-MM-dd}, \code{yyyy-MM-dd'T'HH:mm:ss}, \code{yyyy-MM-dd'T'HH:mm:ssXXX}, exemples : \code{2018-12-01}, \code{2018-12-11T00:00:01}, \code{2018-12-11T00:00:01Z}
#' @param date_fin_obs Date de fin des mesures à collecter (exprimée en TU), les formats de date (ISO 8601) supportés : \code{yyyy-MM-dd}, \code{yyyy-MM-dd'T'HH:mm:ss}, \code{yyyy-MM-dd'T'HH:mm:ssXXX}, exemples : \code{2018-12-01}, \code{2018-12-11T00:00:01}, \code{2018-12-11T00:00:01Z}
#' @param bbox Zone géographique concernée, au format bbox généré  par sf, en WGS84/EPSG 4326
#' @import glue
#' @import httr
#' @import tibble
#' @import tidyverse
#' @export
#' @examples
#' hydrologie.hubeau("qmnj", "V244402001", "2025-04-01")
#' hydrologie.hubeau("qmnj", "V231000201", "2025-04-01", "2025-04-10")
#' hydrologie.hubeau("tr", "V231000201", today() - days(1)) %>% view()

hydrologie.hubeau <- function(
    type = c("qmnj", "qmm", "hixm", "hixnj", "qinm", "qinnj", "qixm", "qixnj", "tr"),
    codesie = NA_character_,
    date_debut_obs = NA_character_,
    date_fin_obs = NA_character_,
    bbox = NA
    
)
{
  #### Évaluation des choix ####
  type <- match.arg(type)
  
  #### Test de cohérence ####
  if(all(is.na(type) & is.na(codesie) & is.na(date_debut_obs) & is.na(date_fin_obs))) stop("Aucun paramètre fourni")
  if(type == "tr") {
    if(date_debut_obs < (today() - months(1))) stop("Données temps réel disponibles uniquement sur une période de 30 jours à partir d'aujourd'hui")
  }
  
  #### Collecte des données ####
  ##### Correspondance des paramètres #####
  type_mesures <-
    tribble(~type_sale, ~type_propre,
            "qmnj", "QmnJ",
            "qmm", "QmM",
            "hixm", "HIXM",
            "hixnj", "HIXnJ",
            "qinm", "QINM",
            "qinnj", "QINnJ",
            "qixm", "QixM",
            "qixnj", "QIXnJ"
            )
  type_mesures_retenu <- type_mesures %>% filter(type_sale == type) %>% pull(type_propre)
  
  ##### Construction de l'URL #####
  if(type == "tr") url_base <- "https://hubeau.eaufrance.fr/api/v2/hydrometrie/observations_tr.csv?size=20000" else url_base <- "https://hubeau.eaufrance.fr/api/v2/hydrometrie/obs_elab.csv?size=20000"
  if(type != "tr") url_base <- glue("{url_base}&grandeur_hydro_elab={type_mesures_retenu}")
  
  url <- url_base
  if(!is.na(codesie)) url <- glue("{url}&code_entite={codesie}")
  if(!is.na(date_debut_obs) & type == "tr") url <- glue("{url}&date_debut_obs={date_debut_obs}")
  if(!is.na(date_debut_obs) & type != "tr") url <- glue("{url}&date_debut_obs_elab={date_debut_obs}")
  if(!is.na(date_fin_obs) & type == "tr") url <- glue("{url}&date_fin_obs={date_fin_obs}")
  if(!is.na(date_fin_obs) & type != "tr") url <- glue("{url}&date_fin_obs_elab={date_fin_obs}")
  if(!is.na(bbox)) url <- glue("{url}&bbox={bbox$xmin}%2C{bbox$ymin}%2C{bbox$xmax}%2C{bbox$ymax}")
  
  ##### Collecte à proprement parler #####
  data_to_import <- GET(url)
  if(http_status(data_to_import)$category != "Success") stop(glue("Problème dans l'exécution de la requête : '{http_status(data_to_import)$message}'"))
  
  data_to_add <- 
    data_to_import %>% 
    content(type = "text") %>%
    read_csv2() %>% 
    {if("date_obs_elab" %in% names(.)) rename(., date_obs = date_obs_elab) else .} %>% 
    {if("resultat_obs_elab" %in% names(.)) rename(., resultat_obs = resultat_obs_elab) else .} %>% 
    mutate(resultat_obs = as.numeric(resultat_obs)) # Dans le cas de données vide -> format de colonne à character et jointure impossible avec un map
  
  #### Traitement des données ####
  ##### Tri #####
  data_to_add_2 <-
    data_to_add #%>% 
    # arrange(libelle_station_hydrobio, desc(date_prelevement))
  
  #### Sortie des données ####
  return(data_to_add_2)

} # Fin de la fonction
