#' Collecte de données à partir d'un flux WFS
#'
#' Cette fonction permet de collecter des données à partir d'un flux WFS
#' @name SIG.flux.wfs
#' @param X Coordonnée X à rechercher (Lambert 93)
#' @param Y Coordonnée Y à rechercher (Lambert 93)
#' @import glue
#' @import httr
#' @import ows4R
#' @import sf
#' @import tidyverse
#' @param url URL de base du flux - Exemple : \code{https://data.geopf.fr/wfs/}
#' @param dataset Jeu de données recherché - Exemple : \code{ADMINEXPRESS-COG.LATEST:departement}
#' @param champ Champ concerné si recherche d'une valeur particulière - Exemple : \code{insee_dep}
#' @param valeur Valeur concernée si recherche d'une valeur particulière - Exemple : \code{39}
#' @export
#' @examples
#' SIG.flux.wfs("https://data.geopf.fr/wfs/", "ADMINEXPRESS-COG.LATEST:departement")
#' SIG.flux.wfs("https://data.geopf.fr/wfs/", "ADMINEXPRESS-COG.LATEST:departement", "insee_dep", "39")

SIG.flux.wfs <- function(
  url = NA_character_,
  dataset = NA_character_,
  champ = NA_character_,
  valeur = NA_character_
    )
{

  #### Test de cohérence ####
  if(!is.na(champ) & is.na(valeur)) stop(glue("La valeur de {champ} doit forcément être complétée"))
  if(is.na(champ) & !is.na(valeur)) stop(glue("Le champ de {valeur} doit forcément être complété"))
  
  #### Nettoyage & reformatage ####
  wfs_url <- glue("WFS:{url}wfs?service=wfs&version=2.0.0&REQUEST=GetFeature&TYPENAME={dataset}&outputFormat=application/json&SRSName=epsg:4326")
  
  #### Filtrage ####
  if(!is.na(champ)){
    wfs_url <- glue("{wfs_url}&CQL_FILTER={champ}={valeur}")
  }
  
  #### Calcul ####
  data_to_add <- st_read(dsn = wfs_url) %>% st_transform(2154)

  #### Sortie des données ####
  return(data_to_add)
  
} # Fin de la fonction