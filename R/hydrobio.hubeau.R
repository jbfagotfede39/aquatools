#' Collecte de données de hydrobiologiques depuis Hub'eau
#'
#' Cette fonction permet de collecter de données d'hydrobiologie depuis Hub'eau
#' @name hydrobio.hubeau
#' @param departementinsee Code INSEE du département (\code{39} par défaut)
#' @param bbox Zone géographique concernée, au format bbox généré  par sf, en WGS84/EPSG 4326
#' @param indicesandre Code SANDRE de l'indice concerné : \code{1000} pour IBGN, \code{2928} pour IBMR, \code{5856} pour IBD, \code{7036} pour IPR, \code{7939} pour IPLAC
#' @param codesie Code SANDRE de la station concernée
#' @import glue
#' @import httr
#' @import tidyverse
#' @export
#' @examples
#' hydrobio.hubeau(39, 7939)
#' hydrobio.hubeau(indicesandre = 7939)
#' hydrobio.hubeau(indicesandre = 7939) %>% filter(grepl("rousses", tolower(libelle_station_hydrobio)))

hydrobio.hubeau <- function(
    departementinsee = NA_integer_,
    indicesandre = NA_integer_,
    codesie = NA_integer_,
    bbox = NA
    
)
{
  
  #### Test ####
  if(all(is.na(departementinsee) & is.na(indicesandre) & is.na(codesie))) stop("Aucun paramètre fourni")
  
  #### Collecte des données ####
  ### Construction de l'URL ###
  # url <- glue("https://hubeau.eaufrance.fr/api/v2/qualite_rivieres/analyse_pc.csv?code_station={codesie}&code_parametre={parametresandre}&code_support={supportsandre}&code_qualification={qualificationsandre}&size=20000")
  url_base <- "https://hubeau.eaufrance.fr/api/v1/hydrobio/indices.csv?size=10000"
  url <- url_base
  if(!is.na(departementinsee)) url <- glue("{url}&code_departement={departementinsee}")
  if(!is.na(indicesandre)) url <- glue("{url}&code_indice={indicesandre}")
  if(!is.na(codesie)) url <- glue("{url}&code_station_hydrobio={codesie}")
  if(!is.na(bbox)) url <- glue("{url}&bbox={bbox$xmin}%2C{bbox$ymin}%2C{bbox$xmax}%2C{bbox$ymax}")
  
  ### Collecte à proprement parler ###
  data_to_import <- GET(url)
  if(http_status(data_to_import)$category != "Success") stop(glue("Problème dans l'exécution de la requête : '{http_status(data_to_import)$message}'"))
  
  data_to_add <- 
    data_to_import %>% 
    content(type = "text") %>%
    read_delim(delim = ";", locale = locale(decimal_mark = "."), show_col_types = FALSE)
    # read_csv2(show_col_types = FALSE)
  
  #### Traitement des données ####
  ### Tri ###
  data_to_add_2 <-
    data_to_add %>% 
    arrange(libelle_station_hydrobio, desc(date_prelevement))
  
  #### Sortie des données ####
  return(data_to_add_2)

} # Fin de la fonction
