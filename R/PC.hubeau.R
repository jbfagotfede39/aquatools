#' Collecte de données de physico-chimie depuis Hub'eau
#'
#' Cette fonction permet de collecter de données de physico-chimie depuis Hub'eau
#' @name PC.hubeau
#' @param codesie Code SANDRE de la station concernée
#' @param communeinsee Code INSEE de la commune (\code{vide} par défaut)
#' @param bbox Zone géographique concernée, au format bbox généré  par sf, en WGS84/EPSG 4326
#' @param parametresandre Code SANDRE du paramètre concerné
#' @param supportsandre Code SANDRE du support concerné : \code{3} - Eau (par défaut), \code{6} - Sédiments
#' @param coderemarque Code SANDRE du code remarque concerné : \code{1} - Résultat > seuil de quantification et < au seuil de saturation (par défaut)
#' @param qualificationsandre Code SANDRE de la qualification concernée : \code{1} - Correcte (par défaut)
#' @import glue
#' @import httr
#' @import tidyverse
#' @export
#' @examples
#' PC.hubeau(codesie, parametresandre)
#' PC.hubeau("06400085", "1340")
#' PC.hubeau("06400085", "1386", supportsandre = "6")
#' PC.hubeau(communeinsee = "39470", parametresandre = "1312", supportsandre = "3")

PC.hubeau <- function(
  codesie = NA_character_,
  communeinsee = NA_character_,
  bbox = NA,
  parametresandre = NA_character_,
  supportsandre = c("3", "6"),
  coderemarque = c("1"),
  qualificationsandre = c("1")
  )
{
  #### Évaluation des choix ####
  supportsandre <- match.arg(supportsandre)
  coderemarque <- match.arg(coderemarque)
  qualificationsandre <- match.arg(qualificationsandre)
  
  #### Test ####
  if(all(is.na(codesie) & is.na(communeinsee))) stop("Aucun codesie ou communeinsee fourni")
  if(is.na(parametresandre)) stop("Aucun parametresandre fourni")
  
  #### Collecte des données ####
  ### Construction de l'URL ###
  url_base <- "https://hubeau.eaufrance.fr/api/v2/qualite_rivieres/analyse_pc.csv?size=20000"
  url <- url_base
  if(!is.na(codesie)) url <- glue("{url}&code_station={codesie}")
  if(!is.na(communeinsee)) url <- glue("{url}&code_commune={communeinsee}")
  if(!is.na(bbox)) url <- glue("{url}&bbox={bbox$xmin}%2C{bbox$ymin}%2C{bbox$xmax}%2C{bbox$ymax}")
  if(!is.na(parametresandre)) url <- glue("{url}&code_parametre={parametresandre}")
  if(!is.na(supportsandre)) url <- glue("{url}&code_support={supportsandre}")
  if(!is.na(qualificationsandre)) url <- glue("{url}&code_qualification={qualificationsandre}")
  
  ### Collecte à proprement parler ###
  data_to_import <- GET(url)
  if(http_status(data_to_import)$category != "Success") stop(glue("Problème dans l'exécution de la requête : '{http_status(data_to_import)$message}'"))
  
  data_to_add <- 
    data_to_import %>% 
    content(type = "text") %>%
    read_delim(delim = ";", locale = locale(decimal_mark = "."), show_col_types = FALSE)
    # read_csv2(show_col_types = FALSE)
  
  #### Traitement des données ####
  ### Filtrage ###
  data_to_add_2 <-
    data_to_add %>% 
    filter(code_remarque == coderemarque)
  
  ### Tri ###
  data_to_add_3 <-
    data_to_add_2 %>% 
    arrange(desc(date_prelevement), heure_prelevement, code_station, libelle_parametre)
  
  #### Sortie des données ####
  return(data_to_add_3)

} # Fin de la fonction
