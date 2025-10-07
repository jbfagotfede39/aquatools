#' Collecte des opérations de physico-chimie depuis Hub'eau
#'
#' Cette fonction permet de collecter les données d'opérations de suivi physico-chimique depuis Hub'eau
#' @name PC.hubeau.operation
#' @param codesie Code SANDRE de la station concernée
#' @param codeoperation Code SANDRE de l'opération concernée
#' @param communeinsee Code INSEE de la commune (\code{vide} par défaut)
#' @param bbox Zone géographique concernée, au format bbox généré  par sf, en WGS84/EPSG 4326
#' @param date_start Date de début du prélèvement d'échantillon : \code{NA} aucune (par défaut) - Format \code{AAAA-MM-JJ}
#' @param date_end Date de fin du prélèvement d'échantillon : \code{NA} aucune (par défaut) - Format \code{AAAA-MM-JJ}
#' @import glue
#' @import httr2
#' @import testit
#' @import tidyverse
#' @export
#' @examples
#' PC.hubeau.operation("06400085")
#' PC.hubeau.operation(codesie_sandre)
#' PC.hubeau.operation("V2205003", date_start = "2019-01-01", date_end = "2020-01-01")

PC.hubeau.operation <- function(
  codesie = NA_character_,
  codeoperation = NA_character_,
  communeinsee = NA_character_,
  bbox = NA,
  date_start = NA_character_,
  date_end = NA_character_
  )
{
  
  #### Test ####
  if(all(is.na(codesie) & is.na(codeoperation) & is.na(communeinsee))) stop("Aucun codesie ou codeoperation ou communeinsee fourni")

  #### Collecte des données ####
  ##### Construction de l'URL #####
  requete_commande <- 
    request("https://hubeau.eaufrance.fr/api/v2/qualite_rivieres/") %>% 
    req_method("GET") %>% 
    req_url_path_append("operation_pc.csv?size=5000") %>% 
    {if(!is.na(codesie)) req_url_query(., "code_station" = codesie) else .} %>% 
    {if(!is.na(codeoperation)) req_url_query(., "code_operation" = codeoperation) else .} %>% 
    {if(!is.na(communeinsee)) req_url_query(., "code_commune" = communeinsee) else .} %>% 
    {if(!is.na(bbox)) req_url_query(., "bbox" = glue("{bbox$xmin}%2C{bbox$ymin}%2C{bbox$xmax}%2C{bbox$ymax}")) else .} %>% 
    {if(!is.na(date_start)) req_url_query(., "date_debut_prelevement" = date_start) else .} %>% 
    {if(!is.na(date_end)) req_url_query(., "date_fin_prelevement" = date_end) else .}

  ### Collecte à proprement parler ###
  data_to_import <- requete_commande %>% req_perform()
  if(!(data_to_import$status_code %in% c(200, 206))) stop(glue("Problème dans l'exécution de la requête : 'code {data_to_import$status_code}'"))
  if(data_to_import$status_code == 206) warning("Attention, tous les résultats n'ont pas été récupérés (code 206)")
  if(data_to_import %>% resp_body_string() %>% read_csv2() %>% has_error()) stop(glue("Problème dans l'exécution de la requête : le contenu en retour est vide. Les paramètres de requête sont-ils corrects (station, etc.) ?"))

  data_to_add <- 
    data_to_import %>% 
    resp_body_string() %>% 
    read_csv2()
  
  #### Traitement des données ####
  ##### Tri #####
  data_to_add_2 <-
    data_to_add %>% 
    arrange(desc(date_prelevement), heure_prelevement, code_station, profondeur)
  
  #### Sortie des données ####
  return(data_to_add_2)

} # Fin de la fonction
