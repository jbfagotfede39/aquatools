#' Collecte de données de physico-chimie depuis Hub'eau
#'
#' Cette fonction permet de collecter des données d'analyses physico-chimiques depuis Hub'eau
#' @name PC.hubeau
#' @param codesie Code SANDRE de la station concernée
#' @param communeinsee Code INSEE de la commune (\code{vide} par défaut)
#' @param bbox Zone géographique concernée, au format bbox généré  par sf, en WGS84/EPSG 4326
#' @param parametresandre Code SANDRE du paramètre concerné
#' @param supportsandre Code SANDRE du support concerné : \code{NA} tous (par défaut), \code{3} - Eau, \code{4} - Poissons, \code{6} - Sédiments
#' @param date_start Date de début du prélèvement d'échantillon : \code{NA} aucune (par défaut) - Format \code{AAAA-MM-JJ}
#' @param date_end Date de fin du prélèvement d'échantillon : \code{NA} aucune (par défaut) - Format \code{AAAA-MM-JJ}
#' @param coderemarque Code SANDRE du code remarque concerné : \code{NA} tous (par défaut), \code{1} - Résultat > seuil de quantification et < au seuil de saturation (par défaut)
#' @param qualificationsandre Code SANDRE de la qualification concernée : \code{1} - Correcte (par défaut)
#' @import glue
#' @import httr2
#' @import testit
#' @import tidyverse
#' @export
#' @examples
#' PC.hubeau(codesie, parametresandre)
#' PC.hubeau("06400085", "1340")
#' PC.hubeau("06400085", "1386", supportsandre = "6")
#' PC.hubeau(communeinsee = "39470", parametresandre = "1312", supportsandre = "3")
#' PC.hubeau("V2205003", date_start = "2019-01-01", date_end = "2020-01-01")
#' pc_hubeau <- tibble(id = c("3", "4", "6")) %>% group_split(id) %>% map(~ PC.hubeau(codesie = codesie_sandre, supportsandre = .$id)) %>% list_rbind()

PC.hubeau <- function(
  codesie = NA_character_,
  communeinsee = NA_character_,
  bbox = NA,
  parametresandre = NA_character_,
  supportsandre = c(NA_character_, "3", "4", "6"),
  date_start = NA_character_,
  date_end = NA_character_,
  coderemarque = c(NA_character_, "1"),
  qualificationsandre = c("1")
  )
{
  #### Évaluation des choix ####
  supportsandre <- match.arg(supportsandre)
  coderemarque <- match.arg(coderemarque)
  qualificationsandre <- match.arg(qualificationsandre)
  
  #### Test ####
  if(all(is.na(codesie) & is.na(communeinsee))) stop("Aucun codesie ou communeinsee fourni")

  #### Collecte des données ####
  ##### Construction de l'URL #####
  requete_commande <- 
    request("https://hubeau.eaufrance.fr/api/v2/qualite_rivieres/") %>% 
    req_method("GET") %>% 
    req_url_path_append("analyse_pc.csv?size=20000") %>% 
    {if(!is.na(codesie)) req_url_query(., "code_station" = codesie) else .} %>% 
    {if(!is.na(communeinsee)) req_url_query(., "code_commune" = communeinsee) else .} %>% 
    {if(!is.na(bbox)) req_url_query(., "bbox" = glue("{bbox$xmin}%2C{bbox$ymin}%2C{bbox$xmax}%2C{bbox$ymax}")) else .} %>% 
    {if(!is.na(parametresandre)) req_url_query(., "code_parametre" = parametresandre) else .} %>% 
    {if(!is.na(supportsandre)) req_url_query(., "code_support" = supportsandre) else .} %>% 
    {if(!is.na(date_start)) req_url_query(., "date_debut_prelevement" = date_start) else .} %>% 
    {if(!is.na(date_end)) req_url_query(., "date_fin_prelevement" = date_end) else .} %>% 
    {if(!is.na(coderemarque)) req_url_query(., "code_remarque" = coderemarque) else .} %>% 
    {if(!is.na(qualificationsandre)) req_url_query(., "code_qualification" = qualificationsandre) else .}

  ##### Collecte à proprement parler #####
  data_to_import <- requete_commande %>% req_perform()
  if(!(data_to_import$status_code %in% c(200, 206))) stop(glue("Problème dans l'exécution de la requête : 'code {data_to_import$status_code}'"))
  if(data_to_import %>% resp_body_string() %>% read_csv2() %>% has_error()) stop(glue("Problème dans l'exécution de la requête : le contenu en retour est vide. Les paramètres de requête sont-ils corrects (station, etc.) ?"))

  data_to_add_partie_1 <- 
    data_to_import %>% 
    resp_body_string() %>% 
    read_csv2() %>% 
    mutate(limite_detection = limite_detection %>% as.character()) %>% 
    mutate(limite_quantification = limite_quantification %>% as.character()) %>% 
    mutate(incertitude_analytique = incertitude_analytique %>% as.character())
  
  if(data_to_import$status_code != 206) data_to_add <- data_to_add_partie_1
  if(data_to_import$status_code == 206){
    data_to_add_partie_2 <- requete_commande %>% req_url_query("sort" = "desc") %>% req_perform() %>% resp_body_string() %>% read_csv2() %>% 
      mutate(limite_detection = limite_detection %>% as.character()) %>% 
      mutate(limite_quantification = limite_quantification %>% as.character()) %>% 
      mutate(incertitude_analytique = incertitude_analytique %>% as.character())
    data_to_add <- data_to_add_partie_1 %>% union(data_to_add_partie_2)
  }
  
  if(data_to_add %>% nrow() == 40000) warning("Attention, tous les résultats n'ont pas été récupérés (plus de 40000 mesures)")
  # 
  # page <- 1
  # while(data_to_import$status_code == 206){ 
  # data_to_import <- requete_commande %>% req_url_query("page" = page) %>% req_perform()
  # data_to_add_partie_suivante <- data_to_import %>% resp_body_string() %>% read_csv2()
  # if(page == 1) data_to_add <- data_to_add_partie_suivante
  # if(page != 1) {data_to_add <- data_to_add %>% 
  #   mutate(limite_detection = limite_detection %>% as.character()) %>% 
  #   mutate(limite_quantification = limite_quantification %>% as.character()) %>% 
  #   union(data_to_add_partie_suivante %>% 
  #           mutate(incertitude_analytique = incertitude_analytique %>% as.character())
  #   )
  # }
  # print(page)
  # 
  # page <- page + 1
  #   
  # } 
  # data_to_add
  # 
  #### Traitement des données ####
  #### Changement de datatype ####
  data_to_add_2 <-
    data_to_add %>% 
    mutate(resultat = as.numeric(resultat))
  
  ##### Tri #####
  data_to_add_3 <-
    data_to_add_2 %>% 
    arrange(desc(date_prelevement), heure_prelevement, code_station, libelle_parametre)
  
  #### Sortie des données ####
  return(data_to_add_2)

} # Fin de la fonction
