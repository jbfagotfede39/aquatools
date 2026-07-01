#' Collecte de données ONDE depuis Hub'eau
#'
#' Cette fonction permet de collecter des données d'observations ONDE depuis Hub'eau
#' @name hydrologie.hubeau.onde
#' @param code_departement Code INSEE du département (\code{vide} par défaut)
#' @param formatage Formatage des champs au format interne FDPPMA39 (\code{TRUE} par défaut)
#' @import glue
#' @import httr2
#' @import testit
#' @import tidyverse
#' @export
#' @examples
#' hydrologie.hubeau.onde(39)
#' hydrologie.hubeau.onde(39, formatage = FALSE)

hydrologie.hubeau.onde <- function(
  code_departement = NA_integer_,
  formatage = TRUE
  )
{

  #### Test ####
  if(is.na(code_departement)) stop("Aucun code_departement fourni")

  #### Collecte des données ####
  ##### Construction de l'URL #####
  requete_commande <- 
    request("https://hubeau.eaufrance.fr/api/v1/ecoulement/") %>% 
    req_method("GET") %>% 
    req_url_path_append("observations.csv?size=20000") %>% 
    {if(!is.na(code_departement)) req_url_query(., "code_departement" = code_departement) else .}

  ##### Collecte à proprement parler #####
  data_to_import <- requete_commande %>% req_perform()
  if(!(data_to_import$status_code %in% c(200, 206))) stop(glue("Problème dans l'exécution de la requête : 'code {data_to_import$status_code}'"))
  if(data_to_import %>% resp_body_string() %>% I() %>% read_csv2() %>% has_error()) stop(glue("Problème dans l'exécution de la requête : le contenu en retour est vide. Les paramètres de requête sont-ils corrects (station, etc.) ?"))

  data_to_add <- 
    data_to_import %>% 
    resp_body_string() %>% 
    I() %>% 
    read_csv2() %>% 
    filter(!is.na(date_observation))
  
  if(data_to_add %>% nrow() == 20000) warning("Attention, tous les résultats n'ont pas été récupérés (plus de 20000 mesures)")

  #### Traitement des données ####
  ##### Formatage #####
  data_to_add_2 <-
    data_to_add %>% 
    mutate(dispositif = "Onde", .before = code_station) %>% 
    mutate(coordonnee_x_station = coordonnee_x_station/10) %>%
    mutate(coordonnee_y_station = coordonnee_y_station/10) %>%
    mutate(x_hs = ifelse(coordonnee_x_station > 1000000, TRUE, FALSE), .after = coordonnee_x_station) %>% 
    mutate(coordonnee_x_station = ifelse(x_hs == TRUE, coordonnee_x_station/100000000, coordonnee_x_station)) %>% 
    mutate(x_hs = ifelse(coordonnee_x_station > 1000000, TRUE, FALSE), .after = coordonnee_x_station) %>% 
    mutate(coordonnee_x_station = ifelse(x_hs == TRUE, coordonnee_x_station/10, coordonnee_x_station)) %>% 
    mutate(y_hs = ifelse(coordonnee_y_station > 10000000, TRUE, FALSE), .after = coordonnee_y_station) %>% 
    mutate(coordonnee_y_station = ifelse(y_hs == TRUE, coordonnee_y_station/10000000, coordonnee_y_station)) %>% 
    mutate(y_hs = ifelse(coordonnee_y_station > 10000000, TRUE, FALSE), .after = coordonnee_y_station) %>% 
    mutate(coordonnee_y_station = ifelse(y_hs == TRUE, coordonnee_y_station/10, coordonnee_y_station)) %>% 
    mutate(y_hs = ifelse(coordonnee_y_station > 10000000, TRUE, FALSE), .after = coordonnee_y_station) %>% 
    mutate(coordonnee_y_station = ifelse(y_hs == TRUE, coordonnee_y_station/10, coordonnee_y_station)) %>% 
    select(-x_hs, y_hs) %>% 
    {if(formatage == TRUE) rename(., id_original = code_station) else .} %>% 
    {if(formatage == TRUE) rename(., station = libelle_station) else .} %>% 
    {if(formatage == TRUE) select(., -uri_station) else .} %>% 
    {if(formatage == TRUE) rename(., commune_insee = code_commune) else .} %>% 
    {if(formatage == TRUE) rename(., commune_libelle = libelle_commune) else .} %>%
    {if(formatage == TRUE) mutate(., departement_insee = code_departement) else .} %>% 
    {if(formatage == TRUE) rename(., departement_libelle = libelle_departement) else .} %>%
    # {if(formatage == TRUE) rename(., commune_insee = code_commune) else .} %>% 
    # {if(formatage == TRUE) rename(., commune_libelle = libelle_commune) else .} %>% 
    {if(formatage == TRUE) rename(., region_insee = code_region) else .} %>% 
    {if(formatage == TRUE) rename(., region_libelle = libelle_region) else .} %>% 
    {if(formatage == TRUE) select(., -code_bassin) else .} %>% 
    {if(formatage == TRUE) rename(., bassin_sdage = libelle_bassin) else .} %>% 
    {if(formatage == TRUE) rename(., coord_x = coordonnee_x_station) else .} %>% 
    {if(formatage == TRUE) rename(., coord_y = coordonnee_y_station) else .} %>% 
    {if(formatage == TRUE) rename(., coord_epsg = libelle_projection_station) else .} %>% 
    {if(formatage == TRUE) mutate(., coord_epsg = ifelse(coord_epsg == "RGF93 / Lambert 93", 2154, NA)) else .} %>% 
    {if(formatage == TRUE) rename(., milieu_sandre = code_cours_eau) else .} %>% 
    {if(formatage == TRUE) mutate(., milieu_saisi = NA_character_) else .} %>% 
    {if(formatage == TRUE) rename(., milieu_libelle = libelle_cours_eau) else .} %>% 
    {if(formatage == TRUE) select(., -(uri_cours_eau:uri_reseau)) else .} %>% 
    {if(formatage == TRUE) mutate(., time = ymd_hms(glue("{date_observation}_00:00:00"))) else .} %>% 
    {if(formatage == TRUE) rename(., modalite_ecoulement = libelle_ecoulement) else .} %>% 
    {if(formatage == TRUE) select(., -code_ecoulement) else .} %>% 
    {if(formatage == TRUE) select(., -latitude, -longitude) else .} %>% 
    {if(formatage == TRUE) mutate(., mo = "OFB") else .} %>% 
    {if(formatage == TRUE) rename_all(., list(~stringi::stri_trans_general(., "latin-ascii"))) else .} %>% 
    {if(formatage == TRUE) rename_all(., list(~paste0("hysvietia_",.))) else .} %>% 
    {if(formatage == TRUE) rename_all(., list(~gsub("[[:punct:]]", "_", .))) else .} %>% 
    {if(formatage == TRUE) rename_all(., list(~tolower(.))) else .} %>% 
    {if(formatage == TRUE) mutate(., id = row_number(), .before = hysvietia_dispositif) else .} %>% 
    {if(formatage == TRUE) mutate(., hysvietia_presence_jussie = NA) else .} %>% 
    {if(formatage == TRUE) mutate(., hysvietia_presence_jacinthe = NA) else .} %>% 
    {if(formatage == TRUE) mutate(., hysvietia_meteo = NA_character_) else .} %>% 
    {if(formatage == TRUE) mutate(., hysvietia_remarques = NA_character_) else .} %>% 
    {if(formatage == TRUE) mutate(., `_modif_utilisateur` = NA_character_) else .} %>% 
    {if(formatage == TRUE) mutate(., `_modif_type` = NA_character_) else .} %>% 
    {if(formatage == TRUE) mutate(., `_modif_date` = NA_character_) else .} %>% 
    {if(formatage == TRUE) sf::st_as_sf(., coords = c("hysvietia_coord_x","hysvietia_coord_y"), remove = FALSE) else .} %>% 
    {if(formatage == TRUE) st_set_crs(., 2154) else .} %>% 
    {if(formatage == TRUE) rename(., geom = geometry) else .} %>% 
    {if(formatage == TRUE) select(., id, hysvietia_dispositif, hysvietia_id_original, hysvietia_station, hysvietia_commune_libelle, hysvietia_commune_insee, hysvietia_departement_libelle, hysvietia_departement_insee, hysvietia_region_libelle, hysvietia_region_insee, hysvietia_bassin_sdage, hysvietia_milieu_saisi, hysvietia_milieu_libelle, hysvietia_milieu_sandre, hysvietia_coord_x, hysvietia_coord_y, hysvietia_coord_epsg, hysvietia_time, hysvietia_modalite_ecoulement, hysvietia_presence_jussie, hysvietia_presence_jacinthe, hysvietia_meteo, hysvietia_remarques, hysvietia_mo, `_modif_utilisateur`, `_modif_type`, `_modif_date`, geom) else .}
  
  ##### Tri #####
  data_to_add_3 <-
    data_to_add_2 %>% 
    arrange(desc(hysvietia_time), hysvietia_id_original)
  
  #### Sortie des données ####
  return(data_to_add_3)

} # Fin de la fonction
