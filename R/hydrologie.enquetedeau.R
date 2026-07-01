#' Collecte de données En quête d'eau
#'
#' Cette fonction permet de collecter des données d'observations En quête d'eau
#' @name hydrologie.enquetedeau
#' @param code_departement Code INSEE du département (\code{vide} par défaut)
#' @param annee Année recherchée (\code{vide} par défaut)
#' @param formatage Formatage des champs au format interne FDPPMA39 (\code{TRUE} par défaut)
#' @import glue
#' @import httr2
#' @import testit
#' @import tidyverse
#' @export
#' @examples
#' hydrologie.enquetedeau(39, 2026)
#' hydrologie.enquetedeau(39, 2026, formatage = FALSE)

hydrologie.enquetedeau <- function(
  code_departement = NA_integer_,
  annee = NA_integer_,
  formatage = TRUE
  )
{

  #### Test ####
  if(is.na(code_departement)) stop("Aucun code_departement fourni")
  if(is.na(annee)) stop("Aucune année fournie")

  #### Collecte des données ####
  temp <- tempfile()
  download.file(glue("https://enquetedeau.eaufrance.fr/sites/default/files/export-observations/export-{annee}.zip"), temp) # non fonctionnel car actualisé une fois par mois
  data_to_add_en_quete_deau <-
    read_delim(unz(temp, "observations.csv"), delim = ";")
  unlink(temp)
  
  #### Traitement des données ####
  ##### Nettoyage #####
  data_to_add_en_quete_deau <-
    data_to_add_en_quete_deau %>% 
    mutate(`Nom du point d'observation` = ifelse(`Nom du point d'observation` == " ", NA, `Nom du point d'observation`))
  
  ##### Filtrage #####
  data_to_add_en_quete_deau <-
    data_to_add_en_quete_deau %>% 
    filter(`Code département` == as.character(code_departement))
  
  ##### Formatage #####
  data_to_add_2 <-
    data_to_add_en_quete_deau %>% 
    {if(formatage == TRUE) select(., -Dispositif) else .} %>% 
    {if(formatage == TRUE) select(., -`Code ONDE`) else .} %>% 
    {if(formatage == TRUE) mutate(., dispositif = "En quête d'eau", .before = `Identifiant technique du point d'observation`) else .} %>% 
    {if(formatage == TRUE) mutate(., id_original = as.character(`Identifiant technique du point d'observation`)) else .} %>% 
    {if(formatage == TRUE) rename(., station = `Nom du point d'observation`) else .} %>% 
    {if(formatage == TRUE) rename(., commune_libelle = Commune) else .} %>% 
    {if(formatage == TRUE) mutate(., commune_insee = as.numeric(`Code Commune`)) else .} %>%  
    {if(formatage == TRUE) rename(., departement_libelle = Département) else .} %>% 
    {if(formatage == TRUE) mutate(., departement_insee = as.numeric(`Code département`)) else .} %>% 
    {if(formatage == TRUE) rename(., region_libelle = Région) else .} %>% 
    {if(formatage == TRUE) rename(., region_insee = `Code Région`) else .} %>% 
    {if(formatage == TRUE) rename(., bassin_sdage = Bassin) else .} %>% 
    {if(formatage == TRUE) rename(., milieu_saisi = `Nom usuel du cours d'eau saisi`) else .} %>% 
    {if(formatage == TRUE) rename(., milieu_libelle = `Nom officiel du Cours d'eau`) else .} %>% 
    {if(formatage == TRUE) rename(., milieu_sandre = `Code officiel du cours d'eau`) else .} %>% 
    {if(formatage == TRUE) rename(., coord_x = `Coordonnée X`) else .} %>% 
    {if(formatage == TRUE) rename(., coord_y = `Coordonnée Y`) else .} %>% 
    {if(formatage == TRUE) rename(., coord_epsg = `Système de projection`) else .} %>% 
    {if(formatage == TRUE) rename(., time = `Date et heure de l'observation`) else .} %>% 
    {if(formatage == TRUE) mutate(., time = dmy_hms(time)) else .} %>% 
    {if(formatage == TRUE) rename(., modalite_ecoulement = `Modalité d'écoulement`) else .} %>% 
    {if(formatage == TRUE) rename(., presence_jussie = `Présence de Jussie`) else .} %>% 
    {if(formatage == TRUE) mutate(., presence_jussie = ifelse(presence_jussie == "Non", FALSE, presence_jussie)) else .} %>% 
    {if(formatage == TRUE) mutate(., presence_jussie = ifelse(presence_jussie == "Oui", TRUE, presence_jussie)) else .} %>% 
    {if(formatage == TRUE) rename(., presence_jacinthe = `Présence de Jacinthe`) else .} %>% 
    {if(formatage == TRUE) mutate(., presence_jacinthe = ifelse(presence_jacinthe == "Non", FALSE, presence_jacinthe)) else .} %>% 
    {if(formatage == TRUE) mutate(., presence_jacinthe = ifelse(presence_jacinthe == "Oui", TRUE, presence_jacinthe)) else .} %>% 
    {if(formatage == TRUE) rename(., remarques = Commentaire) else .} %>% 
    {if(formatage == TRUE) rename(., mo = Structure) else .} %>% 
    {if(formatage == TRUE) rename_all(., list(~stringi::stri_trans_general(., "latin-ascii"))) else .} %>%  # Pour remplacer les caractères accentués par les mêmes sans accents
    {if(formatage == TRUE) rename_all(., list(~paste0("hysvietia_",.))) else .} %>% 
    {if(formatage == TRUE) rename_all(., list(~gsub("[[:punct:]]", "_", .))) else .} %>% 
    {if(formatage == TRUE) rename_all(., list(~tolower(.))) else .} %>% 
    {if(formatage == TRUE) mutate(., id = row_number(), .before = hysvietia_dispositif) else .} %>% 
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
