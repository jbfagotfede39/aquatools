#' Création automatisée de vues de profils le long de transects
#'
#' Cette fonction permet de représenter des profils (topographiques par exemple) le long de transects
#' @name topographie.figure.profil.automatique
#' @param transects Dataframe avec les identifiants et les coordonnées de début et de fin des transects à représenter
#' @param leves Nuage de points issu de `topographie.mesures`
#' @param buffer Distance maximale entre les points et le transect (\code{10} par défaut)
#' @param ligne_eau Altitude de la ligne d'eau. Si non fourni, calculé automatiquement à partir des \code{tplv_code}, mais forçage manuel possible ici
#' @param save Si \code{FALSE} (par défaut), n'enregistre pas les figures. Si \code{TRUE}, les enregistre.
#' @param save_name Nom de fichier à utiliser pour l'enregistrement. \code{"{today()}_Vue_profil.png"} si vide.
#' @keywords topographie
#' @import glue
#' @import sf
#' @import tidyverse
#' @export 
#' @examples
#' transect %>% topographie.figure.profil.automatique()

topographie.figure.profil.automatique <- function(
    transects = NA,
    leves = NA,
    buffer = 10,
    ligne_eau = NA,
    save = F,
    save_name = NA_character_
)
{
  #### Contexte ####
  n_transect <- n_distinct(transects$id)
  
  #### Test de cohérence ####
  if(n_transect == 0) stop("Attention : aucun transect fourni")
  if(n_transect != 1) stop("Attention : la fonction topographie.figure.profil.automatique ne peut traiter qu'un unique transect à la fois")
  if(nrow(leves) == 0) stop("Attention : aucune donnée de levés fournie")
  
  #### Calcul de la projection des valeurs sur le profil ####
  points_projetes_sur_transect <- transects %>% group_split(id) %>% map(~ topographie.profil(leves, ., buffer)) %>% reduce(rbind)
  
  #### Complément/nettoyage des informations ####
  points_projetes_sur_transect_commentes <-
    points_projetes_sur_transect %>% 
    left_join(leves %>% st_drop_geometry() %>% select(id, tplv_code), by = join_by(id))
  
  lignes_eau <- 
    points_projetes_sur_transect_commentes %>% 
    filter(grepl("LE", tplv_code))
  ligne_eau_moyenne <-
    lignes_eau %>% 
    st_drop_geometry() %>% 
    summarise(moy = mean(tplv_coord_z)) %>% 
    pull()
  if(is.na(ligne_eau_moyenne) & is.na(ligne_eau)) ligne_eau_moyenne <- 0 # Afin qu'il n'y ait pas d'affichage de la ligne d'eau via topographie.figure.profil
  if(is.na(ligne_eau_moyenne) & !is.na(ligne_eau)) ligne_eau_moyenne <- ligne_eau # Forçage manuel
  
  terrain_naturel <- 
    points_projetes_sur_transect_commentes %>% 
    filter(!grepl("LE", tplv_code))
  
  #### Représentation ####
  vue_transect <- 
    points_projetes_sur_transect %>% 
    topographie.figure.profil(altitude_ligne_deau = ligne_eau_moyenne)
  nom_transect <- points_projetes_sur_transect %>% distinct(id_transect) %>% pull()
  vue_transect <- vue_transect + labs(title = glue("Transect {nom_transect}"))
  
  #### Sortie ####
  # Enregistrement
  if(save==T){
    if(is.na(save_name)){
      ggsave(file = glue("{today()}_Vue_profil.png"))
    }
    if(!is.na(save_name)){
      ggsave(file = glue("{today()}_{save_name}.png"))
    }
  }
  
  # Affichage
  return(vue_transect)
  
  
} # Fin de la fonction