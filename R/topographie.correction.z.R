#' Correction altimétrique de coordonnées
#'
#' Cette fonction permet de corriger la coordonnée Z de points au format \code{sf}
#' @name topographie.correction.z
#' @param leves Nuage de points issu de `topographie.mesures`
#' @param correction Valeur de la correction à apporter, en mètres
#' @keywords topographie
#' @import sf
#' @import tidyverse
#' @export 
#' @examples
#' 
#' leves_1 %>% topographie.correction.z(0.1)

topographie.correction.z <- function(
  leves = NA_character_,
  correction = 0
  )
{

  #### Calcul ####
  leves_v2 <-
    leves %>% 
    mutate(chsta_coord_x = st_coordinates(.)[,1]) %>%
    mutate(chsta_coord_y = st_coordinates(.)[,2]) %>%
    mutate(chsta_coord_z = st_coordinates(.)[,3]) %>%
    mutate(chsta_coord_z = chsta_coord_z + correction) %>% 
    st_drop_geometry() %>% 
    sf::st_as_sf(coords = c("chsta_coord_x", "chsta_coord_y", "chsta_coord_z"), remove = T) %>%
    st_set_crs(2154) %>% 
    rename(geom = geometry)

  #### Sortie ####
  return(leves_v2)

} # Fin de la fonction