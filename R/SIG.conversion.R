#' Conversion de coordonnées
#'
#' Cette fonction permet de convertir des coordonnées d'une projection vers une autre
#' @name SIG.conversion.l93dec2
#' @param coord_x Coordonnée x à convertir
#' @param coord_y Coordonnée y à convertir
#' @param projection_source Projection initiale
#' @param projection_cible Projection cible
#' @keywords SIG
#' @import sf
#' @import tidyverse
#' @export
#' @examples
#' SIG.conversion(888227, 6655627, 2154, 4326)

    SIG.conversion <- function(coord_x, coord_y, projection_source = NA, projection_cible = NA) {
      
      #### Test de cohérence ####
      if(is.na(projection_source)) stop("Pas de projection_source fournie")
      if(is.na(projection_cible)) stop("Pas de projection_cible fournie")
      
      #### Calcul ####
      cible <-
        tribble(~id, ~point_coord_x, ~point_coord_y,
              1, coord_x, coord_y
      ) %>% 
        st_as_sf(coords = c("point_coord_x", "point_coord_y"), crs = projection_source, dim = "XY", remove = F) %>%
        st_transform(projection_cible) %>% 
        mutate(chsta_coord_x_conv = st_coordinates(.)[,1]) %>% 
        mutate(chsta_coord_y_conv= st_coordinates(.)[,2]) %>% 
        st_drop_geometry() %>% 
        st_as_sf(coords = c("chsta_coord_x_conv", "chsta_coord_y_conv"), crs = projection_cible, dim = "XY", remove = T)
        # {if(projection_source %in% c("32631", "32632", "4326")) st_as_sf(., coords = c("chsta_coord_x_conv", "chsta_coord_y_conv"), crs = projection_cible, dim = "XY", remove = T) else .} %>%
        # {if(projection_source == c("2154") & projection_cible == c("4326")) st_as_sf(., coords = c("chsta_coord_y_conv", "chsta_coord_x_conv"), crs = projection_cible, dim = "XY", remove = T) else .}
      
      #### Sortie ####
      return(cible)
    }