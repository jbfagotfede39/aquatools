#' Conversion de coordonnées en Lambert 93 vers degrés décimaux
#'
#' Cette fonction permet de convertir des coordonnées en du Lambert 93 vers degrés décimaux
#' @name SIG.conversion.l93dec2
#' @param coord_x Coordonnée x à convertir
#' @param coord_y Coordonnée y à convertir
#' @keywords SIG
#' @import sf
#' @import tidyverse
#' @export
#' @examples
#' SIG.conversion.l93dec2(888227, 6655627)

    SIG.conversion.l93dec2 <- function(coord_x, coord_y) {
      
      wgs84 <-
        tribble(~id, ~point_coord_x, ~point_coord_y,
              1, coord_x, coord_y
      ) %>% 
        st_as_sf(coords = c("point_coord_x", "point_coord_y"), crs = 2154, dim = "XY", remove = F) %>%
        st_transform(4326) %>% 
        mutate(chsta_coord_x_conv = st_coordinates(.)[,1]) %>% 
        mutate(chsta_coord_y_conv= st_coordinates(.)[,2]) %>% 
        st_drop_geometry() %>% 
        st_as_sf(coords = c("chsta_coord_y_conv", "chsta_coord_x_conv"), crs = 4326, dim = "XY", remove = T)
      
      return(wgs84)
    }
