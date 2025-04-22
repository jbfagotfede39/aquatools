#' Conversion de coordonnées en degrés décimaux vers Lambert 93
#'
#' Cette fonction permet de convertir des coordonnées en egrés décimaux vers du Lambert 93
#' @name SIG.conversion.dec2l93
#' @param coord_x Coordonnée x à convertir
#' @param coord_y Coordonnée y à convertir
#' @keywords SIG
#' @import sf
#' @import tidyverse
#' @export
#' @examples
#' SIG.conversion.dec2l93(46.806626, 5.972321)

    SIG.conversion.dec2l93 <- function(coord_x, coord_y) {
      
      l93 <-
        tribble(~id, ~point_coord_x, ~point_coord_y,
              1, coord_y, coord_x
      ) %>% 
        st_as_sf(coords = c("point_coord_x", "point_coord_y"), crs = 4326, dim = "XY", remove = F) %>%
        st_transform(2154)
      
      return(l93)
    }
