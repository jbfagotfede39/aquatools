#' Conversion de coordonnées en degrés décimaux vers Lambert 93
#'
#' Cette fonction permet de convertir des coordonnées en degrés décimaux (4326) vers du Lambert 93 (2154)
#' @name SIG.conversion.dec2l93
#' @param coord_x Coordonnée x à convertir
#' @param coord_y Coordonnée y à convertir
#' @keywords SIG
#' @import sf
#' @import tidyverse
#' @export
#' @examples
#' SIG.conversion.dec2l93(5.972321, 46.806626) %>% mapview::mapview()

    SIG.conversion.dec2l93 <- function(coord_x = NA, coord_y = NA) {
      
      #### Test de cohérence ####
      if(is.na(coord_x)) stop("Pas de coord_x fournie")
      if(is.na(coord_y)) stop("Pas de coord_y fournie")
      
      #### Calcul ####
      cible <- SIG.conversion(coord_x, coord_y, 4326, 2154)
      
      #### Sortie ####
      return(cible)
    }