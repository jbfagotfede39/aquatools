#' Conversion de coordonnées en 32N vers Lambert 93
#'
#' Cette fonction permet de convertir des coordonnées en 32N (32632) vers Lambert 93 (2154)
#' @name SIG.conversion.32n2l93
#' @param coord_x Coordonnée x à convertir
#' @param coord_y Coordonnée y à convertir
#' @keywords SIG
#' @import sf
#' @import tidyverse
#' @export
#' @examples
#' SIG.conversion.32n2l93(276412.375, 5154007.570)

    SIG.conversion.32n2l93 <- function(coord_x = NA, coord_y = NA) {
      
      #### Test de cohérence ####
      if(is.na(coord_x)) stop("Pas de coord_x fournie")
      if(is.na(coord_y)) stop("Pas de coord_y fournie")
      
      #### Calcul ####
      cible <- SIG.conversion(coord_x, coord_y, 32632, 2154)
      
      #### Sortie ####
      return(cible)
    }
