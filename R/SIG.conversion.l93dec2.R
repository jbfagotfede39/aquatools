#' Conversion de coordonnées en Lambert 93 vers degrés décimaux
#'
#' Cette fonction permet de convertir des coordonnées en du Lambert 93 (2154) vers degrés décimaux (4326)
#' @name SIG.conversion.l93dec2
#' @param coord_x Coordonnée x à convertir
#' @param coord_y Coordonnée y à convertir
#' @keywords SIG
#' @import sf
#' @import tidyverse
#' @export
#' @examples
#' SIG.conversion.l93dec2(888227, 6655627)

    SIG.conversion.l93dec2 <- function(coord_x = NA, coord_y = NA) {
        
        #### Test de cohérence ####
        if(is.na(coord_x)) stop("Pas de coord_x fournie")
        if(is.na(coord_y)) stop("Pas de coord_y fournie")
        
        #### Calcul ####
        cible <- SIG.conversion(coord_x, coord_y, 2154, 4326)
        
        #### Sortie ####
        return(cible)
      }