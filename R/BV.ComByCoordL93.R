#' Recherche de caractéristiques administratives d'un point
#'
#' Cette fonction permet de fournir les caractéristiques géographiques d'un point donné (commune, département, région, population, etc.) à partir de la GeoAPI
#' 
#' @param X Coordonnée X à rechercher (Lambert 93)
#' @param Y Coordonnée Y à rechercher (Lambert 93)
#' @import rgeoapi
#' @import sf
#' @import tidyverse
#' @export
#' @examples
#' BV.ComByCoordL93(936722,6603736)
#' data %>% rowwise() %>% mutate(Commune = BV.ComByCoordL93(X,Y) %>% select(name) %>% as.character())

##### TODO LIST #####
#
#####################

BV.ComByCoordL93 <- function(
  X = as.numeric(0),
  Y = as.numeric(0)
    )
{

  #### Conversion des coordonnées en Lambert WGS84 ####
  Point <- sf::st_as_sf(data.frame(X,Y), coords = c("X","Y")) %>% st_set_crs(2154) %>% st_transform(4269) %>% st_coordinates()
  
  #### Recherche en tant que telle ####
data <- rgeoapi::ComByCoord(lat = Point[,2], lon = Point[,1])

  #### Sortie des données ####
return(data)
  
} # Fin de la fonction