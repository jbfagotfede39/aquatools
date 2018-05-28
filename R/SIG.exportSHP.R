#' Export de données en shp
#'
#' Cette fonction permet d'exporter un dataframe (avec deux colonnes X et Y) dans un shapefile en Lambert 93
#' 
#' @param Data Dataframe à exporter
#' @param nomfichier Nom du fichier de sortie
#' @keywords stations poissons
#' @import DBI
#' @import dplyr
#' @import RSQLite 
#' @import sf
#' @export
#' @examples
#' SIG.exportSHP(Stations, "Export_stations.shp")

##### TODO LIST #####
# 
#####################

SIG.exportSHP <- function(
  Data = Stations,
  nomfichier = "Export_stations.shp")
{

  ##### Écriture d'une couche SIG #####
Data2 <- Data %>% st_as_sf(coords = c("X", "Y"), crs = 2154)
st_write(Data2, nomfichier, delete_layer = TRUE)
} # Fin de la fonction