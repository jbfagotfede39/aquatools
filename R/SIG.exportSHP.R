#' Export de données en shp
#'
#' Cette fonction permet d'exporter un shapefile en Lambert 93
#' 
#' @param Data Dataframe à exporter
#' @param XLambert Colonne des X
#' @param YLambert Colonne des Y
#' @param nomfichier Nom du fichier de sortie
#' @keywords stations poissons
#' @import DBI dplyr rgdal RSQLite sp
#' @export
#' @examples
#' SIG.exportSHP(Stations, XLambert, YLambert, "Export_stations")

##### TODO LIST #####
# Bug à l'export des stations de thermie depuis la BDD :
# ne fonctionne pad : SIG.exportSHP(Stations, XL93, YL93, "2015-10-07_stations_thermie")
# alors que ça fonctionne si on passe directement : coordinates(Stations) <- ~XL93+YL93
# ce qui semble poser pb est la transfo de nom de XL93 en XLambert par la fonction, car il n'y a ensuite pas de XLambert dans le dataframe et la fct coordinates refuse de fonctionner

#####################

SIG.exportSHP <- function(
  Data = Stations,
  XLambert = XLambert,
  YLambert = YLambert,
  nomfichier = "Export_stations")
{
  
  ##### Écriture d'une couche SIG #####
coordinates(Data) <- ~XLambert+YLambert # Permet de créer un SpatialPointsDataFrame à partir d'un dataframe Data contenant les coordonnées dans les colonnes XLambert et YLambert
proj4string(Data) <- CRS("+init=epsg:2154") # Définition de la projection, ici pour Lambert 93
# proj4string(Data) <- CRS("+init=IGNF:LAMBE") # Pour LambertIIétendu
setCPLConfigOption("SHAPE_ENCODING", "UTF-8") # Afin de définir l'encodage en UT8
writeOGR(Data, ".", nomfichier, driver="ESRI Shapefile", layer_options= c(encoding= "UTF-8"), overwrite_layer=T)

} # Fin de la fonction