#' Export de données en shp
#'
#' Cette fonction permet d'exporter un dataframe (avec deux colonnes X et Y) ou un sf en Lambert 93 dans un shapefile/geojson/kml/excel
#' @name SIG.export
#' @param data Dataframe à exporter
#' @param nomfichier Nom du fichier de sortie
#' @param shp Export au format shapefile (\code{true} par défaut)
#' @param kml Export au format kml (\code{true} par défaut)
#' @param geojson Export au format geojson (\code{true} par défaut)
#' @param excel Export au format excel (\code{true} par défaut)
#' @keywords stations poissons
#' @import DBI
#' @import dplyr
#' @import RSQLite 
#' @import sf
#' @export
#' @examples
#' SIG.export(Stations, "Export_stations")
#' DataToAdd %>% st_as_sf(coords = c("XLong", "YLat"), crs = 4269) %>% st_transform(2154) %>% SIG.export("GCL_Bathy_points_bruts")

##### TODO LIST #####
# 
#####################

SIG.export <- function(
  data = Stations,
  nomfichier = "Export_stations",
  shp = TRUE,
  kml = TRUE,
  geojson = TRUE, 
  excel = TRUE)
{

#### Détection du format d'entrée ####
if("sf" %in% class(data) == TRUE) Datatype <- "sf"
if("sf" %in% class(data) == FALSE) Datatype <- "tbl_df"


#### Transformations ####
if(Datatype == "tbl_df"){
if("X" %in% colnames(data) == TRUE) data <- data %>% st_as_sf(coords = c("X", "Y"), crs = 2154) # Cas général
if("xlambert" %in% colnames(data) == TRUE) data <- data %>% st_as_sf(coords = c("xlambert", "ylambert"), crs = 2154) # pour la table issue de Multifish
#if("X" %in% colnames(data) == FALSE) stop(paste0(data, " ne contient pas de colonnes X et Y"))
}

if(Datatype == "sf"){data <- data %>% st_transform(2154)}

#### Vérifications ####
if("sf" %in% class(data) == FALSE) stop(paste0("Impossible de spatialiser ",data))

##### Exportation #####

# Export en shp #
if(shp == TRUE){
st_write(data, paste0(nomfichier, ".shp"), delete_layer = TRUE)
}

# Export en geojson #
if(geojson == TRUE){
  st_write(data, dsn = paste0(nomfichier, ".geojson"), driver='GeoJSON', update=TRUE)
}

# Export en excel #
if(excel == TRUE){
  #openxlsx::write.xlsx(st_set_geometry(data, NULL), paste0(nomfichier, ".xlsx"), sheetName = "Feuille1", row.names = F, showNA = F)
  st_write(data, dsn = paste0(nomfichier, ".xlsx"), layer = str_replace(nomfichier, "-", "_"), driver = "XLSX", layer_options = "OVERWRITE=true")
}

# Export en kml #
if(kml == TRUE){
data %>% 
  mutate(name = ifelse("chsta_coderhj" %in% names(.), chsta_coderhj, NA)) %>% 
  mutate(name = ifelse("nom" %in% names(.), nom, NA)) %>% # pour la table de multifish
  st_transform(4326) %>% 
  st_write(dsn = paste0(nomfichier, ".kml"), driver='kml', update=TRUE)
}


} # Fin de la fonction