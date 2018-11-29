#' Export de données en shp
#'
#' Cette fonction permet d'exporter un dataframe (avec deux colonnes X et Y) ou un sf en Lambert 93 dans un shapefile/geojson/kml/excel
#' @name SIG.export
#' @param sata Dataframe à exporter
#' @param nomfichier Nom du fichier de sortie
#' @keywords stations poissons
#' @import DBI
#' @import dplyr
#' @import RSQLite 
#' @import sf
#' @export
#' @examples
#' SIG.export(Stations, "Export_stations")

##### TODO LIST #####
# 
#####################

SIG.export <- function(
  data = Stations,
  nomfichier = "Export_stations")
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

#### Vérifications ####
if("sf" %in% class(data) == FALSE) stop(paste0("Impossible de spatialiser ",data))

##### Exportation #####

# Export en shp #
st_write(data, paste0(nomfichier, ".shp"), delete_layer = TRUE)

# Export en kml #
data %>% 
  mutate(name = ifelse("chsta_coderhj" %in% names(.), chsta_coderhj, NA)) %>% 
  mutate(name = ifelse("nom" %in% names(.), nom, NA)) %>% # pour la table de multifish
  st_transform(4326) %>% 
  st_write(dsn = paste0(nomfichier, ".kml"), driver='kml', update=TRUE)

# Export en geojson #
st_write(data, dsn = paste0(nomfichier, ".geojson"), driver='GeoJSON', update=TRUE)

# Export en excel #
#openxlsx::write.xlsx(st_set_geometry(data, NULL), paste0(nomfichier, ".xlsx"), sheetName = "Feuille1", row.names = F, showNA = F)
st_write(data, dsn = paste0(nomfichier, ".xlsx"), layer = str_replace(nomfichier, "-", "_"), driver = "XLSX", layer_options = "OVERWRITE=true")

} # Fin de la fonction