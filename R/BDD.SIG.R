#' Export BDD vers SIG des stations
#'
#' Cette fonction permet d'exporter automatiquement les stations des différentes bases de données vers les couches SIG correspondantes
#' @name BDD.SIG
#' @param data Chronique à valider
#' @keywords data
#' @import DBI
#' @import dplyr
#' @import lubridate
#' @import openxlsx
#' @import sf
#' @import tidyverse
#' @export
#' @examples
#' BDD.SIG()

###### À faire #####
# 
####################

BDD.SIG <- function()
{


  ###### Chroniques ######
  ## Chargement des données ##
  dbD <- BDD.ouverture("Data")
  Stations <- sf::st_read(dbD, query = "select * from fd_production.chroniques_stations;") %>% arrange(chsta_coderhj)

  ## Copie ##
  file.copy(adresse.switch("NAS-SIG/Données/Réseaux/Chroniques/Stations_chroniques.dbf"), paste0(adresse.switch("NAS-SIG/Données/Réseaux/Chroniques/archives/"), format(now(), format="%Y-%m-%d"),"_Stations_chroniques.dbf"), overwrite = T)
  file.copy(adresse.switch("NAS-SIG/Données/Réseaux/Chroniques/Stations_chroniques.prj"), paste0(adresse.switch("NAS-SIG/Données/Réseaux/Chroniques/archives/"), format(now(), format="%Y-%m-%d"),"_Stations_chroniques.prj"), overwrite = T)
  file.copy(adresse.switch("NAS-SIG/Données/Réseaux/Chroniques/Stations_chroniques.shp"), paste0(adresse.switch("NAS-SIG/Données/Réseaux/Chroniques/archives/"), format(now(), format="%Y-%m-%d"),"_Stations_chroniques.shp"), overwrite = T)
  file.copy(adresse.switch("NAS-SIG/Données/Réseaux/Chroniques/Stations_chroniques.shx"), paste0(adresse.switch("NAS-SIG/Données/Réseaux/Chroniques/archives/"), format(now(), format="%Y-%m-%d"),"_Stations_chroniques.shx"), overwrite = T)
  file.copy(adresse.switch("NAS-SIG/Données/Réseaux/Chroniques/Stations_chroniques.kml"), paste0(adresse.switch("NAS-SIG/Données/Réseaux/Chroniques/archives/"), format(now(), format="%Y-%m-%d"),"_Stations_chroniques.kml"), overwrite = T)
  file.copy(adresse.switch("NAS-SIG/Données/Réseaux/Chroniques/Stations_chroniques.geojson"), paste0(adresse.switch("NAS-SIG/Données/Réseaux/Chroniques/archives/"), format(now(), format="%Y-%m-%d"),"_Stations_chroniques.geojson"), overwrite = T)
  file.copy(adresse.switch("NAS-SIG/Données/Réseaux/Chroniques/Stations_chroniques.xlsx"), paste0(adresse.switch("NAS-SIG/Données/Réseaux/Chroniques/archives/"), format(now(), format="%Y-%m-%d"),"_Stations_chroniques.xlsx"), overwrite = T)
  
  ## Exportation ##
  SIG.exportSHP(Stations, adresse.switch("NAS-SIG/Données/Réseaux/Chroniques/Stations_chroniques.shp"))
  st_write(Stations %>% mutate(name = chsta_coderhj) %>% st_transform(4326), "/Users/imac27/NAS-SIG/Données/Réseaux/Chroniques/Stations_chroniques.kml", driver='kml', delete_dsn=TRUE) # kml
  st_write(Stations, adresse.switch("NAS-SIG/Données/Réseaux/Chroniques/Stations_chroniques.geojson"), driver='GeoJSON', delete_dsn=TRUE) # geojson
  openxlsx::write.xlsx(st_set_geometry(Stations, NULL), adresse.switch("NAS-SIG/Données/Réseaux/Chroniques/Stations_chroniques.xlsx"), sheetName = "Feuille1", row.names = F, showNA = F) # excel
  
  ###### Poissons ######
  db <- BDD.ouverture("Poissons")
  Stations <- 
    tbl(db,"stations") %>% 
    collect() %>% 
    filter(typelambert == 'L93') %>% 
    rename(X = xlambert) %>% 
    rename(Y = ylambert) 
  
    if(file.exists("/Users/imac27/NAS-DATA/Poissons/Base poisson FD/MaxiFish_V3/multifish - datas.sqlite") == T){
    file.copy("/Users/imac27/NAS-SIG/Données/Réseaux/Poissons/Stations_poissons.dbf", paste0("/Users/imac27/NAS-SIG/Données/Réseaux/Poissons/archives/", format(now(), format="%Y-%m-%d"),"_Stations_poissons.dbf"), overwrite = T)
    file.copy("/Users/imac27/NAS-SIG/Données/Réseaux/Poissons/Stations_poissons.prj", paste0("/Users/imac27/NAS-SIG/Données/Réseaux/Poissons/archives/", format(now(), format="%Y-%m-%d"),"_Stations_poissons.prj"), overwrite = T)
    file.copy("/Users/imac27/NAS-SIG/Données/Réseaux/Poissons/Stations_poissons.shp", paste0("/Users/imac27/NAS-SIG/Données/Réseaux/Poissons/archives/", format(now(), format="%Y-%m-%d"),"_Stations_poissons.shp"), overwrite = T)
    file.copy("/Users/imac27/NAS-SIG/Données/Réseaux/Poissons/Stations_poissons.shx", paste0("/Users/imac27/NAS-SIG/Données/Réseaux/Poissons/archives/", format(now(), format="%Y-%m-%d"),"_Stations_poissons.shx"), overwrite = T)
    SIG.exportSHP(Stations, "/Users/imac27/NAS-SIG/Données/Réseaux/Poissons/Stations_poissons.shp")
  }
  
  if(file.exists("/Volumes/Fixe-FD39/NAS-DATA/Poissons/Base poisson FD/MaxiFish_V3/multifish - datas.sqlite") == T){
    file.copy("/Volumes/Fixe-FD39/NAS-SIG/Données/Réseaux/Poissons/Stations_poissons.dbf", paste0("/Volumes/Fixe-FD39/NAS-SIG/Données/Réseaux/Poissons/archives/", format(now(), format="%Y-%m-%d"),"_Stations_poissons.dbf"), overwrite = T)
    file.copy("/Volumes/Fixe-FD39/NAS-SIG/Données/Réseaux/Poissons/Stations_poissons.prj", paste0("/Volumes/Fixe-FD39/NAS-SIG/Données/Réseaux/Poissons/archives/", format(now(), format="%Y-%m-%d"),"_Stations_poissons.prj"), overwrite = T)
    file.copy("/Volumes/Fixe-FD39/NAS-SIG/Données/Réseaux/Poissons/Stations_poissons.shp", paste0("/Volumes/Fixe-FD39/NAS-SIG/Données/Réseaux/Poissons/archives/", format(now(), format="%Y-%m-%d"),"_Stations_poissons.shp"), overwrite = T)
    file.copy("/Volumes/Fixe-FD39/NAS-SIG/Données/Réseaux/Poissons/Stations_poissons.shx", paste0("/Volumes/Fixe-FD39/NAS-SIG/Données/Réseaux/Poissons/archives/", format(now(), format="%Y-%m-%d"),"_Stations_poissons.shx"), overwrite = T)
    SIG.exportSHP(Stations, "/Volumes/Fixe-FD39/NAS-SIG/Données/Réseaux/Poissons/Stations_poissons.shp")
  }
  
  if(file.exists("/Volumes/adrienlavigne/NAS-DATA/Poissons/Base poisson FD/MaxiFish_V3/multifish - datas.sqlite") == T){
    file.copy("/Volumes/adrienlavigne/NAS-SIG/Données/Réseaux/Poissons/Stations_poissons.dbf", paste0("/Volumes/adrienlavigne/NAS-SIG/Données/Réseaux/Poissons/archives/", format(now(), format="%Y-%m-%d"),"_Stations_poissons.dbf"), overwrite = T)
    file.copy("/Volumes/adrienlavigne/NAS-SIG/Données/Réseaux/Poissons/Stations_poissons.prj", paste0("/Volumes/adrienlavigne/NAS-SIG/Données/Réseaux/Poissons/archives/", format(now(), format="%Y-%m-%d"),"_Stations_poissons.prj"), overwrite = T)
    file.copy("/Volumes/adrienlavigne/NAS-SIG/Données/Réseaux/Poissons/Stations_poissons.shp", paste0("/Volumes/adrienlavigne/NAS-SIG/Données/Réseaux/Poissons/archives/", format(now(), format="%Y-%m-%d"),"_Stations_poissons.shp"), overwrite = T)
    file.copy("/Volumes/adrienlavigne/NAS-SIG/Données/Réseaux/Poissons/Stations_poissons.shx", paste0("/Volumes/adrienlavigne/NAS-SIG/Données/Réseaux/Poissons/archives/", format(now(), format="%Y-%m-%d"),"_Stations_poissons.shx"), overwrite = T)
    SIG.exportSHP(Stations, "/Volumes/adrienlavigne/NAS-SIG/Données/Réseaux/Poissons/Stations_poissons.shp")
  }
  
  ###### Physico-chimie ######
  db <- BDD.ouverture("Physico-chimie")
  Stations <- 
    tbl(db,"Operations") %>% 
    collect() %>% 
    filter(!is.na(XL93)) %>% 
    rename(X = XL93) %>% 
    rename(Y = YL93) 
  
  if(file.exists("/Users/imac27/NAS-DATA/Physico-chimie/BDD_Physico-chimie_FD39.sqlite") == T){
    file.copy("/Users/imac27/NAS-SIG/Données/Réseaux/Physico-chimie/Stations_PC.dbf", paste0("/Users/imac27/NAS-SIG/Données/Réseaux/Physico-chimie/archives/", format(now(), format="%Y-%m-%d"),"_Stations_PC.dbf"), overwrite = T)
    file.copy("/Users/imac27/NAS-SIG/Données/Réseaux/Physico-chimie/Stations_PC.prj", paste0("/Users/imac27/NAS-SIG/Données/Réseaux/Physico-chimie/archives/", format(now(), format="%Y-%m-%d"),"_Stations_PC.prj"), overwrite = T)
    file.copy("/Users/imac27/NAS-SIG/Données/Réseaux/Physico-chimie/Stations_PC.shp", paste0("/Users/imac27/NAS-SIG/Données/Réseaux/Physico-chimie/archives/", format(now(), format="%Y-%m-%d"),"_Stations_PC.shp"), overwrite = T)
    file.copy("/Users/imac27/NAS-SIG/Données/Réseaux/Physico-chimie/Stations_PC.shx", paste0("/Users/imac27/NAS-SIG/Données/Réseaux/Physico-chimie/archives/", format(now(), format="%Y-%m-%d"),"_Stations_PC.shx"), overwrite = T)
    SIG.exportSHP(Stations, "/Users/imac27/NAS-SIG/Données/Réseaux/Physico-chimie/Stations_PC.shp")
  }
  
  if(file.exists("/Volumes/Fixe-FD39/NAS-DATA/Physico-chimie/BDD_Physico-chimie_FD39.sqlite") == T){
    file.copy("/Volumes/Fixe-FD39/NAS-SIG/Données/Réseaux/Physico-chimie/Stations_PC.dbf", paste0("/Volumes/Fixe-FD39/NAS-SIG/Données/Réseaux/Physico-chimie/archives/", format(now(), format="%Y-%m-%d"),"_Stations_PC.dbf"), overwrite = T)
    file.copy("/Volumes/Fixe-FD39/NAS-SIG/Données/Réseaux/Physico-chimie/Stations_PC.prj", paste0("/Volumes/Fixe-FD39/NAS-SIG/Données/Réseaux/Physico-chimie/archives/", format(now(), format="%Y-%m-%d"),"_Stations_PC.prj"), overwrite = T)
    file.copy("/Volumes/Fixe-FD39/NAS-SIG/Données/Réseaux/Physico-chimie/Stations_PC.shp", paste0("/Volumes/Fixe-FD39/NAS-SIG/Données/Réseaux/Physico-chimie/archives/", format(now(), format="%Y-%m-%d"),"_Stations_PC.shp"), overwrite = T)
    file.copy("/Volumes/Fixe-FD39/NAS-SIG/Données/Réseaux/Physico-chimie/Stations_PC.shx", paste0("/Volumes/Fixe-FD39/NAS-SIG/Données/Réseaux/Physico-chimie/archives/", format(now(), format="%Y-%m-%d"),"_Stations_PC.shx"), overwrite = T)
    SIG.exportSHP(Stations, "/Volumes/Fixe-FD39/NAS-SIG/Données/Réseaux/Physico-chimie/Stations_PC.shp")
  }
  
  if(file.exists("/Users/adrienlavigne/NAS-DATA/Physico-chimie/BDD_Physico-chimie_FD39.sqlite") == T){
    file.copy("/Users/adrienlavigne/NAS-SIG/Données/Réseaux/Physico-chimie/Stations_PC.dbf", paste0("/Users/adrienlavigne/NAS-SIG/Données/Réseaux/Physico-chimie/archives/", format(now(), format="%Y-%m-%d"),"_Stations_PC.dbf"), overwrite = T)
    file.copy("/Users/adrienlavigne/NAS-SIG/Données/Réseaux/Physico-chimie/Stations_PC.prj", paste0("/Users/adrienlavigne/NAS-SIG/Données/Réseaux/Physico-chimie/archives/", format(now(), format="%Y-%m-%d"),"_Stations_PC.prj"), overwrite = T)
    file.copy("/Users/adrienlavigne/NAS-SIG/Données/Réseaux/Physico-chimie/Stations_PC.shp", paste0("/Users/adrienlavigne/NAS-SIG/Données/Réseaux/Physico-chimie/archives/", format(now(), format="%Y-%m-%d"),"_Stations_PC.shp"), overwrite = T)
    file.copy("/Users/adrienlavigne/NAS-SIG/Données/Réseaux/Physico-chimie/Stations_PC.shx", paste0("/Users/adrienlavigne/NAS-SIG/Données/Réseaux/Physico-chimie/archives/", format(now(), format="%Y-%m-%d"),"_Stations_PC.shx"), overwrite = T)
    SIG.exportSHP(Stations, "/Users/imac27/NAS-SIG/Données/Réseaux/Physico-chimie/Stations_PC.shp")
  }
  
} # Fin de la fonction