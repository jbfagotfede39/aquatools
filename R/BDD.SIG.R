#' Export BDD vers SIG des stations
#'
#' Cette fonction permet d'exporter automatiquement les stations des différentes bases de données vers les couches SIG correspondantes
#' 
#' @param data Chronique à valider
#' @keywords data
#' @import dplyr DBI lubridate
#' @export
#' @examples
#' BDD.SIG()

###### À faire #####
# 
####################

BDD.SIG <- function()
{


  ###### Chroniques ######
  db <- BDD.ouverture("Chroniques")
  Stations <- dbReadTable(db, "Stations")
  Stations <- Stations %>% filter(!is.na(XL93))
  
  if(file.exists("/Users/imac27/hubiC/Données/Chroniques/BDD_Chroniques_FD39.sqlite") == T){
  file.copy("/Users/imac27/hubiC/SIG/Données/Réseaux/Chroniques/Stations_chroniques.dbf", paste0("/Users/imac27/hubiC/SIG/Données/Réseaux/Chroniques/archives/", format(now(), format="%Y-%m-%d"),"_Stations_chroniques.dbf"), overwrite = T)
  file.copy("/Users/imac27/hubiC/SIG/Données/Réseaux/Chroniques/Stations_chroniques.prj", paste0("/Users/imac27/hubiC/SIG/Données/Réseaux/Chroniques/archives/", format(now(), format="%Y-%m-%d"),"_Stations_chroniques.prj"), overwrite = T)
  file.copy("/Users/imac27/hubiC/SIG/Données/Réseaux/Chroniques/Stations_chroniques.shp", paste0("/Users/imac27/hubiC/SIG/Données/Réseaux/Chroniques/archives/", format(now(), format="%Y-%m-%d"),"_Stations_chroniques.shp"), overwrite = T)
  file.copy("/Users/imac27/hubiC/SIG/Données/Réseaux/Chroniques/Stations_chroniques.shx", paste0("/Users/imac27/hubiC/SIG/Données/Réseaux/Chroniques/archives/", format(now(), format="%Y-%m-%d"),"_Stations_chroniques.shx"), overwrite = T)
  SIG.exportSHP(Stations, Stations$XL93, Stations$YL93, "/Users/imac27/hubiC/SIG/Données/Réseaux/Chroniques/Stations_chroniques")
  }
  
  if(file.exists("/Volumes/Fixe-FD39/hubiC/Données/Chroniques/BDD_Chroniques_FD39.sqlite") == T){
    file.copy("/Volumes/Fixe-FD39/hubiC/SIG/Données/Réseaux/Chroniques/Stations_chroniques.dbf", paste0("/Volumes/Fixe-FD39/hubiC/SIG/Données/Réseaux/Chroniques/archives/", format(now(), format="%Y-%m-%d"),"_Stations_chroniques.dbf"), overwrite = T)
    file.copy("/Volumes/Fixe-FD39/hubiC/SIG/Données/Réseaux/Chroniques/Stations_chroniques.prj", paste0("/Volumes/Fixe-FD39/hubiC/SIG/Données/Réseaux/Chroniques/archives/", format(now(), format="%Y-%m-%d"),"_Stations_chroniques.prj"), overwrite = T)
    file.copy("/Volumes/Fixe-FD39/hubiC/SIG/Données/Réseaux/Chroniques/Stations_chroniques.shp", paste0("/Volumes/Fixe-FD39/hubiC/SIG/Données/Réseaux/Chroniques/archives/", format(now(), format="%Y-%m-%d"),"_Stations_chroniques.shp"), overwrite = T)
    file.copy("/Volumes/Fixe-FD39/hubiC/SIG/Données/Réseaux/Chroniques/Stations_chroniques.shx", paste0("/Volumes/Fixe-FD39/hubiC/SIG/Données/Réseaux/Chroniques/archives/", format(now(), format="%Y-%m-%d"),"_Stations_chroniques.shx"), overwrite = T)
    SIG.exportSHP(Stations, Stations$XL93, Stations$YL93, "/Volumes/Fixe-FD39/hubiC/SIG/Données/Réseaux/Chroniques/Stations_chroniques")
  }
  
  ###### Poissons ######
  db <- BDD.ouverture("Poissons")
  Stations <- dbReadTable(db, "Stations")
  Stations <- Stations %>% filter(TypeLambert == 'L93')
  
    if(file.exists("/Users/imac27/hubiC/Données/Poissons/Base poisson FD/MaxiFish_V3/multifish - datas.sqlite") == T){
    file.copy("/Users/imac27/hubiC/SIG/Données/Réseaux/Poissons/Stations_poissons.dbf", paste0("/Users/imac27/hubiC/SIG/Données/Réseaux/Poissons/archives/", format(now(), format="%Y-%m-%d"),"_Stations_poissons.dbf"), overwrite = T)
    file.copy("/Users/imac27/hubiC/SIG/Données/Réseaux/Poissons/Stations_poissons.prj", paste0("/Users/imac27/hubiC/SIG/Données/Réseaux/Poissons/archives/", format(now(), format="%Y-%m-%d"),"_Stations_poissons.prj"), overwrite = T)
    file.copy("/Users/imac27/hubiC/SIG/Données/Réseaux/Poissons/Stations_poissons.shp", paste0("/Users/imac27/hubiC/SIG/Données/Réseaux/Poissons/archives/", format(now(), format="%Y-%m-%d"),"_Stations_poissons.shp"), overwrite = T)
    file.copy("/Users/imac27/hubiC/SIG/Données/Réseaux/Poissons/Stations_poissons.shx", paste0("/Users/imac27/hubiC/SIG/Données/Réseaux/Poissons/archives/", format(now(), format="%Y-%m-%d"),"_Stations_poissons.shx"), overwrite = T)
    SIG.exportSHP(Stations, Stations$XLambert, Stations$YLambert, "/Users/imac27/hubiC/SIG/Données/Réseaux/Poissons/Stations_poissons")
  }
  
  if(file.exists("/Volumes/Fixe-FD39/hubiC/DonnéesPoissons/Base poisson FD/MaxiFish_V3/multifish - datas.sqlite") == T){
    file.copy("/Volumes/Fixe-FD39/hubiC/SIG/Données/Réseaux/Poissons/Stations_poissons.dbf", paste0("/Volumes/Fixe-FD39/hubiC/SIG/Données/Réseaux/Poissons/archives/", format(now(), format="%Y-%m-%d"),"_Stations_poissons.dbf"), overwrite = T)
    file.copy("/Volumes/Fixe-FD39/hubiC/SIG/Données/Réseaux/Poissons/Stations_poissons.prj", paste0("/Volumes/Fixe-FD39/hubiC/SIG/Données/Réseaux/Poissons/archives/", format(now(), format="%Y-%m-%d"),"_Stations_poissons.prj"), overwrite = T)
    file.copy("/Volumes/Fixe-FD39/hubiC/SIG/Données/Réseaux/Poissons/Stations_poissons.shp", paste0("/Volumes/Fixe-FD39/hubiC/SIG/Données/Réseaux/Poissons/archives/", format(now(), format="%Y-%m-%d"),"_Stations_poissons.shp"), overwrite = T)
    file.copy("/Volumes/Fixe-FD39/hubiC/SIG/Données/Réseaux/Poissons/Stations_poissons.shx", paste0("/Volumes/Fixe-FD39/hubiC/SIG/Données/Réseaux/Poissons/archives/", format(now(), format="%Y-%m-%d"),"_Stations_poissons.shx"), overwrite = T)
    SIG.exportSHP(Stations, Stations$XLambert, Stations$YLambert, "/Volumes/Fixe-FD39/hubiC/SIG/Données/Réseaux/Poissons/Stations_poissons")
  }
  
  ###### Physico-chimie ######
  db <- BDD.ouverture("Physico-chimie")
  Stations <- dbReadTable(db, "Operations")
  Stations <- Stations %>% filter(!is.na(XL93))
  
  if(file.exists("/Users/imac27/hubiC/Données/Physico-chimie/BDD_Physico-chimie_FD39.sqlite") == T){
    file.copy("/Users/imac27/hubiC/SIG/Données/Réseaux/Physico-chimie/Stations_PC.dbf", paste0("/Users/imac27/hubiC/SIG/Données/Réseaux/Physico-chimie/archives/", format(now(), format="%Y-%m-%d"),"_Stations_PC.dbf"), overwrite = T)
    file.copy("/Users/imac27/hubiC/SIG/Données/Réseaux/Physico-chimie/Stations_PC.prj", paste0("/Users/imac27/hubiC/SIG/Données/Réseaux/Physico-chimie/archives/", format(now(), format="%Y-%m-%d"),"_Stations_PC.prj"), overwrite = T)
    file.copy("/Users/imac27/hubiC/SIG/Données/Réseaux/Physico-chimie/Stations_PC.shp", paste0("/Users/imac27/hubiC/SIG/Données/Réseaux/Physico-chimie/archives/", format(now(), format="%Y-%m-%d"),"_Stations_PC.shp"), overwrite = T)
    file.copy("/Users/imac27/hubiC/SIG/Données/Réseaux/Physico-chimie/Stations_PC.shx", paste0("/Users/imac27/hubiC/SIG/Données/Réseaux/Physico-chimie/archives/", format(now(), format="%Y-%m-%d"),"_Stations_PC.shx"), overwrite = T)
    SIG.exportSHP(Stations, Stations$XL93, Stations$YL93, "/Users/imac27/hubiC/SIG/Données/Réseaux/Physico-chimie/Stations_PC")
  }
  
  if(file.exists("/Volumes/Fixe-FD39/hubiC/Données/Physico-chimie/BDD_Physico-chimie_FD39.sqlite") == T){
    file.copy("/Volumes/Fixe-FD39/hubiC/SIG/Données/Réseaux/Physico-chimie/Stations_PC.dbf", paste0("/Volumes/Fixe-FD39/hubiC/SIG/Données/Réseaux/Physico-chimie/archives/", format(now(), format="%Y-%m-%d"),"_Stations_PC.dbf"), overwrite = T)
    file.copy("/Volumes/Fixe-FD39/hubiC/SIG/Données/Réseaux/Physico-chimie/Stations_PC.prj", paste0("/Volumes/Fixe-FD39/hubiC/SIG/Données/Réseaux/Physico-chimie/archives/", format(now(), format="%Y-%m-%d"),"_Stations_PC.prj"), overwrite = T)
    file.copy("/Volumes/Fixe-FD39/hubiC/SIG/Données/Réseaux/Physico-chimie/Stations_PC.shp", paste0("/Volumes/Fixe-FD39/hubiC/SIG/Données/Réseaux/Physico-chimie/archives/", format(now(), format="%Y-%m-%d"),"_Stations_PC.shp"), overwrite = T)
    file.copy("/Volumes/Fixe-FD39/hubiC/SIG/Données/Réseaux/Physico-chimie/Stations_PC.shx", paste0("/Volumes/Fixe-FD39/hubiC/SIG/Données/Réseaux/Physico-chimie/archives/", format(now(), format="%Y-%m-%d"),"_Stations_PC.shx"), overwrite = T)
    SIG.exportSHP(Stations, Stations$XL93, Stations$YL93, "/Volumes/Fixe-FD39/hubiC/SIG/Données/Réseaux/Physico-chimie/Stations_PC")
  }
  
} # Fin de la fonction