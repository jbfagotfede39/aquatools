#' Export BDD vers SIG des stations
#'
#' Cette fonction permet d'exporter automatiquement les stations des différentes bases de données vers les couches SIG correspondantes
#' 
#' @param data Chronique à valider
#' @keywords data
#' @import DBI
#' @import dplyr
#' @import lubridate
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
  db <- BDD.ouverture("Chroniques")
  Stations <- 
    tbl(db,"Stations") %>% 
    collect() %>% 
    filter(!is.na(XL93)) #%>%
    # rename(X = XL93) %>%
    # rename(Y = YL93)

  if(file.exists("/Users/imac27/NAS-DATA/Chroniques/BDD_Chroniques_FD39.sqlite") == T){
  file.copy("/Users/imac27/NAS-SIG/Données/Réseaux/Chroniques/Stations_chroniques.dbf", paste0("/Users/imac27/NAS-SIG/Données/Réseaux/Chroniques/archives/", format(now(), format="%Y-%m-%d"),"_Stations_chroniques.dbf"), overwrite = T)
  file.copy("/Users/imac27/NAS-SIG/Données/Réseaux/Chroniques/Stations_chroniques.prj", paste0("/Users/imac27/NAS-SIG/Données/Réseaux/Chroniques/archives/", format(now(), format="%Y-%m-%d"),"_Stations_chroniques.prj"), overwrite = T)
  file.copy("/Users/imac27/NAS-SIG/Données/Réseaux/Chroniques/Stations_chroniques.shp", paste0("/Users/imac27/NAS-SIG/Données/Réseaux/Chroniques/archives/", format(now(), format="%Y-%m-%d"),"_Stations_chroniques.shp"), overwrite = T)
  file.copy("/Users/imac27/NAS-SIG/Données/Réseaux/Chroniques/Stations_chroniques.shx", paste0("/Users/imac27/NAS-SIG/Données/Réseaux/Chroniques/archives/", format(now(), format="%Y-%m-%d"),"_Stations_chroniques.shx"), overwrite = T)
  SIG.exportSHP(Stations, "/Users/imac27/NAS-SIG/Données/Réseaux/Chroniques/Stations_chroniques.shp")
  }
  
  if(file.exists("/Volumes/Fixe-FD39/NAS-DATA/Chroniques/BDD_Chroniques_FD39.sqlite") == T){
    file.copy("/Volumes/Fixe-FD39/NAS-SIG/Données/Réseaux/Chroniques/Stations_chroniques.dbf", paste0("/Volumes/Fixe-FD39/NAS-SIG/Données/Réseaux/Chroniques/archives/", format(now(), format="%Y-%m-%d"),"_Stations_chroniques.dbf"), overwrite = T)
    file.copy("/Volumes/Fixe-FD39/NAS-SIG/Données/Réseaux/Chroniques/Stations_chroniques.prj", paste0("/Volumes/Fixe-FD39/NAS-SIG/Données/Réseaux/Chroniques/archives/", format(now(), format="%Y-%m-%d"),"_Stations_chroniques.prj"), overwrite = T)
    file.copy("/Volumes/Fixe-FD39/NAS-SIG/Données/Réseaux/Chroniques/Stations_chroniques.shp", paste0("/Volumes/Fixe-FD39/NAS-SIG/Données/Réseaux/Chroniques/archives/", format(now(), format="%Y-%m-%d"),"_Stations_chroniques.shp"), overwrite = T)
    file.copy("/Volumes/Fixe-FD39/NAS-SIG/Données/Réseaux/Chroniques/Stations_chroniques.shx", paste0("/Volumes/Fixe-FD39/NAS-SIG/Données/Réseaux/Chroniques/archives/", format(now(), format="%Y-%m-%d"),"_Stations_chroniques.shx"), overwrite = T)
    SIG.exportSHP(Stations, "/Volumes/Fixe-FD39/NAS-SIG/Données/Réseaux/Chroniques/Stations_chroniques.shp")
  }
  
  if(file.exists("/Users/adrienlavigne/NAS-DATA/Chroniques/BDD_Chroniques_FD39.sqlite") == T){
    file.copy("/Users/adrienlavigne/NAS-SIG/Données/Réseaux/Chroniques/Stations_chroniques.dbf", paste0("/Users/adrienlavigne/NAS-SIG/Données/Réseaux/Chroniques/archives/", format(now(), format="%Y-%m-%d"),"_Stations_chroniques.dbf"), overwrite = T)
    file.copy("/Users/adrienlavigne/NAS-SIG/Données/Réseaux/Chroniques/Stations_chroniques.prj", paste0("/Users/adrienlavigne/NAS-SIG/Données/Réseaux/Chroniques/archives/", format(now(), format="%Y-%m-%d"),"_Stations_chroniques.prj"), overwrite = T)
    file.copy("/Users/adrienlavigne/NAS-SIG/Données/Réseaux/Chroniques/Stations_chroniques.shp", paste0("/Users/adrienlavigne/NAS-SIG/Données/Réseaux/Chroniques/archives/", format(now(), format="%Y-%m-%d"),"_Stations_chroniques.shp"), overwrite = T)
    file.copy("/Users/adrienlavigne/NAS-SIG/Données/Réseaux/Chroniques/Stations_chroniques.shx", paste0("/Users/adrienlavigne/NAS-SIG/Données/Réseaux/Chroniques/archives/", format(now(), format="%Y-%m-%d"),"_Stations_chroniques.shx"), overwrite = T)
    SIG.exportSHP(Stations, "/Users/adrienlavigne/NAS-SIG/Données/Réseaux/Chroniques/Stations_chroniques.shp")
  }
  
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