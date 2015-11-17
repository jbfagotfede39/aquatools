#' Exportation des IPR
#'
#' Cette fonction permet d'exporter les résultats IPR de pêche
#' 
#' @param station Code de la station
#' @param date Date de la pêche
#' @keywords poissons
#' @import dplyr DBI RSQLite xlsx lubridate
#' @export
#' @examples
#' poissons.IPR("SOR10-2", "2015-05-19")

##### TODO LIST #####

#####################

poissons.IPR <- function(
  station="SAI17-8",
  date="2011-09-08")
{
  
  ## Connexion à la BDD ##
  drv <- dbDriver("SQLite")
  #db <- dbConnect(drv, dbname="/Users/imac27/hubiC/Données/Poissons/Base poisson FD/MaxiFish_V3/multifish - fede39.sqlite")
  
  if (file.exists("/Users/imac27/hubiC/Données/Poissons/Base poisson FD/MaxiFish_V3/multifish - fede39.sqlite") == T) db <- dbConnect(drv, dbname="/Users/imac27/hubiC/Données/Poissons/Base poisson FD/MaxiFish_V3/multifish - fede39.sqlite")
  if (file.exists("/Users/jean-baptistefagot_FD39/hubiC/Données/Poissons/Base poisson FD/MaxiFish_V3/multifish - fede39.sqlite") == T) db <- dbConnect(drv, dbname="/Users/jean-baptistefagot_FD39/hubiC/Données/Poissons/Base poisson FD/MaxiFish_V3/multifish - fede39.sqlite")
  
  ## Récupération des données ##
  #Captures <- dbReadTable(db, "Captures")
  #Operations <- dbReadTable(db, "Operations")
  IPR <- dbReadTable(db, "IPRs")
  #Inventaires <- dbReadTable(db, "Inventaires")
  Stations <- dbReadTable(db, "Stations")
  
  ## Synthèse des données ##
  #IPR <- merge(IPR, Inventaires, by = c("CodeInventaire"))
  IPR <- merge(IPR, Stations, by = c("CodeStation"))
  
  ## Format de dates ##
  IPR$DateIPR <- ymd_hms(IPR$DateIPR)
  IPR$DateIPR <- format(IPR$DateIPR, "%Y-%m-%d")
  
  ## Simplification ##
  IPR <- 
    IPR %>%
    select(1:42) %>% # Pour nettoyage
    select(-(1:2), -(4:6)) %>% # Pour nettoyage
    select(37,1:33) %>% # Pour remettre le nom de la station en premier
    filter(Nom == station, DateIPR == date) %>% 
    rename(Stations = Nom, Dates = DateIPR) %>% 
    arrange(Stations)
  
  IPR <-
    IPR %>% 
    select(Stations:Especes)
  
  ##### Sorties des résultats traités au format Excel #####
  
  #write.xlsx(x = IPR, file = paste0(station, "_", date, "_IPR.xlsx"),
  #           sheetName = paste0(station, " ", date), row.names = F)
  
  return(IPR)
  
} # Fin de la fonction