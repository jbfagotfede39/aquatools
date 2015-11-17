#' Listing des opérations pour une station
#'
#' Cette fonction permet de lister les différentes opérations pour une station donnée
#' 
#' @param station Code de la station
#' @keywords poissons
#' @import dplyr RSQLite DBI lubridate
#' @export
#' @examples
#' poissons.operations("SOR10-2")

##### TODO LIST #####
# 
#####################

poissons.operations <- function(
  station="SOR10-2")
{
  
  ## Connexion à la BDD ##
  drv <- dbDriver("SQLite")
  #db <- dbConnect(drv, dbname="/Users/imac27/hubiC/Données/Poissons/Base poisson FD/MaxiFish_V3/multifish - fede39.sqlite")
  
  if (file.exists("/Users/imac27/hubiC/Données/Poissons/Base poisson FD/MaxiFish_V3/multifish - fede39.sqlite") == T) db <- dbConnect(drv, dbname="/Users/imac27/hubiC/Données/Poissons/Base poisson FD/MaxiFish_V3/multifish - fede39.sqlite")
  if (file.exists("/Users/jean-baptistefagot_FD39/hubiC/Données/Poissons/Base poisson FD/MaxiFish_V3/multifish - fede39.sqlite") == T) db <- dbConnect(drv, dbname="/Users/jean-baptistefagot_FD39/hubiC/Données/Poissons/Base poisson FD/MaxiFish_V3/multifish - fede39.sqlite")
  
  ## Récupération des données ##
  #Captures <- dbReadTable(db, "Captures")
  #Operations <- dbReadTable(db, "Operations")
  Inventaires <- dbReadTable(db, "Inventaires")
  Stations <- dbReadTable(db, "Stations")
  
  ## Synthèse des données ##
  Inventaires <- merge(Inventaires, Stations, by = c("CodeStation"))
  
  ## Format de dates ##
  Inventaires$DateDebut <- ymd_hms(Inventaires$DateDebut)
  Inventaires$DateDebut <- format(Inventaires$DateDebut, "%Y-%m-%d")
  
  ## Extraction des stations ##
  # Test si le nom existe bien, sinon message d'erreur et arrêt de la fonction #
  
  if(dim(Inventaires %>% filter(Nom == station)
  )[1] == 0) 
    stop("Attention : nom de station absent de la base de données")
  
  ## Simplification ##
  Inventaires <- 
    Inventaires %>%
    select(Nom, DateDebut) %>% 
    filter(Nom == station) %>% 
    dplyr::rename(Stations = Nom, Dates = DateDebut)
    
} # Fin de la fonction