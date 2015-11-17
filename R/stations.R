#' Extraction de données de stations
#'
#' Cette fonction permet d'extraire les données complètes d'une station
#' 
#' @param Nom de la station
#' @keywords stations
#' @import DBI dplyr RSQLite
#' @export
#' @examples
#' stations("SOR10-2")

##### TODO LIST #####

#####################

stations <- function(
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
  #IPR <- dbReadTable(db, "IPRs")
  #Inventaires <- dbReadTable(db, "Inventaires")
  Stations <- dbReadTable(db, "Stations")
  #Ecosystemes <- dbReadTable(db, "Ecosystemes")
  
  ## Extraction des afférences ##
  # Test si le nom existe bien, sinon message d'erreur et arrêt de la fonction #
  
  if(dim(Stations %>% filter(Nom == station)
  )[1] == 0) 
    stop("Attention : nom de station absent de la base de données")
  
  station <-
    Stations %>% 
    filter(Nom == station)
  
} # Fin de la fonction