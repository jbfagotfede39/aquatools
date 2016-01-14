#' Extraction des données de référence des espèces
#'
#' Récupère l'ensemble des données de référence des espèces de Multifish
#' @keywords donnees
#' @import DBI dplyr RSQLite
#' @export
#' @examples
#' poissons.especes()

poissons.especes <- function(){
  
  #library("RSQLite");library("dplyr")
  
  ##### Connexion à la BDD #####
  drv <- dbDriver("SQLite")
  #db <- dbConnect(drv, dbname="/Users/imac27/hubiC/Données/Poissons/Base poisson FD/MaxiFish_V3/multifish - fede39.sqlite")
  
  if (file.exists("/Users/imac27/hubiC/Données/Poissons/Base poisson FD/MaxiFish_V3/multifish - fede39.sqlite") == T) db <- dbConnect(drv, dbname="/Users/imac27/hubiC/Données/Poissons/Base poisson FD/MaxiFish_V3/multifish - fede39.sqlite")
  if (file.exists("/Users/jean-baptistefagot_FD39/hubiC/Données/Poissons/Base poisson FD/MaxiFish_V3/multifish - fede39.sqlite") == T) db <- dbConnect(drv, dbname="/Users/jean-baptistefagot_FD39/hubiC/Données/Poissons/Base poisson FD/MaxiFish_V3/multifish - fede39.sqlite")
  
  ##### Récupération des données #####
  #Resultats <- dbReadTable(db, "Resultats")
  #Operations <- dbReadTable(db, "Operations")
  #Inventaires <- dbReadTable(db, "Inventaires")
  #Stations <- dbReadTable(db, "Stations")
  #Ecosystemes <- dbReadTable(db, "Ecosystemes")
  #Communes <- dbReadTable(db, "Communes")
  Especes <- dbReadTable(db, "Especes")
  
  return(Especes)
} # Fin de la fonction

# Resultats <- poissons.resultats() pour avoir les données en utilisant la fonction