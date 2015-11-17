#' Listage des stations d'un cours d'eau
#'
#' Cette fonction permet de lister les stations d'un cours d'eau donné
#' 
#' @param Nom du cours d'eau
#' @keywords stations
#' @import dplyr DBI RSQLite
#' @export
#' @examples
#' stations.ecosysteme("Ain")

##### TODO LIST #####
# Rajouter le choix de l'écosystème avec les codes (avec recherche dans le champ observation) et de même avec le code de la masse d'eau, avec un interrupteur disant où la fonction doit chercher initialement. Le reste du code reste identique.
#####################

stations.ecosysteme <- function(
  ecosysteme="Ain")
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
  Ecosystemes <- dbReadTable(db, "Ecosystemes")
  
  ## Extraction des afférences ##
  # Test si le nom existe bien, sinon message d'erreur et arrêt de la fonction #
  
  if(dim(Ecosystemes %>% filter(Nomecosysteme == ecosysteme)
  )[1] == 0) 
    stop("Attention : nom d'écosystème absent de la base de données")
  
  ecosysteme <-
    Ecosystemes %>% 
    filter(Nomecosysteme == ecosysteme)
  
  # Extraction du Codeecosysteme du CE qui nous concerne
  ecosysteme[,1]
  
  # Recherche des stations qui ont un Codeecosysteme = à ce Codeecosysteme
  Stations <-
    Stations %>% 
    filter(Codeecosysteme == ecosysteme[,1]) %>% 
    select(Nom)
  
  
  #rm(list=setdiff(ls(), "EcosystemeRechercheV6")) # Pour ne conserver que l'objet final
  #return(EcosystemeRechercheV6)
  
} # Fin de la fonction