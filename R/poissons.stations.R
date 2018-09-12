#' Extraction de données de stations
#'
#' Cette fonction permet d'extraire les données complètes de l'ensemble des stations piscicoles (par défaut) ou d'une seule
#' 
#' @param Nom de la station
#' @keywords stations
#' @import DBI 
#' @import dplyr 
#' @import RSQLite
#' @export
#' @examples
#' Stations <- poissons.stations()
#' Station <- poissons.stations("SOR10-2")

##### TODO LIST #####

#####################

poissons.stations <- function(
  station="")
{
  
  ## Ouverture de la BDD ##
  dbP <- BDD.ouverture(Type = "Poissons")
  
  ## Récupération des données ##
  Stations <- tbl(dbP,"stations") %>% collect(n = Inf)
  
  ## Fermeture de la BDD ##
  DBI::dbDisconnect(dbP)
  
  ## Extraction des données de la station si une est spécifiée ##
  if(nchar(station) != 0) {
    # Test si le nom existe bien, sinon message d'erreur et arrêt de la fonction #
  if(dim(Stations %>% filter(Nom == station)
  )[1] == 0) 
    stop("Attention : nom de station absent de la base de données")
    
    # filtrage en tant que tel 
    Stations <-
    Stations %>% 
    filter(Nom == station)}
  
  ## Extraction de toutes les stations si aucune spécifiée ##
  
  return(Stations)
  
} # Fin de la fonction