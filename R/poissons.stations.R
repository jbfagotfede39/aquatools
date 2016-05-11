#' Extraction de données de stations
#'
#' Cette fonction permet d'extraire les données complètes d'une station piscicole
#' 
#' @param Nom de la station
#' @keywords stations
#' @import DBI dplyr RSQLite
#' @export
#' @examples
#' poissons.stations("SOR10-2")

##### TODO LIST #####

#####################

poissons.stations <- function(
  station="SOR10-2")
{
  
  ## Ouverture de la BDD ##
  db <- BDD.ouverture(Type = "Poissons")
  
  ## Récupération des données ##
  #Captures <- dbReadTable(db, "Captures")
  #Operations <- dbReadTable(db, "Operations")
  #IPR <- dbReadTable(db, "IPRs")
  #Inventaires <- dbReadTable(db, "Inventaires")
  Stations <- dbReadTable(db, "Stations")
  
  ## Extraction des afférences ##
  # Test si le nom existe bien, sinon message d'erreur et arrêt de la fonction #
  
  if(dim(Stations %>% filter(Nom == station)
  )[1] == 0) 
    stop("Attention : nom de station absent de la base de données")
  
  station <-
    Stations %>% 
    filter(Nom == station)
  
} # Fin de la fonction