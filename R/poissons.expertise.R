#' Exportation des avis à dire d'expert
#'
#' Cette fonction permet d'exporter les avis à dire d'expert des opérations de suivi piscicole
#' 
#' @param station Code de la station
#' @param date Date de la pêche
#' @keywords poissons
#' @import dplyr RSQLite lubridate
#' @export
#' @examples
#' poissons.expertise("SOR10-2", "2015-05-19")

##### TODO LIST #####

#####################

poissons.expertise <- function(
  station="SOR10-2",
  date="2015-05-19")
{
  
  ## Ouverture de la BDD ##
  db <- BDD.ouverture(Type = "Poissons")
  
  ## Récupération des données ##
  #Captures <- dbReadTable(db, "Captures")
  Operations <- dbReadTable(db, "Operations")
  #IPR <- dbReadTable(db, "IPRs")
  Inventaires <- dbReadTable(db, "Inventaires")
  Stations <- dbReadTable(db, "Stations")
  
  ## Synthèse des données ##
  Operations <- merge(Operations, Inventaires, by = c("CodeInventaire"))
  Operations <- merge(Operations, Stations, by = c("CodeStation"))
  
  ## Format de dates ##
  Operations$DateDebut.x <- ymd_hms(Operations$DateDebut.x)
  Operations$DateDebut.x <- format(Operations$DateDebut.x, "%Y-%m-%d")
  
  ## Simplification ##
  Operations <- 
    Operations %>%
    filter(Nom == station, DateDebut.x == date) %>% 
    rename(Station = Nom, Date = DateDebut.x, État = AvisExpertCourt.x, Détails = AvisExpert.x) %>% 
    select(Station, Date, État, Détails) %>% 
    arrange(Station)
  
  # Test si plusieurs résultats #
  
  if(dim(Operations)[1] == 0) 
    stop("Attention : pas d'opération dans la base de données")
  
  if(dim(Operations)[1] > 1) 
    stop("Attention : plusieurs opérations dans la base de données")
  
  ## Sortie ##
  return(Operations)
  
} # Fin de la fonction