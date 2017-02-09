#' Listing des opérations pour une station
#'
#' Cette fonction permet de lister les différentes opérations pour une station donnée
#' 
#' @param station Code de la station
#' @keywords poissons
#' @import dplyr RSQLite DBI lubridate
#' @export
#' @examples
#' poissons.operations()
#' poissons.operations("SOR10-2")

##### TODO LIST #####
# 
#####################

poissons.operations <- function(
  station="")
{

  ## Ouverture de la BDD ##
  ## Connexion à la BDD
  db <- BDD.ouverture(Type = "Poissons")
  #dbListTables(db)
  ## Récupération des données ##
  #Captures <- dbReadTable(db, "Captures")
  Operations <- dbReadTable(db, "Operations")
  Inventaires <- dbReadTable(db, "Inventaires")
  Stations <- dbReadTable(db, "Stations")
  
  ## Synthèse des données ##
  Inventaires <- merge(Inventaires, Stations, by = c("CodeStation"))
  Operations <- merge(Operations, Inventaires, by = c("CodeInventaire"))
  
  ## Format de dates ##
  Operations$DateDebut.x <- ymd_hms(Operations$DateDebut.x)
  Operations$DateDebut.x <- format(Operations$DateDebut.x, "%Y-%m-%d")
  
  ## Extraction des stations ##
  # Test si le nom existe bien, sinon message d'erreur et arrêt de la fonction #
  
  if(dim(Operations %>% filter(Nom == station))[1] == 0 & nchar(station) != 0)
    warning("Attention : nom de station absent des opérations de la base de données")
  
  ## Simplification ##
  # Travail sur une seule station
  if(nchar(station) != 0){
    Operations <- 
      Operations %>%
    select(Nom, DateDebut.x) %>% 
    filter(Nom == station) %>% 
    dplyr::rename(Stations = Nom, Dates = DateDebut.x)}
  
  # Travail sur l'ensemble des opérations
  if(nchar(station) == 0){
    Operations <- 
      Operations %>%
      select(Nom, DateDebut.x) %>% 
      #filter(Nom == station) %>% 
      dplyr::rename(Stations = Nom, Dates = DateDebut.x)}

  return(Operations)
  
} # Fin de la fonction