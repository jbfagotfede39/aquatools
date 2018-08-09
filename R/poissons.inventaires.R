#' Listing des inventaires pour une station
#'
#' Cette fonction permet de lister les différents inventaires pour une station donnée ou l'ensemble de la BDD
#' 
#' @param station Code de la station
#' @param all Affichage de tous les paramètres de la table - \code{FALSE} (par défault) 
#' @keywords poissons
#' @import dplyr RSQLite DBI lubridate
#' @export
#' @examples
#' poissons.inventaires()
#' poissons.inventaires("SOR10-2")
#' poissons.inventaires(all=T)

##### TODO LIST #####
# 
#####################

poissons.inventaires <- function(
  station="",
  all=FALSE)
{
  
  ## Ouverture de la BDD ##
  ## Connexion à la BDD
  if(exists("dbP") == FALSE){
    dbP <- BDD.ouverture(Type = "Poissons")
    assign("dbP", dbP, envir = .GlobalEnv)
  }

  ## Récupération des données ##
  Inventaires <- tbl(dbP, "inventaires") %>% collect(n = Inf)
  Stations <- tbl(dbP, "stations") %>% collect(n = Inf)
  
  ## Synthèse des données ##
  Inventaires <- merge(Inventaires, Stations, by = c("codestation"))
  
  ## Format de dates ##
  Inventaires$DateDebut <- ymd(Inventaires$DateDebut)
  
  ## Extraction des stations ##
  # Test si le nom existe bien, sinon message d'erreur et arrêt de la fonction #
  
  if(dim(Inventaires %>% filter(Nom == station))[1] == 0 & nchar(station) != 0)
    warning("Attention : nom de station absent des inventaires de la base de données")
  
  ## Simplification ##
  # Travail sur une seule station
  if(nchar(station) != 0 & all == F){
    Inventaires <- 
      Inventaires %>%
      select(Nom, DateDebut) %>% 
      filter(Nom == station) %>% 
      dplyr::rename(Stations = Nom, Dates = DateDebut)}
  
  if(nchar(station) != 0 & all == T){
    Inventaires <- 
      Inventaires %>%
      #select(Nom, DateDebut) %>% 
      filter(Nom == station) %>% 
      dplyr::rename(Stations = Nom, Dates = DateDebut)}
  
  # Travail sur l'ensemble des opérations
  if(nchar(station) == 0 & all == F){
    Inventaires <- 
      Inventaires %>%
      select(Nom, DateDebut) %>% 
      #filter(Nom == station) %>% 
      dplyr::rename(Stations = Nom, Dates = DateDebut)}
  
  if(nchar(station) == 0 & all == T){
    Inventaires <- 
      Inventaires %>%
      #select(Nom, DateDebut) %>% 
      #filter(Nom == station) %>% 
      dplyr::rename(Stations = Nom, Dates = DateDebut)}
  
  return(Inventaires)
  
} # Fin de la fonction