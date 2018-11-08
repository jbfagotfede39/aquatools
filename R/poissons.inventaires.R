#' Listing des inventaires pour une station
#'
#' Cette fonction permet de lister les différents inventaires pour une station donnée ou l'ensemble de la BDD
#' @name poissons.inventaires
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
  dbP <- BDD.ouverture(Type = "Poissons")

  ## Récupération des données ##
  Inventaires <- tbl(dbP, "inventaires") %>% collect(n = Inf)
  Stations <- tbl(dbP, "stations") %>% collect(n = Inf)
  
  ## Fermeture de la BDD ##
  DBI::dbDisconnect(dbP)
  
  ## Synthèse des données ##
  Inventaires <- merge(Inventaires, Stations, by = c("codestation"))
  
  ## Format de dates ##
  Inventaires$datedebut <- ymd(Inventaires$datedebut)
  
  ## Extraction des stations ##
  # Test si le nom existe bien, sinon message d'erreur et arrêt de la fonction #
  
  if(dim(Inventaires %>% filter(nom == station))[1] == 0 & nchar(station) != 0)
    warning("Attention : nom de station absent des inventaires de la base de données")
  
  ## Simplification ##
  # Travail sur une seule station
  if(nchar(station) != 0 & all == F){
    Inventaires <- 
      Inventaires %>%
      select(nom, datedebut) %>% 
      filter(nom == station) %>% 
      dplyr::rename(Stations = nom, Dates = datedebut)}
  
  if(nchar(station) != 0 & all == T){
    Inventaires <- 
      Inventaires %>%
      #select(nom, datedebut) %>% 
      filter(nom == station) %>% 
      dplyr::rename(Stations = nom, Dates = datedebut)}
  
  # Travail sur l'ensemble des opérations
  if(nchar(station) == 0 & all == F){
    Inventaires <- 
      Inventaires %>%
      select(nom, datedebut) %>% 
      #filter(nom == station) %>% 
      dplyr::rename(Stations = nom, Dates = datedebut)}
  
  if(nchar(station) == 0 & all == T){
    Inventaires <- 
      Inventaires %>%
      #select(nom, datedebut) %>% 
      #filter(nom == station) %>% 
      dplyr::rename(Stations = nom, Dates = datedebut)}
  
  return(Inventaires)
  
} # Fin de la fonction