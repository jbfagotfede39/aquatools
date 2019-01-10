#' Listing des placettes pour une station
#'
#' Cette fonction permet de lister les différentes placettes pour une station donnée
#' @name poissons.placettes
#' @param station Code de la station
#' @param complet Données simplifiées \code{FALSE} (par défault) ou complètes \code{TRUE}
#' @keywords poissons
#' @import dplyr 
#' @import RSQLite
#' @import DBI
#' @import lubridate
#' @export
#' @examples
#' poissons.placettes()
#' poissons.placettes("SOR10-2")
#' poissons.placettes(complet=TRUE)
#' poissons.placettes("SOR10-2",complet=TRUE)

##### TODO LIST #####
# 
#####################

poissons.placettes <- function(
  station="",
  complet=FALSE)
{
  
  ## Ouverture de la BDD ##
  ## Connexion à la BDD
  dbP <- BDD.ouverture(Type = "Poissons")
 
  ## Récupération des données ##
  Placettes <- tbl(dbP,"placettes") %>% collect(n = Inf)
  Operations <- tbl(dbP,"operations") %>% collect(n = Inf)
  Inventaires <- tbl(dbP,"inventaires") %>% collect(n = Inf)
  Stations <- tbl(dbP,"stations") %>% collect(n = Inf)
  
  ## Fermeture de la BDD ##
  DBI::dbDisconnect(dbP)
  
  ## Synthèse des données ##
  Inventaires <- left_join(Inventaires, Stations, by = c("codestation"))
  Operations <- left_join(Operations, Inventaires, by = c("codeinventaire"))
  Placettes <- Placettes %>%  left_join(Operations, by = c("codeoperation" = "codeoperation"))
  
  ## Format de dates ##
  Placettes$datedebut.x <- ymd(Placettes$datedebut.x)
  #Placettes$datedebut.x <- ymd_hms(Placettes$datedebut.x)
  #Placettes$datedebut.x <- format(Placettes$datedebut.x, "%Y-%m-%d")
  
  ## Extraction des stations ##
  # Test si le nom existe bien, sinon message d'erreur et arrêt de la fonction #
  
  if(dim(Placettes %>% filter(nom == station))[1] == 0 & nchar(station) != 0)
    warning("Attention : nom de station absent des opérations de la base de données")
  
  ## Simplification ##
  # Travail sur une seule station
  if(nchar(station) != 0 & complet == FALSE){ # Données simplifiées
    Placettes <- 
      Placettes %>%
      filter(nom == station) %>% 
      select(nom, datedebut.x) %>% 
      dplyr::rename(Stations = nom, Dates = datedebut.x)}
  
  if(nchar(station) != 0 & complet == TRUE){ # Données complètes
    Placettes <- 
      Placettes %>%
      filter(nom == station)
      #select(nom, datedebut.x) %>% 
      #dplyr::rename(Stations = nom, Dates = datedebut.x)
    }
  
  # Travail sur l'ensemble des opérations
  if(nchar(station) == 0 & complet == FALSE){ # Données simplifiées
    Placettes <- 
      Placettes %>%
      select(nom, datedebut.x) %>% 
      #filter(nom == station) %>% 
      dplyr::rename(Stations = nom, Dates = datedebut.x)}
  
  if(nchar(station) == 0 & complet == TRUE){ # Données complètes
    Placettes <- 
      Placettes # Rien à faire
      #select(nom, datedebut.x) %>% 
      #filter(nom == station) %>% 
      #dplyr::rename(Stations = nom, Dates = datedebut.x)
    }
  
  return(Placettes)
  
} # Fin de la fonction