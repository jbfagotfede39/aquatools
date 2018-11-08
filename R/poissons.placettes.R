#' Listing des opérations pour une station
#'
#' Cette fonction permet de lister les différentes opérations pour une station donnée
#' @name poissons.placettes
#' @param station Code de la station
#' @param complet Données simplifiées \code{FALSE} (par défault) ou complètes \code{TRUE}
#' @keywords poissons
#' @import dplyr RSQLite DBI lubridate
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
  Inventaires <- merge(Inventaires, Stations, by = c("CodeStation"))
  Operations <- merge(Operations, Inventaires, by = c("CodeInventaire"))
  Placettes <- Placettes %>%  left_join(Operations, by = c("CodeOperation" = "Codeoperation"))
  
  ## Format de dates ##
  Placettes$DateDebut.x <- ymd_hms(Placettes$DateDebut.x)
  Placettes$DateDebut.x <- format(Placettes$DateDebut.x, "%Y-%m-%d")
  
  ## Extraction des stations ##
  # Test si le nom existe bien, sinon message d'erreur et arrêt de la fonction #
  
  if(dim(Placettes %>% filter(Nom == station))[1] == 0 & nchar(station) != 0)
    warning("Attention : nom de station absent des opérations de la base de données")
  
  ## Simplification ##
  # Travail sur une seule station
  if(nchar(station) != 0 & complet == FALSE){ # Données simplifiées
    Placettes <- 
      Placettes %>%
      filter(Nom == station) %>% 
      select(Nom, DateDebut.x) %>% 
      dplyr::rename(Stations = Nom, Dates = DateDebut.x)}
  
  if(nchar(station) != 0 & complet == TRUE){ # Données complètes
    Placettes <- 
      Placettes %>%
      filter(Nom == station)
      #select(Nom, DateDebut.x) %>% 
      #dplyr::rename(Stations = Nom, Dates = DateDebut.x)
    }
  
  # Travail sur l'ensemble des opérations
  if(nchar(station) == 0 & complet == FALSE){ # Données simplifiées
    Placettes <- 
      Placettes %>%
      select(Nom, DateDebut.x) %>% 
      #filter(Nom == station) %>% 
      dplyr::rename(Stations = Nom, Dates = DateDebut.x)}
  
  if(nchar(station) == 0 & complet == TRUE){ # Données complètes
    Placettes <- 
      Placettes # Rien à faire
      #select(Nom, DateDebut.x) %>% 
      #filter(Nom == station) %>% 
      #dplyr::rename(Stations = Nom, Dates = DateDebut.x)
    }
  
  return(Placettes)
  
} # Fin de la fonction