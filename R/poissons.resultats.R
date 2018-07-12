#' Extraction des données des résultats d'une opération
#'
#' Récupère les données de résultats d'une opération dans Maxifish pour les mettre dans un dataframe
#' @keywords donnees
#' @import dplyr 
#' @import RSQLite
#' @import DBI 
#' @import lubridate
#' @export
#' @examples
#' poissons.resultats("SOR10-2", "2015-05-19")

poissons.resultats <- function(
  station="SOR10-2",
  date="2015-05-19")
  {
  
  #library("RSQLite");library("dplyr")
  
  
  ## Ouverture de la BDD ##
  dbP <- BDD.ouverture(Type = "Poissons")
  
  ##### Récupération des données #####
  Resultats <- tbl(dbP,"resultats") %>% collect(n = Inf)
  Operations <- tbl(dbP,"operations") %>% collect(n = Inf)
  Inventaires <- tbl(dbP,"inventaires") %>% collect(n = Inf)
  Stations <- tbl(dbP,"stations") %>% collect(n = Inf)
  Ecosystemes <- tbl(dbP,"ecosystemes") %>% collect(n = Inf)
  Communes <- tbl(dbP,"communes") %>% collect(n = Inf)
  
  ## Renommage des colonnes Observations ##
  Resultats <- Resultats %>% rename(ObservationsResultats = Observations)
  Inventaires <- Inventaires %>% rename(ObservationInventaires = Observations)
  Stations <- Stations %>% rename(ObservationStations = Observations)
  Ecosystemes <- Ecosystemes %>% rename(ObservationsEcosystemes = Observations)
  Communes <- Communes %>% rename(ObservationsCommunes = Observations)
  
  ##### Synthèse des données #####
  Inventaires <- merge(Inventaires, Stations, by = c("codestation"))
  Operations <- merge(Operations, Inventaires, by = c("codeinventaire"))
  Resultats <- merge(Resultats, Operations, by = c("codeoperation"))
  
  ##### Remplacement des codes communes et écosystèmes #####
  Resultats <- merge(Resultats, Ecosystemes, by = c("codeecosysteme"))
  Communes <- select(Communes, CodeCommune, Commune)
  Resultats <- merge(Resultats, Communes, by = c("codecommune"))
  
  ##### Transformation des formats de dates
  Resultats$datedebut.x <- ymd(Resultats$datedebut.x)
  
  ##### Filtrage #####
  Resultats <-
    Resultats %>% 
    filter(nom == station, datedebut.x == date) %>%
    #select(Nom, DateDebut.x, Codeespece, N_SommeCapturePassage1, N_SommeCapturePassage2, N_SommeCapturePassage3, NombreTotalCaptures, DensiteNumeriqueBrute, BiomasseTotaleCapturee, DensitePonderaleBrute) %>%
    arrange(codeespece)
  
  Resultats$densitenumeriquebrute <- round(Resultats$densitenumeriquebrute,1)
  Resultats$densiteponderalebrute <- round(Resultats$densiteponderalebrute,1)
  Resultats$densitenumeriqueestimee <- round(Resultats$densitenumeriqueestimee,1)
  Resultats$densiteponderaleestimee <- round(Resultats$densiteponderaleestimee,1)
  
  ##### Simplification #####
#  Resultats <- 
#    Resultats %>%
#    select(Nom, DateDebut, Codeespece, TailleMinimum, TailleMaximum, Nombre, Poids)

  return(Resultats)
  DBI::dbDisconnect(dbP)
} # Fin de la fonction

# Resultats <- poissons.resultats() pour avoir les données en utilisant la fonction