#' Extraction des données des résultats d'une opération
#'
#' Récupère les données de résultats d'une opération dans Maxifish pour les mettre dans un dataframe
#' @keywords donnees
#' @import dplyr RSQLite DBI lubridate
#' @export
#' @examples
#' poissons.resultats("SOR10-2", "2015-05-19")

poissons.resultats <- function(
  station="SOR10-2",
  date="2015-05-19")
  {
  
  #library("RSQLite");library("dplyr")
  
  
  ## Ouverture de la BDD ##
  db <- BDD.ouverture(Type = "Poissons")
  
  ##### Récupération des données #####
  Resultats <- dbReadTable(db, "Resultats")
  Operations <- dbReadTable(db, "Operations")
  Inventaires <- dbReadTable(db, "Inventaires")
  Stations <- dbReadTable(db, "Stations")
  Ecosystemes <- dbReadTable(db, "Ecosystemes")
  Communes <- dbReadTable(db, "Communes")
  
  ## Renommage des colonnes Observations ##
  Resultats <- Resultats %>% rename(ObservationsResultats = Observations)
  Inventaires <- Inventaires %>% rename(ObservationInventaires = Observations)
  Stations <- Stations %>% rename(ObservationStations = Observations)
  Ecosystemes <- Ecosystemes %>% rename(ObservationsEcosystemes = Observations)
  Communes <- Communes %>% rename(ObservationsCommunes = Observations)
  
  ##### Synthèse des données #####
  Inventaires <- merge(Inventaires, Stations, by = c("CodeStation"))
  Operations <- merge(Operations, Inventaires, by = c("CodeInventaire"))
  Resultats <- merge(Resultats, Operations, by = c("Codeoperation"))
  
  ##### Remplacement des codes communes et écosystèmes #####
  Resultats <- merge(Resultats, Ecosystemes, by = c("Codeecosysteme"))
  Communes <- select(Communes, CodeCommune, Commune)
  Resultats <- merge(Resultats, Communes, by = c("CodeCommune"))
  
  ##### Transformation des formats de dates
  Resultats$DateDebut.x <- ymd_hms(Resultats$DateDebut.x)
  Resultats$DateDebut.x <- format(Resultats$DateDebut.x, "%Y-%m-%d")
  
  ##### Filtrage #####
  Resultats <-
    Resultats %>% 
    filter(Nom == station, DateDebut.x == date) %>%
    #select(Nom, DateDebut.x, Codeespece, N_SommeCapturePassage1, N_SommeCapturePassage2, N_SommeCapturePassage3, NombreTotalCaptures, DensiteNumeriqueBrute, BiomasseTotaleCapturee, DensitePonderaleBrute) %>%
    arrange(Codeespece)
  
  Resultats$DensiteNumeriqueBrute <- round(Resultats$DensiteNumeriqueBrute,1)
  Resultats$DensitePonderaleBrute <- round(Resultats$DensitePonderaleBrute,1)
  Resultats$DensiteNumeriqueestimee <- round(Resultats$DensiteNumeriqueestimee,1)
  Resultats$DensitePonderaleestimee <- round(Resultats$DensitePonderaleestimee,1)
  
  ##### Simplification #####
#  Resultats <- 
#    Resultats %>%
#    select(Nom, DateDebut, Codeespece, TailleMinimum, TailleMaximum, Nombre, Poids)

  return(Resultats)
} # Fin de la fonction

# Resultats <- poissons.resultats() pour avoir les données en utilisant la fonction