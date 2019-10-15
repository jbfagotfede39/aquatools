#' Extraction des résultats pour une station
#'
#' Cette fonction permet de collecter les différents résultats pour une liste donnée de stations
#' @name poissons.resultats
#' @param ListeStations Dataframe contenant un colonne "Nom" avec le code de la station (RHJ)
#' @param Sortie Forme du dataframe de sortie - \code{Simple} (par défault), \code{Propre} (format diffusable, avec stations) ou \code{Complet} (tous les champs)
#' @keywords donnees
#' @import dplyr 
#' @import RSQLite
#' @import DBI 
#' @import lubridate
#' @export
#' @examples
#' poissons.resultats()
#' Resultats <- poissons.resultats()
#' poissons.resultats(data.frame(Nom = "SOR10-2"), Sortie = "Propre")

poissons.resultats <- function(
  ListeStations = data.frame(Nom = character(0)),
  Sortie = c("Simple","Propre","Complet")
)
  {
  
  ## Évaluation des choix ##
  Sortie <- match.arg(Sortie)
  
  ## Ouverture de la BDD ##
  dbP <- BDD.ouverture(Type = "Poissons")
  
  ##### Récupération des données #####
  Resultats <- tbl(dbP,"resultats") %>% collect(n = Inf)
  Operations <- tbl(dbP,"operations") %>% collect(n = Inf)
  Inventaires <- tbl(dbP,"inventaires") %>% collect(n = Inf)
  Stations <- tbl(dbP,"stations") %>% collect(n = Inf)
  Ecosystemes <- tbl(dbP,"ecosystemes") %>% collect(n = Inf)
  Communes <- tbl(dbP,"communes") %>% collect(n = Inf)
  
  ## Fermeture de la BDD ##
  DBI::dbDisconnect(dbP)
  
  ## Renommage des colonnes Observations ##
  Resultats <- Resultats %>% rename(ObservationsResultats = observations)
  Inventaires <- Inventaires %>% rename(ObservationInventaires = observations)
  Stations <- Stations %>% rename(ObservationStations = observations)
  Ecosystemes <- Ecosystemes %>% rename(ObservationsEcosystemes = observations)
  Communes <- Communes %>% rename(ObservationsCommunes = observations)
  
  ##### Synthèse des données #####
  Inventaires <- left_join(Inventaires, Stations, by = c("codestation"))
  Operations <- left_join(Operations, Inventaires, by = c("codeinventaire"))
  Resultats <- left_join(Resultats, Operations, by = c("codeoperation"))
  
  ##### Remplacement des codes communes et écosystèmes #####
  Resultats <- left_join(Resultats, Ecosystemes, by = c("codeecosysteme.y" = "codeecosysteme"))
  Communes <- select(Communes, codecommune, commune)
  Resultats <- left_join(Resultats, Communes, by = c("codecommune"))
  
  ##### Transformation des formats ####
  ## Dates ##
  Resultats$datedebut.x <- ymd(Resultats$datedebut.x)
  
  ## Arrondis ##
  Resultats$densitenumeriquebrute <- round(Resultats$densitenumeriquebrute,1)
  Resultats$densiteponderalebrute <- round(Resultats$densiteponderalebrute,1)
  Resultats$densitenumeriqueestimee <- round(Resultats$densitenumeriqueestimee,1)
  Resultats$densiteponderaleestimee <- round(Resultats$densiteponderaleestimee,1)
  
  ##### Filtrage #####
if(dim(ListeStations)[1] != 0){
  Resultats <-
    Resultats %>% 
    filter(nom %in% ListeStations$Nom) %>% 
    arrange(codeespece)
}
  
  ##### Simplification #####
if(dim(ListeStations)[1] == 0 & Sortie == "Simple"){
 Resultats <-
   Resultats %>%
   dplyr::rename(Station = nom, Date = datedebut.x) %>% 
   select(Station, Date, codeespece)
}
  
  if(dim(ListeStations)[1] == 0 & Sortie == "Propre"){
    stop("Paramétrage de la sortie à développer")
    # Resultats <-
    #   Resultats %>%
    #   select(Nom, DateDebut, Codeespece, TailleMinimum, TailleMaximum, Nombre, Poids)
  }
  
  if(dim(ListeStations)[1] != 0 & Sortie == "Propre"){
    stop("Paramétrage de la sortie à développer")
    # Resultats <-
    #   Resultats %>%
    #   select(Nom, DateDebut, Codeespece, TailleMinimum, TailleMaximum, Nombre, Poids)
  }

  return(Resultats)
} # Fin de la fonction