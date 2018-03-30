#' Exportation des IPR
#'
#' Cette fonction permet d'exporter les résultats IPR de pêche
#' 
#' @param ListeOperations Dataframe contenant un colonne "Station" avec le code de la station (RHJ) et une colonne "Date"
#' @param expertise \code{TRUE} par défault
#' @keywords poissons
#' @import dplyr DBI RSQLite lubridate
#' @export
#' @examples
#' poissons.IPR()
#' poissons.IPR(listeOperations)
#' poissons.IPR(data.frame(Station = "SOR10-2", Date = "2012-11-03"))
#' poissons.IPR(data.frame(Station = "SOR10-2", Date = "2012-11-03"), expertise = FALSE)

##### TODO LIST #####

#####################

#library(aquatools);library(dplyr);library(lubridate);library(RSQLite)

poissons.IPR <- function(
  ListeOperations = data.frame(Station = character(0), Date = character(0)),
  expertise=TRUE)
{
  
  ## Ouverture de la BDD ##
  db <- BDD.ouverture(Type = "Poissons")
  
  ## Récupération des données ##
  Operations <- tbl(db,"Operations") %>% collect(n = Inf) %>% select(Codeoperation, AvisExpertCourt, AvisExpert)
  Stations <- tbl(db,"Stations") %>% collect(n = Inf)
  IPR <- tbl(db,"IPRs") %>% collect(n = Inf)
  
  ## Synthèse des données ##
  IPR <- left_join(IPR, Operations, by = c("CodeOperation" = "Codeoperation"))
  IPR <- left_join(IPR, Stations, by = c("CodeStation"))

  ## Format de dates ##
  IPR$DateIPR <- ymd_hms(IPR$DateIPR)
  IPR$DateIPR <- format(IPR$DateIPR, "%Y-%m-%d")
  
  ## Simplification ##
  IPR <- 
    IPR %>%
    rename(Station = Nom, Date = DateIPR) %>% 
    arrange(Station, Date)
  
  # Travail sur toutes les opérations #
  if(dim(ListeOperations)[1] == 0 & expertise == TRUE){
  IPR <-
    IPR %>% 
    select(Station, CodeSIERMC, Altitude, Date, Classe, Score, Qualite, AvisExpertCourt, AvisExpert, Especes)
  }
  
  if(dim(ListeOperations)[1] == 0 & expertise == FALSE){
    IPR <-
      IPR %>% 
      select(Station, CodeSIERMC, Altitude, Date, Classe, Score, Qualite, Especes)
  }
  
  # Travail sur quelques opérations #
  if(dim(ListeOperations)[1] != 0 & expertise == TRUE){
    IPR <-
      IPR %>% 
      select(Station, CodeSIERMC, Altitude, Date, Classe, Score, Qualite, AvisExpertCourt, AvisExpert, Especes) %>% 
      filter(Station %in% ListeOperations$Station & Date %in% ListeOperations$Date)
  }
  
  if(dim(ListeOperations)[1] != 0 & expertise == FALSE){
    IPR <-
      IPR %>% 
      select(Station, CodeSIERMC, Altitude, Date, Classe, Score, Qualite, Especes) %>% 
      filter(Station %in% ListeOperations$Station & Date %in% ListeOperations$Date)
  }
  
  # Rendu du résultat #
  return(IPR)
  
} # Fin de la fonction