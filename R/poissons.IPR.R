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
  dbP <- BDD.ouverture(Type = "Poissons")
  
  ## Récupération des données ##
  Operations <- tbl(dbP,"operations") %>% collect(n = Inf) %>% select(codeoperation, avisexpertcourt, avisexpert)
  Stations <- tbl(dbP,"stations") %>% collect(n = Inf)
  IPR <- tbl(dbP,"iprs") %>% collect(n = Inf)
  
  ## Synthèse des données ##
  IPR <- left_join(IPR, Operations, by = c("codeoperation" = "codeoperation"))
  IPR <- left_join(IPR, Stations, by = c("codestation"))

  ## Format de dates ##
  IPR$dateipr <- ymd(IPR$dateipr)
  IPR$dateipr <- format(IPR$dateipr, "%Y-%m-%d")
  
  ## Simplification ##
  IPR <- 
    IPR %>%
    rename(Station = nom, Date = dateipr) %>% 
    arrange(Station, Date) %>% 
    rename(CodeSIERMC = codesiermc, Altitude = altitude, Classe = classe, Score = score, Qualite = qualite, AvisExpertCourt = avisexpertcourt, AvisExpert = avisexpert, Especes = especes)
  
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
    ListeOperations <- ListeOperations %>% mutate(Cle = paste0(Station, " - ", Date))
    IPR <-
      IPR %>% 
      mutate(Cle = paste0(Station, " - ", Date)) %>% 
      filter(Cle %in% ListeOperations$Cle) %>% 
      select(Station, CodeSIERMC, Altitude, Date, Classe, Score, Qualite, AvisExpertCourt, AvisExpert, Especes)
  }
  
  if(dim(ListeOperations)[1] != 0 & expertise == FALSE){
    ListeOperations <- ListeOperations %>% mutate(Cle = paste0(Station, " - ", Date))
    IPR <-
      IPR %>% 
      mutate(Cle = paste0(Station, " - ", Date)) %>% 
      filter(Cle %in% ListeOperations$Cle) %>% 
      select(Station, CodeSIERMC, Altitude, Date, Classe, Score, Qualite, Especes)
  }
  
  # Rendu du résultat #
  return(IPR)
  DBI::dbDisconnect(dbP)
  
} # Fin de la fonction