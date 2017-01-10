#' Exportation des IPR
#'
#' Cette fonction permet d'exporter les résultats IPR de pêche
#' 
#' @param station Code de la station
#' @param date Date de la pêche
#' @param expertise \code{TRUE} par défault
#' @keywords poissons
#' @import dplyr DBI RSQLite xlsx lubridate
#' @export
#' @examples
#' poissons.IPR("SOR10-2", "2015-05-19")
#' poissons.IPR("SOR10-2", "2015-05-19", expertise = FALSE)

##### TODO LIST #####

#####################

#library(aquatools);library(dplyr);library(lubridate);library(RSQLite)

poissons.IPR <- function(
  station="LEU15-2",
  date="2013-10-22",
  expertise=TRUE)
{
  
  ## Ouverture de la BDD ##
  db <- BDD.ouverture(Type = "Poissons")
  
  ## Récupération des données ##
  #Captures <- dbReadTable(db, "Captures")
  Operations <- dbReadTable(db, "Operations") %>% select(Codeoperation, AvisExpertCourt, AvisExpert)
  IPR <- dbReadTable(db, "IPRs")
  #Inventaires <- dbReadTable(db, "Inventaires")
  Stations <- dbReadTable(db, "Stations")
  
  ## Synthèse des données ##
  IPR <- left_join(IPR, Operations, by = c("CodeOperation" = "Codeoperation"))
  IPR <- merge(IPR, Stations, by = c("CodeStation"))
  
  ## Format de dates ##
  IPR$DateIPR <- ymd_hms(IPR$DateIPR)
  IPR$DateIPR <- format(IPR$DateIPR, "%Y-%m-%d")
  
  ## Simplification ##
  IPR <- 
    IPR %>%
    select(1:44,51,77) %>% # Pour nettoyage
    select(-(1:2), -(4:6)) %>% # Pour nettoyage
    select(39,41,40,1:35) %>% # Pour remettre le nom de la station en premier
    #select(38,40,39,1:34) %>% # Pour remettre le nom de la station en premier
    filter(Nom == station, DateIPR == date) %>% 
    rename(Stations = Nom, Date = DateIPR) %>% 
    arrange(Stations)
  
  if(expertise == TRUE){
  IPR <-
    IPR %>% 
    select(Stations, CodeSIERMC, Altitude, Date, Classe, Score, Qualite, AvisExpertCourt, AvisExpert, Especes)
  }
  
  if(expertise == FALSE){
    IPR <-
      IPR %>% 
      select(Stations, CodeSIERMC, Altitude, Date, Classe, Score, Qualite, Especes)
  }
  
  ##### Sorties des résultats traités au format Excel #####
  
  #write.xlsx(x = IPR, file = paste0(station, "_", date, "_IPR.xlsx"),
  #           sheetName = paste0(station, " ", date), row.names = F)
  
  return(IPR)
  
} # Fin de la fonction