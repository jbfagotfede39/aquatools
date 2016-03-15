#' Extraction des données des captures de MI
#'
#' Récupère les données de captures d'une opération de suivi MI
#' @keywords donnees
#' @import dplyr RSQLite DBI lubridate
#' @export
#' @examples
#' MI.captures("VAL37-4", "2013-07-22")

MI.captures <- function(
  station="VAL37-4",
  date="2013-07-22")
{
  
  #library("RSQLite");library("dplyr");library(lubridate)
  
  ##### Connexion à la BDD #####
  ## Connexion à la BDD ##
  db <- BDD.ouverture("Macroinvertébrés")
  #BDD.sauvegarde("Macroinvertébrés")
  
  ##### Récupération des données #####
  Operations <- dbReadTable(db, "Operations")
  Prelevements <- dbReadTable(db, "Prelevements")
  Captures <- dbReadTable(db, "Captures")
  
  #HabitatsReference <- dbReadTable(db, "HabitatsReference")
  #EspecesReference <- dbReadTable(db, "EspecesReference")
  #GenresReference <- dbReadTable(db, "GenresReference")
  #SousFamillesReference <- dbReadTable(db, "SousFamillesReference")
  #FamillesReference <- dbReadTable(db, "FamillesReference")
  #OrdresReference <- dbReadTable(db, "OrdresReference")
  
  ##### Synthèse des données #####
  Prelevements <- merge(Prelevements, Operations, by = c("OperationID"))
  Captures <- merge(Captures, Prelevements, by = c("PrelevementID"))

  ##### Transformation des formats de dates
  Captures$Date <- ymd(Captures$Date)
  date <- ymd(date)
  
  ##### Filtrage #####
  Captures <-
    Captures %>% 
    filter(CodeRDT == station, Date == date) %>%
    arrange(NumEchCommun)
  #str(Captures)
  #Captures %>% filter(Date == date)
  #Captures %>% filter(CodeRDT == station)
  return(Captures)
} # Fin de la fonction