#' Extraction de données brutes de chronique
#'
#' Permet d'extraire les données de chronique d'une station pour une année biologique
#' @keywords donnees
#' @import DBI dplyr lubridate RSQLite stringr
#' @export
#' @examples
#' chronique.mesures("DRO6-8", "Thermie", "2013")

thermie.mesures <- function(  
  CodeStation ="DRO6-8",
  Type = c("Thermie", "Piézométrie", "Hydrologie", "O2", "Pluviométrie"),
  annee="2013")
{
  
  # library("DBI");library("dplyr");library("lubridate");library("RSQLite");library("stringr")
  
## Évaluation des choix
  TypeMesure <- match.arg(TypeMesure)
  

##### Connexion à la BDD #####
db <- BDD.ouverture(Type = "Chroniques")
  
## Récupération des données ##
#Stations <- dbReadTable(db, "Stations")
Mesures <- dbReadTable(db, "Mesures")
#ResultatsAnnuels <- dbReadTable(db, "ResultatsAnnuels")
#SuiviTerrain <- dbReadTable(db, "SuiviTerrain")

##### Filtrage des données #####
## Exclusion des données rejetées ##
Mesures <-
  Mesures %>% 
  filter(Validation == "Validé")

## Station ##
Mesures <-
  Mesures %>% 
  filter(CodeRDT == CodeStation)

## TypeMesure ##
Mesures <-
  Mesures %>% 
  filter(TypeMesure == Type)

## Période ##
Mesures$Date <- ymd(Mesures$Date)

Datefin <- ymd(str_c(annee, "-09-30"))
Datedebut <- Datefin - years(1) + days(1)

Mesures <-
Mesures %>% 
  filter(Date >= Datedebut & Date < Datefin + days(1))

return(Mesures)

} # Fin de la fonction