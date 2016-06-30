#' Extraction de données brutes de chronique
#'
#' Permet d'extraire les données de chronique d'une station
#' @keywords chronique
#' @import DBI dplyr lubridate RSQLite stringr
#' @export
#' @examples
#' chronique.mesures("HER0-6", "Thermie") 
#' chronique.mesures("DRO6-8", "Thermie", "2013")
#' chronique.mesures("GCLzhaval", "Piézométrie", "2015", Validé = F)

chronique.mesures <- function(  
  CodeStation ="",
  Type = c("Thermie", "Piézométrie", "Hydrologie", "O2", "Pluviométrie"),
  annee="",
  Validé = T)
{
  
  # library("DBI");library("dplyr");library("lubridate");library("RSQLite");library("stringr")
  # CodeStation ="GCLzhaval";Type="Piézométrie";annee="2015"
  
## Évaluation des choix
  Type <- match.arg(Type)
  

##### Connexion à la BDD #####
db <- BDD.ouverture(Type = "Chroniques")
  
## Récupération des données ##
#Stations <- dbReadTable(db, "Stations")
Mesures <- dbReadTable(db, "Mesures")
#ResultatsAnnuels <- dbReadTable(db, "ResultatsAnnuels")
#SuiviTerrain <- dbReadTable(db, "SuiviTerrain")

##### Filtrage des données #####
## Exclusion des données rejetées ##
if(Validé == T)
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
if(annee != "") {
Mesures$Date <- ymd(Mesures$Date)

Datefin <- ymd(str_c(annee, "-09-30"))
Datedebut <- Datefin - years(1) + days(1)

Mesures <-
Mesures %>% 
  filter(Date >= Datedebut & Date < Datefin + days(1))
}

Mesures <- Mesures %>% arrange(Date, Heure)

return(Mesures)

} # Fin de la fonction