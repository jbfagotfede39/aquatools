#' Extraction de données de thermie
#'
#' Permet d'extraire les données de thermie d'une station pour une année biologique
#' @keywords donnees
#' @import DBI dplyr lubridate RSQLite stringr
#' @export
#' @examples
#' thermie.mesures("DRO6-8", "2013")

thermie.mesures <- function(  
  station="DRO6-8",
  annee="2013")
{
  
  # library("DBI");library("dplyr");library("lubridate");library("RSQLite");library("stringr")
  
##### Connexion à la BDD #####
drv <- dbDriver("SQLite")
if (file.exists("/Users/imac27/hubiC/Données/Thermie/Exploitation_données_thermie/BDD_Thermie_FD39.sqlite") == T) db <- dbConnect(drv, dbname="/Users/imac27/hubiC/Données/Thermie/Exploitation_données_thermie/BDD_Thermie_FD39.sqlite")
if (file.exists("/Users/jean-baptistefagot_FD39/hubiC/Données/Thermie/Exploitation_données_thermie/BDD_Thermie_FD39.sqlite") == T) db <- dbConnect(drv, dbname="/Users/jean-baptistefagot_FD39/hubiC/Données/Thermie/Exploitation_données_thermie/BDD_Thermie_FD39.sqlite")
  
## Récupération des données ##
Stations <- dbReadTable(db, "Stations")
Mesures <- dbReadTable(db, "Mesures")
ResultatsAnnuels <- dbReadTable(db, "ResultatsAnnuels")
SuiviTerrain <- dbReadTable(db, "SuiviTerrain")

##### Filtrage des données #####
## Exclusion des données rejetées ##
Mesures <-
  Mesures %>% 
  filter(Validation == "Validé")

## Station ##
Mesures <-
  Mesures %>% 
  filter(CodeRDT == station)

## Période ##
Mesures$Date <- ymd(Mesures$Date)

Datefin <- ymd(str_c(annee, "-09-30"))
Datedebut <- Datefin - years(1) + days(1)

Mesures <-
Mesures %>% 
  filter(Date >= Datedebut & Date < Datefin + days(1))

return(Mesures)

} # Fin de la fonction