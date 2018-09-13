#' Chargement des bases de données
#'
#' Cette fonction permet de charger les bases de données de la fédération
#' 
#' @param Type Type de base de données. Chroniques par défaut
#' @import dbplyr
#' @import dplyr
#' @import keyring
#' @import RPostgreSQL
#' @export
#' @examples
#' BDD.ouverture()
#' BDD.ouverture(Type = "Poissons")
#' db <- BDD.ouverture()
#' db <- BDD.ouverture(Type = "Poissons")
#' head(tbl(db,"SuiviTerrain"), 10) %>% collect()

##### TODO LIST #####
#
#####################

BDD.ouverture <- function(
                           Type = c("Chroniques", "Poissons", "Macroinvertébrés", "Physico-chimie", "Temps de travail", "Data"))
{
  
  ## Évaluation des choix
  Type <- match.arg(Type)
  
## Connexion à la BDD ##
drv <- dbDriver("SQLite")

if(system('uname -n',intern=T) == "imac27"){UtilisateurFD <- "JB"}
if(system('uname -n',intern=T) == "MacBookJB.local"){UtilisateurFD <- "JB"}
if(system('uname -n',intern=T) == "MacBookJB"){UtilisateurFD <- "JB"}
if(system('uname -n',intern=T) == "MBP-de-Adrien"){UtilisateurFD <- "Adrien"}
if(system('uname -n',intern=T) == "MacBook-Pro-de-Adrien.local"){UtilisateurFD <- "Adrien"}
if(system('uname -n',intern=T) == "MacBook-Pro-de-Adrien"){UtilisateurFD <- "Adrien"}

## Chroniques ##
if(Type == "Chroniques" & file.exists("/Users/imac27/NAS-DATA/Chroniques/BDD_Chroniques_FD39.sqlite") == T) dbC <- src_sqlite("/Users/imac27/NAS-DATA/Chroniques/BDD_Chroniques_FD39.sqlite")
if(Type == "Chroniques" & file.exists("/Volumes/Fixe-FD39/NAS-DATA/Chroniques/BDD_Chroniques_FD39.sqlite") == T) dbC <- src_sqlite("/Volumes/Fixe-FD39/NAS-DATA/Chroniques/BDD_Chroniques_FD39.sqlite")
if(Type == "Chroniques" & file.exists("/Users/adrienlavigne/NAS-DATA/Chroniques/BDD_Chroniques_FD39.sqlite") == T) dbC <- src_sqlite("/Users/adrienlavigne/NAS-DATA/Chroniques/BDD_Chroniques_FD39.sqlite")

## Macroinvertébrés ##
if(Type == "Macroinvertébrés" & file.exists("/Users/imac27/NAS-DATA/Macroinvertébrés/BDD_MI_FD39.sqlite") == T) dbMI <- src_sqlite("/Users/imac27/NAS-DATA/Macroinvertébrés/BDD_MI_FD39.sqlite")
if(Type == "Macroinvertébrés" & file.exists("/Volumes/Fixe-FD39/NAS-DATA/Macroinvertébrés/BDD_MI_FD39.sqlite") == T) dbMI <- src_sqlite("/Volumes/Fixe-FD39/NAS-DATA/Macroinvertébrés/BDD_MI_FD39.sqlite")
if(Type == "Macroinvertébrés" & file.exists("/Users/adrienlavigne/NAS-DATA/Macroinvertébrés/BDD_MI_FD39.sqlite") == T) dbMI <- src_sqlite("/Users/adrienlavigne/NAS-DATA/Macroinvertébrés/BDD_MI_FD39.sqlite")

## Physico-chimie ##
if(Type == "Physico-chimie" & file.exists("/Users/imac27/NAS-DATA/Physico-chimie/BDD_Physico-chimie_FD39.sqlite") == T) dbPC <- src_sqlite("/Users/imac27/NAS-DATA/Physico-chimie/BDD_Physico-chimie_FD39.sqlite")
if(Type == "Physico-chimie" & file.exists("/Volumes/Fixe-FD39/NAS-DATA/Physico-chimie/BDD_Physico-chimie_FD39.sqlite") == T) dbPC <- src_sqlite("/Volumes/Fixe-FD39/NAS-DATA/Physico-chimie/BDD_Physico-chimie_FD39.sqlite")
if(Type == "Physico-chimie" & file.exists("/Users/adrienlavigne/NAS-DATA/Physico-chimie/BDD_Physico-chimie_FD39.sqlite") == T) dbPC <- src_sqlite("/Users/adrienlavigne/NAS-DATA/Physico-chimie/BDD_Physico-chimie_FD39.sqlite")

## Temps de travail ##
if(Type == "Temps de travail" & file.exists("/Users/imac27/NAS-FD/FD39/Activité/Temps de travail/BDD_Tps_travail_FD39.sqlite") == T) dbTW <- src_sqlite("/Users/imac27/NAS-FD/FD39/Activité/Temps de travail/BDD_Tps_travail_FD39.sqlite")
if(Type == "Temps de travail" & file.exists("/Volumes/Fixe-FD39/NAS-FD/FD39/Activité/Temps de travail/BDD_Tps_travail_FD39.sqlite") == T) dbTW <- src_sqlite("/Volumes/Fixe-FD39/NAS-FD/FD39/Activité/Temps de travail/BDD_Tps_travail_FD39.sqlite")

if(Type == "Poissons"){
  if(strsplit(system('system_profiler SPNetworkDataType | grep RouterHardwareAddress',intern=T), "RouterHardwareAddress=")[[1]][2] == "ac:84:c9:42:d2:8d"){
  dbP <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                       dbname = "multifish",
                       host = '192.168.1.2',
                       port = 5432,
                       user = UtilisateurFD,
                       password = keyring::key_get("Multifish")
                       )
  }else{
  dbP <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                       dbname = "multifish",
                       host = '80.11.169.205',
                       port = 5432,
                       user = UtilisateurFD,
                       password = keyring::key_get("Multifish")
                       )
  }
}

if(Type == "Data" & exists("dbD") == FALSE){
  if(strsplit(system('system_profiler SPNetworkDataType | grep RouterHardwareAddress',intern=T), "RouterHardwareAddress=")[[1]][2] == "ac:84:c9:42:d2:8d"){
    dbD <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                          dbname = "nas-sig-data",
                          host = '192.168.1.14',
                          #host = '80.11.169.205',
                          port = 5433,
                          user = UtilisateurFD,
                          password = keyring::key_get("nas-sig-data")
    )
  }else{
    dbD <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                          dbname = "nas-sig-data",
                          #host = '192.168.1.14',
                          host = '80.11.169.205',
                          port = 5433,
                          user = UtilisateurFD,
                          password = keyring::key_get("nas-sig-data")
    )
  }
}

if(Type == "Chroniques"){return(dbC)}
if(Type == "Macroinvertébrés"){return(dbMI)}
if(Type == "Physico-chimie"){return(dbPC)}
if(Type == "Temps de travail"){return(dbTW)}
if(Type == "Poissons"){return(dbP)}
if(Type == "Data"){return(dbD)}

} # Fin de la fonction
