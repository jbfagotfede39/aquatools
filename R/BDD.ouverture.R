#' Chargement des bases de données
#'
#' Cette fonction permet de charger les bases de données de la fédération
#' @name BDD.ouverture
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

#### Utilisateur ####
if(system('uname -n',intern=T) == "imac27"){UtilisateurFD <- "JB"}
if(system('uname -n',intern=T) == "imac27.local"){UtilisateurFD <- "JB"}
if(system('uname -n',intern=T) == "Client_imacJB"){UtilisateurFD <- "JB"}
if(system('uname -n',intern=T) == "Client_imacJB.local"){UtilisateurFD <- "JB"}
if(system('uname -n',intern=T) == "Client_MacBookJB"){UtilisateurFD <- "JB"}
if(system('uname -n',intern=T) == "Client_MacBookJB.local"){UtilisateurFD <- "JB"}
if(system('uname -n',intern=T) == "MacBookJB.local"){UtilisateurFD <- "JB"}
if(system('uname -n',intern=T) == "MacBookJB"){UtilisateurFD <- "JB"}
if(system('uname -n',intern=T) == "MBP-de-Adrien"){UtilisateurFD <- "adrien"}
if(system('uname -n',intern=T) == "MBP-de-Adrien.local"){UtilisateurFD <- "adrien"}
if(system('uname -n',intern=T) == "macbook-pro-de-adrien"){UtilisateurFD <- "adrien"}
if(system('uname -n',intern=T) == "macbook-pro-de-adrien.home"){UtilisateurFD <- "adrien"}
if(system('uname -n',intern=T) == "macbook-pro-de-adrien.local"){UtilisateurFD <- "adrien"}
if(system('uname -n',intern=T) == "MacBook-Pro-de-Adrien"){UtilisateurFD <- "adrien"}
if(system('uname -n',intern=T) == "MacBook-Pro-de-Adrien.local"){UtilisateurFD <- "adrien"}
if(system('uname -n',intern=T) == "MacBook-Pro-de-Adrien.home"){UtilisateurFD <- "adrien"}
if(system('uname -n',intern=T) == "Client_MBP-de-Adrien"){UtilisateurFD <- "adrien"}
if(system('uname -n',intern=T) == "Client_MBP-de-Adrien.local"){UtilisateurFD <- "adrien"}
if(system('uname -n',intern=T) == "iMac-de-Quentin"){UtilisateurFD <- "Quentin"}
if(system('uname -n',intern=T) == "iMac-de-Quentin.local"){UtilisateurFD <- "Quentin"}
if(system('uname -n',intern=T) == "Client_iMac-de-Quentin"){UtilisateurFD <- "Quentin"}
if(system('uname -n',intern=T) == "Client_iMac-de-Quentin.local"){UtilisateurFD <- "Quentin"}
if(system('uname -n',intern=T) == "postgis"){UtilisateurFD <- "automate"}

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
  if(Type == "Poissons" & exists("dbP") == TRUE){if(RPostgreSQL::isPostgresqlIdCurrent(dbP) == FALSE){dbDisconnect(dbP)}}
  if(Type == "Poissons" & exists("dbP") == FALSE){
  if(strsplit(system('system_profiler SPNetworkDataType | grep RouterHardwareAddress',intern=T), "RouterHardwareAddress=")[[1]][2] == "04:92:26:6c:9a:d8"){
  if(UtilisateurFD == "adrien"){UtilisateurFD <- "Adrien"} # car pb de majuscule pour l'identification
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
}

if(Type == "Data" & exists("dbD") == FALSE){
  if(UtilisateurFD == "Quentin") UtilisateurFD <- "quentin"
  if(UtilisateurFD == "Adrien") UtilisateurFD <- "adrien"
  if(UtilisateurFD == "JB") UtilisateurFD <- "jb"
  if(strsplit(system('system_profiler SPNetworkDataType | grep RouterHardwareAddress',intern=T), "RouterHardwareAddress=")[[1]][2] == "04:92:26:6c:9a:d8"){
    dbD <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                          dbname = "eaux-jura-sig-data",
                          host = '192.168.1.100',
                          #host = '80.11.169.205',
                          port = 3254,
                          user = UtilisateurFD,
                          password = keyring::key_get("eaux-jura-sig-data")
    )
  }else{
    dbD <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                          dbname = "eaux-jura-sig-data",
                          #host = '192.168.1.100',
                          host = '80.11.169.205',
                          port = 3254,
                          user = UtilisateurFD,
                          password = keyring::key_get("eaux-jura-sig-data")
    )
  }
}
if(Type == "Data"){
if(Type == "Data" & exists("dbD") == TRUE & RPostgreSQL::isPostgresqlIdCurrent(dbD) == FALSE){
  if(UtilisateurFD == "Quentin") UtilisateurFD <- "quentin"
  if(UtilisateurFD == "Adrien") UtilisateurFD <- "adrien"
  if(UtilisateurFD == "JB") UtilisateurFD <- "jb"
  dbDisconnect(dbD)
  #rm(dbD)
  
  if(strsplit(system('system_profiler SPNetworkDataType | grep RouterHardwareAddress',intern=T), "RouterHardwareAddress=")[[1]][2] == "04:92:26:6c:9a:d8"){
    dbD <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                                  dbname = "eaux-jura-sig-data",
                                  host = '192.168.1.100',
                                  #host = '80.11.169.205',
                                  port = 3254,
                                  user = UtilisateurFD,
                                  password = keyring::key_get("eaux-jura-sig-data")
    )
  }else{
    dbD <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                                  dbname = "eaux-jura-sig-data",
                                  #host = '192.168.1.100',
                                  host = '80.11.169.205',
                                  port = 3254,
                                  user = UtilisateurFD,
                                  password = keyring::key_get("eaux-jura-sig-data")
    )
  }
}
}

if(Type == "Chroniques"){return(dbC)}
if(Type == "Macroinvertébrés"){return(dbMI)}
if(Type == "Physico-chimie"){return(dbPC)}
if(Type == "Temps de travail"){return(dbTW)}
if(Type == "Poissons"){return(dbP)}
if(Type == "Data"){return(dbD)}

} # Fin de la fonction
