#' Chargement des bases de données
#'
#' Cette fonction permet de charger les bases de données de la fédération
#' @name BDD.ouverture
#' @param Type Type de base de données. Chroniques par défaut
#' @param motdepasse Mot de passe du portefeuille keyring, si accès côté RStudio server
#' @import keyring
#' @import RPostgreSQL
#' @import rstudioapi
#' @import tidyverse
#' @export
#' @examples
#' BDD.ouverture()
#' BDD.ouverture(Type = "Poissons")
#' dbD <- BDD.ouverture(Type = "Data")
#' dbP <- BDD.ouverture(Type = "Poissons")
#' tbl(dbD, in_schema("fd_production", "chroniques_commentaires")) %>% collect(n = Inf)

##### TODO LIST #####
#
#####################

BDD.ouverture <- function(
  Type = c("Chroniques", "Poissons", "Macroinvertébrés", "Physico-chimie", "Temps de travail", "Data"),
  motdepasse = NA_character_
  )
{
  
  ## Évaluation des choix
  Type <- match.arg(Type)

  #### Version serveur ou version client classique ####
  if(system('uname -n',intern=T) == "rstudio-server" & grepl(system('lsb_release -d',intern=T) %>% str_replace("Description:\tUbuntu ", "") %>% str_replace(" LTS", ""), "20.04.1", fixed = TRUE)) client <- "serveur"
  if(exists("client") == FALSE) client <- "machineordinaire"
  
#### Utilisateur ####
# Détection en fonction de la machine
if(system('uname -n',intern=T) == "imac27"){UtilisateurFD <- "jb"}
if(system('uname -n',intern=T) == "imac27.local"){UtilisateurFD <- "jb"}
if(system('uname -n',intern=T) == "iMacdeJBaptisteEthernet"){UtilisateurFD <- "jb"}
if(system('uname -n',intern=T) == "iMacdeJBaptisteEthernet.local"){UtilisateurFD <- "jb"}
if(system('uname -n',intern=T) == "Client_imacJB"){UtilisateurFD <- "jb"}
if(system('uname -n',intern=T) == "Client_imacJB.local"){UtilisateurFD <- "jb"}
if(system('uname -n',intern=T) == "Client_imacJB.cloud.peche-jura.com"){UtilisateurFD <- "jb"}
if(system('uname -n',intern=T) == "Client_MacBookJB"){UtilisateurFD <- "jb"}
if(system('uname -n',intern=T) == "Client_MacBookJB.local"){UtilisateurFD <- "jb"}
if(system('uname -n',intern=T) == "Client_MacBookJB.cloud.peche-jura.com"){UtilisateurFD <- "jb"}
if(system('uname -n',intern=T) == "MacBookJB.cloud.peche-jura.com"){UtilisateurFD <- "jb"}
if(system('uname -n',intern=T) == "MacBookJB.local"){UtilisateurFD <- "jb"}
if(system('uname -n',intern=T) == "MacBookJB"){UtilisateurFD <- "jb"}
if(system('uname -n',intern=T) == "MBP-de-Adrien"){UtilisateurFD <- "adrien"}
if(system('uname -n',intern=T) == "MBP-de-Adrien.local"){UtilisateurFD <- "adrien"}
if(system('uname -n',intern=T) == "MBP-de-Adrien.cloud.peche-jura.com"){UtilisateurFD <- "adrien"}
if(system('uname -n',intern=T) == "macbook-pro-de-adrien"){UtilisateurFD <- "adrien"}
if(system('uname -n',intern=T) == "macbook-pro-de-adrien.home"){UtilisateurFD <- "adrien"}
if(system('uname -n',intern=T) == "macbook-pro-de-adrien.local"){UtilisateurFD <- "adrien"}
if(system('uname -n',intern=T) == "MacBook-Pro-de-Adrien"){UtilisateurFD <- "adrien"}
if(system('uname -n',intern=T) == "MacBook-Pro-de-Adrien.local"){UtilisateurFD <- "adrien"}
if(system('uname -n',intern=T) == "MacBook-Pro-de-Adrien.home"){UtilisateurFD <- "adrien"}
if(system('uname -n',intern=T) == "Client_MBP-de-Adrien"){UtilisateurFD <- "adrien"}
if(system('uname -n',intern=T) == "Client_MBP-de-Adrien.local"){UtilisateurFD <- "adrien"}
if(system('uname -n',intern=T) == "Client_MBP-de-Adrien.cloud.peche-jura.com"){UtilisateurFD <- "adrien"}
if(system('uname -n',intern=T) == "iMac-de-Quentin"){UtilisateurFD <- "quentin"}
if(system('uname -n',intern=T) == "iMac-de-Quentin.local"){UtilisateurFD <- "quentin"}
if(system('uname -n',intern=T) == "Client_iMac-de-Quentin"){UtilisateurFD <- "quentin"}
if(system('uname -n',intern=T) == "Client_iMac-de-Quentin.local"){UtilisateurFD <- "quentin"}
if(system('uname -n',intern=T) == "postgis"){UtilisateurFD <- "automate"}

if(exists("UtilisateurFD") == FALSE){
  if(client == "serveur" & system('whoami',intern=T) == "jb") UtilisateurFD <- "jb"
  if(client == "serveur" & system('whoami',intern=T) == "adrien") UtilisateurFD <- "adrien"
  if(client == "serveur" & system('whoami',intern=T) == "ubuntu") UtilisateurFD <- "jb" # il faudrait automate dans l'absolu, mais pas possible d'actualiser des MV car automate n'est pas priopriétaire de celles-ci
  
  if(client == "serveur" & exists("UtilisateurFD") == FALSE) UtilisateurFD <- NA_character_
}

  #### Vérification présence mot de passe ####
  if(client == "serveur" & is.na(motdepasse)) stop("Pas de mot de passe portefeuille keyring fourni alors que nécessaire")
  
#### Création de la connexion ####
## Si utilisateur connu ##
if(!is.na(UtilisateurFD)){
  if(Type == "Data" & exists("dbD") == TRUE){
    if(RPostgreSQL::isPostgresqlIdCurrent(dbD) == FALSE){
      RPostgreSQL::dbDisconnect(dbD)}
  }
  if(Type == "Data"){
    
    dbD <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                                  dbname = "eaux-jura-sig-data",
                                  host = "database.eaux-jura.com",
                                  port = 3254,
                                  user = UtilisateurFD,
                                  password = ifelse(client == "machineordinaire", 
                                                    keyring::key_get("eaux-jura-sig-data", username = UtilisateurFD),
                                                    motdepasse
                                                    )
                                  )
  }
  
  if(Type == "Poissons" & exists("dbP") == TRUE){
    if(RPostgreSQL::isPostgresqlIdCurrent(dbP) == FALSE){
      RPostgreSQL::dbDisconnect(dbP)}
  }
  if(Type == "Poissons"){
    dbP <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                                  dbname = "multifish",
                                  host = "database.eaux-jura.com",
                                  port = 3254,
                                  user = UtilisateurFD,
                                  password = ifelse(client == "machineordinaire", 
                                                    keyring::key_get("multifish", username = UtilisateurFD),
                                                    motdepasse
                                                    )    
                                  )
  }
}

## Si utilisateur inconnu ##
if(is.na(UtilisateurFD)){
  UtilisateurFD <- rstudioapi::showPrompt(
    title = "Nom d'utilisateur", message = "Nom d'utilisateur", default = "" # Demande du nom d'utilisateur si celui-ci est vide après la détection automatique #
  )
  MotdepasseFD <- rstudioapi::askForPassword(prompt = "Mot de passe") # Demande du mdp si celui-ci est vide après la détection automatique #

  if(Type == "Data" & exists("dbD") == TRUE){
    if(RPostgreSQL::isPostgresqlIdCurrent(dbD) == FALSE){
      RPostgreSQL::dbDisconnect(dbD)}
  }
  if(Type == "Data"){
    
    dbD <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                                  dbname = "eaux-jura-sig-data",
                                  host = "database.eaux-jura.com",
                                  port = 3254,
                                  user = UtilisateurFD,
                                  password = MotdepasseFD
    )
  }
  
  if(Type == "Poissons" & exists("dbP") == TRUE){
    if(RPostgreSQL::isPostgresqlIdCurrent(dbP) == FALSE){
      RPostgreSQL::dbDisconnect(dbP)}
  }
  if(Type == "Poissons"){
    dbP <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                                  dbname = "multifish",
                                  host = "database.eaux-jura.com",
                                  port = 3254,
                                  user = UtilisateurFD,
                                  password = MotdepasseFD
    )
  }
  
}



#### Sortie de la connexion ####
if(Type == "Poissons"){return(dbP)}
if(Type == "Data"){return(dbD)}

} # Fin de la fonction
