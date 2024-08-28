#' Chargement des bases de données
#'
#' Cette fonction permet de charger les bases de données de la fédération
#' @name BDD.ouverture
#' @param Type Type de base de données. Chroniques par défaut
#' @param utilisateur Nom d'utilisateur, si on souhaite en spécifier un
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

BDD.ouverture <- function(
  Type = c("Chroniques", "Poissons", "Macroinvertébrés", "Physico-chimie", "Temps de travail", "Data"),
  utilisateur = NA_character_,
  motdepasse = NA_character_
  )
{
  
  ## Évaluation des choix
  Type <- match.arg(Type)

  #### Version serveur ou version client classique ####
  ### r-ftp - VM172 - #201 ###
  if(system('uname -n',intern=T) == "r-ftp"){
    if(grepl(system('lsb_release -d',intern=T) %>% 
             str_replace("Description:\tUbuntu ", "") %>% 
             str_replace(" LTS", ""), "22.04.2", fixed = TRUE)){
      if(is.na(utilisateur) | utilisateur == "automate") client <- "serveur"
      if(utilisateur == "appshiny") client <- "shinyserver"
    }
  }

  ### RStudio server - VM103 - #61 ###
  if(system('uname -n',intern=T) == "rstudio-server"){
    if(grepl(system('lsb_release -d',intern=T) %>% 
            str_replace("Description:\tUbuntu ", "") %>% 
            str_replace(" LTS", ""), "20.04.1", fixed = TRUE)){
      client <- "serveur"}
  }
  ### Shiny server - VM100 - #170 ###
  if(system('uname -n',intern=T) == "postgis"){
    if(grepl(system('lsb_release -d',intern=T) %>% 
             str_replace("Description:\tUbuntu ", "") %>% 
             str_replace(" LTS", ""), "18.04.2", fixed = TRUE)){
      client <- "shinyserver"}
  }
  ### Client classique ###
  if(exists("client") == FALSE) client <- "machineordinaire"
  
#### Utilisateur ####
# Détection en fonction de la machine
  if(is.na(utilisateur)){
    if(system('uname -n',intern=T) == "imac27"){utilisateur <- "jb"}
    if(system('uname -n',intern=T) == "imac27.local"){utilisateur <- "jb"}
    if(system('uname -n',intern=T) == "iMacdeJBaptisteEthernet"){utilisateur <- "jb"}
    if(system('uname -n',intern=T) == "iMacdeJBaptisteEthernet.local"){utilisateur <- "jb"}
    if(system('uname -n',intern=T) == "Client_imacJB"){utilisateur <- "jb"}
    if(system('uname -n',intern=T) == "Client_imacJB.local"){utilisateur <- "jb"}
    if(system('uname -n',intern=T) == "Client_imacJB.cloud.peche-jura.com"){utilisateur <- "jb"}
    if(system('uname -n',intern=T) == "Client_MacBookJB"){utilisateur <- "jb"}
    if(system('uname -n',intern=T) == "Client_MacBookJB.local"){utilisateur <- "jb"}
    if(system('uname -n',intern=T) == "Client_MacBookJB.cloud.peche-jura.com"){utilisateur <- "jb"}
    if(system('uname -n',intern=T) == "MacBookJB.cloud.peche-jura.com"){utilisateur <- "jb"}
    if(system('uname -n',intern=T) == "MacBookJB.local"){utilisateur <- "jb"}
    if(system('uname -n',intern=T) == "MacBookJB"){utilisateur <- "jb"}
    if(system('uname -n',intern=T) == "MBPdeJeBaptiste"){utilisateur <- "jb"}
    if(system('uname -n',intern=T) == "MBPdeJeBaptiste.cloud.peche-jura.com"){utilisateur <- "jb"}
    if(system('uname -n',intern=T) == "MacBook-Pro-de-Jean-Baptiste.local"){utilisateur <- "jb"}
    if(system('uname -n',intern=T) == "MBP-de-Adrien"){utilisateur <- "adrien"}
    if(system('uname -n',intern=T) == "MBP-de-Adrien.local"){utilisateur <- "adrien"}
    if(system('uname -n',intern=T) == "MBP-de-Adrien.cloud.peche-jura.com"){utilisateur <- "adrien"}
    if(system('uname -n',intern=T) == "macbook-pro-de-adrien"){utilisateur <- "adrien"}
    if(system('uname -n',intern=T) == "macbook-pro-de-adrien.home"){utilisateur <- "adrien"}
    if(system('uname -n',intern=T) == "macbook-pro-de-adrien.local"){utilisateur <- "adrien"}
    if(system('uname -n',intern=T) == "MacBook-Pro-de-Adrien"){utilisateur <- "adrien"}
    if(system('uname -n',intern=T) == "MacBook-Pro-de-Adrien.local"){utilisateur <- "adrien"}
    if(system('uname -n',intern=T) == "MacBook-Pro-de-Adrien.home"){utilisateur <- "adrien"}
    if(system('uname -n',intern=T) == "Client_MBP-de-Adrien"){utilisateur <- "adrien"}
    if(system('uname -n',intern=T) == "Client_MBP-de-Adrien.local"){utilisateur <- "adrien"}
    if(system('uname -n',intern=T) == "Client_MBP-de-Adrien.cloud.peche-jura.com"){utilisateur <- "adrien"}
    if(system('uname -n',intern=T) == "Air-de-Malide"){utilisateur <- "malide"}
    if(system('uname -n',intern=T) == "Air-de-Malide.lan"){utilisateur <- "malide"}
    if(system('uname -n',intern=T) == "postgis" & client == "serveur"){utilisateur <- "automate"}
    if(system('uname -n',intern=T) == "postgis" & client == "shinyserver" & system('whoami', intern=T) == "ubuntu") utilisateur <- "appshiny"
    if(system('uname -n',intern=T) == "r-ftp" & client == "serveur"){utilisateur <- "automate"}
  }
  
if(is.na(utilisateur) == TRUE){
  if(client == "serveur" & system('whoami',intern=T) == "jb") utilisateur <- "jb"
  if(client == "serveur" & system('whoami',intern=T) == "adrien") utilisateur <- "adrien"
  if(client == "serveur" & system('whoami',intern=T) == "malide") utilisateur <- "malide"
  if(client == "serveur" & system('whoami',intern=T) == "ubuntu") utilisateur <- "jb" # il faudrait automate dans l'absolu, mais pas possible d'actualiser des MV car automate n'est pas propriétaire de celles-ci
  }

#### Création de la connexion ####
## Si utilisateur connu ##
if(!is.na(utilisateur)){
  if(Type == "Data" & exists("dbD") == TRUE){
    if(RPostgreSQL::isPostgresqlIdCurrent(dbD) == FALSE){
      RPostgreSQL::dbDisconnect(dbD)}
  }
  if(Type == "Data"){
    if(is.na(motdepasse)){
      if(client == "machineordinaire") motdepasse <- keyring::key_get("eaux-jura-sig-data", username = utilisateur)
      if(client == "serveur") motdepasse <- NULL # Le mot de passe sera recherché dans le .pgpass
      if(client == "shinyserver"){
        fileName <- '/srv/shiny-server/conf.txt'
        motdepasse <- readChar(fileName, file.info(fileName)$size-1)
      }
    }
    
    dbD <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                                  dbname = "eaux-jura-sig-data",
                                  host = "database.eaux-jura.com",
                                  port = 3254,
                                  user = utilisateur,
                                  password = motdepasse
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
                                  user = utilisateur,
                                  password = ifelse(client == "machineordinaire", 
                                                    keyring::key_get("multifish", username = utilisateur),
                                                    motdepasse
                                                    )    
                                  )
  }
}

## Si utilisateur inconnu ##
if(is.na(utilisateur)){
  utilisateur <- rstudioapi::showPrompt(
    title = "Nom d'utilisateur", message = "Nom d'utilisateur", default = "" # Demande du nom d'utilisateur si celui-ci est vide après la détection automatique #
  )
  if(is.na(motdepasse)){motdepasse <- rstudioapi::askForPassword(prompt = "Mot de passe")} # Demande du mdp si celui-ci est vide après la détection automatique #

  if(Type == "Data" & exists("dbD") == TRUE){
    if(RPostgreSQL::isPostgresqlIdCurrent(dbD) == FALSE){
      RPostgreSQL::dbDisconnect(dbD)}
  }
  if(Type == "Data"){
    
    dbD <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                                  dbname = "eaux-jura-sig-data",
                                  host = "database.eaux-jura.com",
                                  port = 3254,
                                  user = utilisateur,
                                  password = motdepasse
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
                                  user = utilisateur,
                                  password = motdepasse
    )
  }
  
}



#### Sortie de la connexion ####
if(Type == "Poissons"){return(dbP)}
if(Type == "Data"){return(dbD)}

} # Fin de la fonction
