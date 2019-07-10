#' Extraction de données brutes de chronique
#'
#' Permet d'extraire les données de chronique d'une station
#' @name chronique.mesures
#' @keywords chronique
#' @import lubridate 
#' @import stringr
#' @import tidyverse
#' @param CodeStation Code RHJ de la station
#' @param Type Type de chronique recherchée : Thermie (par défaut), piézométrie, Hydrologie, O2, pluviométrie ou toutes les données
#' @param annee Année biologique recherchée si spécifiée, sinon ensemble de la chronique (par défaut)
#' @param Valide Validité des données (\code(TRUE) par défaut)
#' @export
#' @examples
#' chronique.mesures("HER0-6", "Thermie") 
#' chronique.mesures("DRO6-8", "Thermie", "2013")
#' chronique.mesures("GCLzhaval", "Piézométrie", "2015", Valide = F)
#' chronique.mesures("GCLzhaval", "Tout", "2015", Valide = F)

chronique.mesures <- function(  
  CodeStation = character(0),
  Type = c("Thermie", "Piézométrie", "Tout", "Hydrologie", "O2", "Pluviométrie"),
  annee = numeric(0),
  Valide = T)
  
{
  
#### Évaluation des choix ####
Type <- match.arg(Type)
if(nchar(as.character(CodeStation)) == 0) 
  stop("Attention : pas de station spécifiée")

##### Connexion à la BDD #####
dbD <- BDD.ouverture("Data")

##### Collecte des données #####
if(Valide == F & Type != "Tout"){
  Mesures <- 
    tbl(dbD, in_schema("fd_production", "chroniques_mesures")) %>% 
    filter(chmes_typemesure == Type) %>% 
    filter(chmes_coderhj == as.character(CodeStation)) %>% 
    #filter(chmes_validation == "Validé") %>% 
    collect()
}

if(Valide == T & Type != "Tout"){
  Mesures <- tbl(dbD, in_schema("fd_production", "chroniques_mesures")) %>% 
    filter(chmes_typemesure == Type) %>% 
    filter(chmes_coderhj == as.character(CodeStation)) %>% 
    filter(chmes_validation == "Validé") %>% 
    collect()
}

if(Valide == F & Type == "Tout"){
  Mesures <- 
    tbl(dbD, in_schema("fd_production", "chroniques_mesures")) %>% 
    #filter(chmes_typemesure == Type) %>% 
    filter(chmes_coderhj == as.character(CodeStation)) %>% 
    #filter(chmes_validation == "Validé") %>% 
    collect()
}

if(Valide == T & Type == "Tout"){
  Mesures <- tbl(dbD, in_schema("fd_production", "chroniques_mesures")) %>% 
    #filter(chmes_typemesure == Type) %>% 
    filter(chmes_coderhj == as.character(CodeStation)) %>% 
    filter(chmes_validation == "Validé") %>% 
    collect()
}

## Fermeture de la BDD ##
DBI::dbDisconnect(dbD)

##### Filtrage en fonction de la période #####
if(length(as.character(annee)) != 0){
  Mesures <-
    Mesures %>% 
    formatage.annee.biologique() %>% 
    filter(chmes_anneebiol == as.character(annee))
}

##### Mise en forme #####
Mesures <- Mesures %>% arrange(chmes_date, chmes_heure)

##### Sortie #####
return(Mesures)

} # Fin de la fonction