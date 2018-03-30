#' Extraction de données brutes de chronique
#'
#' Permet d'extraire les données de chronique d'une station
#' @keywords chronique
#' @import DBI
#' @import dplyr
#' @import lubridate 
#' @import RSQLite 
#' @import stringr
#' @export
#' @examples
#' chronique.mesures("HER0-6", "Thermie") 
#' chronique.mesures("DRO6-8", "Thermie", "2013")
#' chronique.mesures("GCLzhaval", "Piézométrie", "2015", Valide = F)

chronique.mesures <- function(  
  CodeStation = character(0),
  Type = c("Thermie", "Piézométrie", "Hydrologie", "O2", "Pluviométrie"),
  annee = numeric(0),
  Valide = T)
{
  
  # library("DBI");library("dplyr");library("lubridate");library("RSQLite");library("stringr")
  # CodeStation ="GCLzhaval";Type="Piézométrie";annee="2015"
  
#### Évaluation des choix ####
Type <- match.arg(Type)
if(nchar(as.character(CodeStation)) == 0) 
  stop("Attention : pas de station spécifiée")

##### Connexion à la BDD #####
db <- BDD.ouverture(Type = "Chroniques")

##### Collecte des données #####
if(Valide == F){
  Mesures <- 
    tbl(db,"Mesures") %>% 
    filter(TypeMesure == Type) %>% 
    filter(CodeRDT == as.character(CodeStation)) %>% 
    #filter(Validation == "Validé") %>% 
    collect()
}

if(Valide == T){
  Mesures <- tbl(db,"Mesures") %>% 
    filter(TypeMesure == Type) %>% 
    filter(CodeRDT == as.character(CodeStation)) %>% 
    filter(Validation == "Validé") %>% 
    collect()
}

##### Filtrage en fonction de la période #####
if(length(as.character(annee)) != 0){
  Mesures <-
    Mesures %>% 
    formatage.annee.biologique() %>% 
    filter(AnneeBiol == as.character(annee))
}

##### Mise en forme #####
Mesures <- Mesures %>% arrange(Date, Heure)

##### Sortie #####
return(Mesures)

} # Fin de la fonction