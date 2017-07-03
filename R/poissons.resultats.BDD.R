#' Extraction des données des résultats de Maxifish
#'
#' Récupère l'ensemble des données de résultats de Maxifish dans un dataframe
#' @keywords donnees
#' @import DBI dplyr RSQLite
#' @export
#' @examples
#' poissons.resultats.BDD()

poissons.resultats.BDD <- function(){
  
  #library("RSQLite");library("dplyr")
  
  ## Ouverture de la BDD ##
  db <- BDD.ouverture(Type = "Poissons")
  
  ##### Récupération des données #####
  Resultats <- tbl(db,"Resultats") %>% collect(n = Inf)
  Operations <- tbl(db,"Operations") %>% collect(n = Inf)
  Inventaires <- tbl(db,"Inventaires") %>% collect(n = Inf)
  Stations <- tbl(db,"Stations") %>% collect(n = Inf)
  Ecosystemes <- tbl(db,"Ecosystemes") %>% collect(n = Inf)
  Communes <- tbl(db,"Communes") %>% collect(n = Inf)
  
  ##### Synthèse des données #####
  Inventaires <- merge(Inventaires, Stations, by = c("CodeStation"))
  Operations <- merge(Operations, Inventaires, by = c("CodeInventaire"))
  Resultats <- merge(Resultats, Operations, by = c("Codeoperation"))
  
  ##### Renommage des colonnes observations #####
  Ecosystemes <- dplyr::rename(Ecosystemes, ObservationsEcosysteme = Observations)
  
  ##### Remplacement des codes communes et écosystèmes #####
  Resultats <- merge(Resultats, Ecosystemes, by = c("Codeecosysteme"))
  Communes <- select(Communes, CodeCommune, Commune)
  Resultats <- merge(Resultats, Communes, by = c("CodeCommune"))
  
  ##### Simplification #####
#  Resultats <- 
#    Resultats %>%
#    select(Nom, DateDebut, Codeespece, TailleMinimum, TailleMaximum, Nombre, Poids)

  return(Resultats)
} # Fin de la fonction

# Resultats <- poissons.resultats() pour avoir les données en utilisant la fonction