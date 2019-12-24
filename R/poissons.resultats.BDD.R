#' Extraction des données des résultats de Maxifish
#'
#' Récupère l'ensemble des données de résultats de Maxifish dans un dataframe
#' @name poissons.resultats.BDD
#' @keywords donnees
#' @import DBI
#' @import dplyr 
#' @import RSQLite
#' @param periode Permet de limiter la durée des données traitées : 10ans (par défaut), 20ans, Complet , 4campagnes
#' @export
#' @examples
#' poissons.resultats.BDD()

poissons.resultats.BDD <- function(
  periode = c("10ans","20ans","Complet","4campagnes")
  )
  
  {
  
  ## Évaluation des choix ##
  periode <- match.arg(periode)
  
  ## Ouverture de la BDD ##
  dbP <- BDD.ouverture(Type = "Poissons")
  
  ##### Récupération des données #####
  Resultats <- tbl(dbP,"resultats") %>% collect(n = Inf)
  Operations <- tbl(dbP,"operations") %>% collect(n = Inf)
  Inventaires <- tbl(dbP,"inventaires") %>% collect(n = Inf)
  Stations <- tbl(dbP,"stations") %>% collect(n = Inf)
  Ecosystemes <- tbl(dbP,"ecosystemes") %>% collect(n = Inf)
  Communes <- tbl(dbP,"communes") %>% collect(n = Inf)
  
  ## Fermeture de la BDD ##
  DBI::dbDisconnect(dbP)
  
  ##### Synthèse des données #####
  Inventaires <- left_join(Inventaires, Stations, by = c("codestation"))
  Operations <- left_join(Operations, Inventaires, by = c("codeinventaire"))
  Resultats <- left_join(Resultats, Operations, by = c("codeoperation"))
  
  ##### Renommage des colonnes observations #####
  Ecosystemes <- dplyr::rename(Ecosystemes, ObservationsEcosysteme = observations)
  
  ##### Remplacement des codes communes et écosystèmes #####
  Resultats <- left_join(Resultats, Ecosystemes, by = c("codeecosysteme.x" = "codeecosysteme"))
  Communes <- Communes %>% select(codecommune, commune)
  Resultats <- left_join(Resultats, Communes, by = c("codecommune"))
  
  ##### Limitation temporelle des résultats #####
  if(periode == "10ans") limitetemporelle <- now()-years(10)
  if(periode == "20ans") limitetemporelle <- now()-years(20)
  if(periode == "Complet") limitetemporelle <- now()-years(100)
  if(periode == "4campagnes") limitetemporelle <- Resultats %>% distinct(datedebut.x) %>% arrange(desc(datedebut.x)) %>% pull() %>% nth(4)
  Resultats <- Resultats %>% filter(datedebut.x >= limitetemporelle)
  
  ##### Simplification #####
#  Resultats <- 
#    Resultats %>%
#    select(Nom, DateDebut, Codeespece, TailleMinimum, TailleMaximum, Nombre, Poids)

  return(Resultats)
} # Fin de la fonction

# Resultats <- poissons.resultats() pour avoir les données en utilisant la fonction