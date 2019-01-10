#' Listing des opérations (MI) pour une station
#'
#' Cette fonction permet de lister les différentes opérations pour une liste donnée de stations
#' @name MI.operations
#' @param listeStations Dataframe contenant un colonne "Nom" avec le code de la station (RHJ)
#' @param CodeOperation Affichage du CodeOperation - \code{FALSE} (par défault)
#' @param Sortie Forme du dataframe de sortie - \code{Simple} (par défault), \code{Propre} (format diffusable, avec stations) ou \code{Complet} (tous les champs)
#' @keywords poissons
#' @import dplyr
#' @import RSQLite
#' @import DBI
#' @import lubridate
#' @export
#' @examples
#' MI.operations()
#' MI.operations(CodeOperation = TRUE)
#' MI.operations(data.frame(Nom = "BCH5-7"))
#' MI.operations(data.frame(Nom = "BCH5-7"), CodeOperation = TRUE)
#' MI.operations(listeStations, Sortie = Propre)
#' MI.operations(data.frame(Nom = "BCH5-7"), Sortie = Complet)

##### TODO LIST #####
# 
#####################

MI.operations <- function(
  listeStations = data.frame(Nom = character(0)),
  CodeOperation = F,
  Sortie = c("Simple","Propre","Complet")
)
{

  ## Évaluation des choix
  Sortie <- match.arg(Sortie)
  
  #### Ouverture de la BDD ####
  ## Connexion à la BDD
  dbMI <- BDD.ouverture(Type = "Macroinvertébrés")

  #### Récupération des données ####
  Operations <- tbl(dbMI,"Operations") %>% collect(n = Inf)
  Captures <- tbl(dbMI,"Captures") %>% collect(n = Inf)
  Prelevements <- tbl(dbMI,"Prelevements") %>% collect(n = Inf)
  Habitats <- tbl(dbMI,"Habitats") %>% collect(n = Inf)
  HabitatsReference <- tbl(dbMI,"HabitatsReference") %>% collect(n = Inf)
  EspecesReference <- tbl(dbMI,"EspecesReference") %>% collect(n = Inf)
  GenresReference <- tbl(dbMI,"GenresReference") %>% collect(n = Inf)
  SousFamillesReference <- tbl(dbMI,"SousFamillesReference") %>% collect(n = Inf)
  FamillesReference <- tbl(dbMI,"FamillesReference") %>% collect(n = Inf)
  OrdresReference <- tbl(dbMI,"OrdresReference") %>% collect(n = Inf)
  
  Habitats$Recouvrement <- as.numeric(sub(",", ".", Habitats$Recouvrement))
  
  # ## Fermeture de la BDD ##
  # DBI::dbDisconnect(dbMI)
  
  ## Format de Date ##
  Operations$Date <- ymd(Operations$Date)
  
  ## Simplification ##
  
  #### Travail sur l'ensemble des stations ####
  if(dim(listeStations)[1] == 0 & CodeOperation == F & Sortie == "Simple"){
    Operations <- 
      Operations %>%
      select(CodeRHJ, Date) %>% 
      dplyr::rename(Station = CodeRHJ) %>% 
      arrange(Station, Date)}
  
  if(dim(listeStations)[1] == 0 & CodeOperation == F & Sortie == "Propre"){
    Operations <- 
      Operations %>%
      select(CodeRHJ, Date, X, Y, TypeCoord) %>% 
      dplyr::rename(Station = CodeRHJ) %>% 
      arrange(Station, Date)}
  
  if(dim(listeStations)[1] == 0 & CodeOperation == F & Sortie == "Complet"){
    Operations <- 
      Operations %>%
      dplyr::rename(Station = CodeRHJ) %>% 
      arrange(Station, Date)}
  
  if(dim(listeStations)[1] == 0 & CodeOperation == T & Sortie == "Simple"){
    Operations <- 
      Operations %>%
      select(OperationID, CodeRHJ, Date) %>% 
      dplyr::rename(Station = CodeRHJ) %>% 
      arrange(Station, Date)}
  
  if(dim(listeStations)[1] == 0 & CodeOperation == T & Sortie == "Propre"){
    Operations <- 
      Operations %>%
      select(OperationID, CodeRHJ, Date, X, Y, TypeCoord) %>% 
      dplyr::rename(Station = CodeRHJ) %>% 
      arrange(Station, Date)}
  
  if(dim(listeStations)[1] == 0 & CodeOperation == T & Sortie == "Complet"){
    Operations <- 
      Operations %>%
      dplyr::rename(Station = CodeRHJ) %>% 
      arrange(Station, Date)}
  
  #### Travail sur une seule station ####
  # Test si le nom existe bien, sinon message d'erreur et arrêt de la fonction #
  if(dim(listeStations)[1] != 0) {
  if(dim(Operations %>% filter(Operations$CodeRHJ %in% listeStations$Nom))[1] == 0)
    warning("Attention : station(s) absente(s) des opérations de la base de données")}
  
  if(dim(listeStations)[1] != 0 & CodeOperation == F & Sortie == "Simple"){
    Operations <- 
      Operations %>%
      filter(CodeRHJ %in% listeStations$Nom) %>% 
      select(CodeRHJ, Date) %>% 
      dplyr::rename(Station = CodeRHJ) %>% 
      arrange(Station, Date)}
  
  if(dim(listeStations)[1] != 0 & CodeOperation == F & Sortie == "Propre"){
    Operations <- 
      Operations %>%
      filter(CodeRHJ %in% listeStations$Nom) %>% 
      select(CodeRHJ, Date, X, Y, TypeCoord) %>% 
      dplyr::rename(Station = CodeRHJ) %>% 
      arrange(Station, Date)}
  
  if(dim(listeStations)[1] != 0 & CodeOperation == F & Sortie == "Complet"){
    Operations <- 
      Operations %>%
      filter(CodeRHJ %in% listeStations$Nom) %>% 
      dplyr::rename(Station = CodeRHJ) %>% 
      arrange(Station, Date)}  
  
  if(dim(listeStations)[1] != 0 & CodeOperation == T & Sortie == "Simple"){
    Operations <- 
      Operations %>%
      select(Nom, Date, OperationID) %>% 
      filter(CodeRHJ %in% listeStations$Nom) %>% 
      dplyr::rename(Station = CodeRHJ) %>% 
      arrange(Station, Date)}
  
  if(dim(listeStations)[1] != 0 & CodeOperation == T & Sortie == "Propre"){
    Operations <- 
      Operations %>%
      filter(CodeRHJ %in% listeStations$Nom) %>% 
      select(OperationID, CodeRHJ, Date, X, Y, TypeCoord) %>% 
      dplyr::rename(Station = CodeRHJ) %>% 
      arrange(Station, Date)}
  
  if(dim(listeStations)[1] != 0 & CodeOperation == T & Sortie == "Complet"){
    Operations <- 
      Operations %>%
      filter(CodeRHJ %in% listeStations$Nom) %>% 
      dplyr::rename(Station = CodeRHJ) %>% 
      arrange(Station, Date)}

  return(Operations)
  
} # Fin de la fonction