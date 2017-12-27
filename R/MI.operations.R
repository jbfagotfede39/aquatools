#' Listing des opérations (MI) pour une station
#'
#' Cette fonction permet de lister les différentes opérations pour une liste donnée de stations
#' 
#' @param listeStations Dataframe contenant un colonne "Nom" avec le code de la station (RHJ)
#' @param CodeOperation Affichage du CodeOperation - \code{FALSE} (par défault)
#' @param Sortie Forme du dataframe de sortie - \code{Simple} (par défault), \code{Propre} (format diffusable, avec stations) ou \code{Complet} (tous les champs)
#' @keywords poissons
#' @import dplyr RSQLite DBI lubridate
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
  db <- BDD.ouverture(Type = "Macroinvertébrés")

  #### Récupération des données ####
  Operations <- tbl(db,"Operations") %>% collect(n = Inf)
  Captures <- tbl(db,"Captures") %>% collect(n = Inf)
  Prelevements <- tbl(db,"Prelevements") %>% collect(n = Inf)
  Habitats <- tbl(db,"Habitats") %>% collect(n = Inf)
  HabitatsReference <- tbl(db,"HabitatsReference") %>% collect(n = Inf)
  EspecesReference <- tbl(db,"EspecesReference") %>% collect(n = Inf)
  GenresReference <- tbl(db,"GenresReference") %>% collect(n = Inf)
  SousFamillesReference <- tbl(db,"SousFamillesReference") %>% collect(n = Inf)
  FamillesReference <- tbl(db,"FamillesReference") %>% collect(n = Inf)
  OrdresReference <- tbl(db,"OrdresReference") %>% collect(n = Inf)
  
  Habitats$Recouvrement <- as.numeric(sub(",", ".", Habitats$Recouvrement))
  
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