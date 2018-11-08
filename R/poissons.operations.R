#' Listing des opérations pour une station
#'
#' Cette fonction permet de lister les différentes opérations pour une liste donnée de stations
#' @name poissons.operations
#' @param ListeStations Dataframe contenant un colonne "Nom" avec le code de la station (RHJ)
#' @param CodeOperation Affichage du CodeOperation - \code{FALSE} (par défault)
#' @param Sortie Forme du dataframe de sortie - \code{Simple} (par défault), \code{Propre} (format diffusable, avec stations) ou \code{Complet} (tous les champs)
#' @keywords poissons
#' @import dplyr RSQLite DBI lubridate
#' @export
#' @examples
#' poissons.operations()
#' poissons.operations(CodeOperation = TRUE)
#' poissons.operations(data.frame(Nom = "SOR10-2"))
#' poissons.operations(data.frame(Nom = "SOR10-2"), CodeOperation = TRUE)
#' poissons.operations(listeStations, Sortie = Propre)
#' poissons.operations(data.frame(Nom = "SOR10-2"), Sortie = Complet)

##### TODO LIST #####
# 
#####################

poissons.operations <- function(
  ListeStations = data.frame(Nom = character(0)),
  CodeOperation = FALSE,
  Sortie = c("Simple","Propre","Complet")
)
{

  ## Évaluation des choix
  Sortie <- match.arg(Sortie)
  
  ## Ouverture de la BDD ##
  ## Connexion à la BDD
  dbP <- BDD.ouverture(Type = "Poissons")

  ## Récupération des données ##
  Operations <- tbl(dbP,"operations") %>% collect(n = Inf)
  Inventaires <- tbl(dbP,"inventaires") %>% collect(n = Inf)
  Stations <- tbl(dbP,"stations") %>% collect(n = Inf)
  
  ## Fermeture de la BDD ##
  DBI::dbDisconnect(dbP)
  
  ## Synthèse des données ##
  Inventaires <- merge(Inventaires, Stations, by = c("codestation"))
  Operations <- merge(Operations, Inventaires, by = c("codeinventaire"))
  
  ## Format de Date ##
  Operations$DateDebut.x <- ymd(Operations$datedebut.x)
  Operations$DateDebut.x <- format(Operations$datedebut.x, "%Y-%m-%d")
  
  ## Extraction des stations ##
  # Test si le nom existe bien, sinon message d'erreur et arrêt de la fonction #
  
  if(dim(Operations %>% filter(nom %in% ListeStations$Nom))[1] == 0 & dim(ListeStations)[1] != 0)
    warning("Attention : station(s) absente(s) des opérations de la base de données")
  
  ## Simplification ##
  # Travail sur une seule station
  if(dim(ListeStations)[1] != 0 & CodeOperation == F & Sortie == "Simple"){
    Operations <- 
      Operations %>%
    select(nom, datedebut.x) %>% 
    filter(nom %in% ListeStations$Nom) %>% 
    dplyr::rename(Station = nom, Date = datedebut.x) %>% 
    arrange(Station, Date)}
  
  if(dim(ListeStations)[1] != 0 & CodeOperation == F & Sortie == "Propre"){
    Operations <- 
      Operations %>%
      filter(nom %in% ListeStations$Nom) %>% 
      select(nom, limiteamont, datedebut.x, modeechantillonnage, xlambert, ylambert, typelambert) %>% 
      dplyr::rename(Station = nom, Date = datedebut.x, X = xlambert, Y = ylambert, TypeCoord = typelambert) %>% 
      arrange(Station, Date)}
  
  if(dim(ListeStations)[1] != 0 & CodeOperation == F & Sortie == "Complet"){
    Operations <- 
      Operations %>%
      filter(nom %in% ListeStations$Nom) %>% 
      dplyr::rename(CodeOperation = codeoperation, Station = nom, Date = datedebut.x, X = xlambert, Y = ylambert, TypeCoord = typelambert) %>% 
      arrange(Station, Date)}  
  
  if(dim(ListeStations)[1] != 0 & CodeOperation == T & Sortie == "Simple"){
    Operations <- 
      Operations %>%
      select(nom, datedebut.x, codeoperation) %>% 
      filter(Nom %in% ListeStations$Nom) %>% 
      dplyr::rename(CodeOperation = codeoperation, Station = nom, Date = datedebut.x) %>% 
      arrange(Station, Date)}
  
  if(dim(ListeStations)[1] != 0 & CodeOperation == T & Sortie == "Propre"){
    Operations <- 
      Operations %>%
      filter(nom %in% ListeStations$Nom) %>% 
      select(codeoperation, nom, limiteamont, datedebut.x, modeechantillonnage, xlambert, ylambert, typelambert) %>% 
      dplyr::rename(CodeOperation = codeoperation, Station = nom, Date = datedebut.x, X = xlambert, Y = ylambert, TypeCoord = typelambert) %>% 
      arrange(Station, Date)}
  
  if(dim(ListeStations)[1] != 0 & CodeOperation == T & Sortie == "Complet"){
    Operations <- 
      Operations %>%
      filter(nom %in% ListeStations$Nom) %>% 
      dplyr::rename(CodeOperation = codeoperation, Station = nom, Date = datedebut.x, X = xlambert, Y = ylambert, TypeCoord = typelambert) %>% 
      arrange(Station, Date)}
  
  # Travail sur l'ensemble des stations
  if(dim(ListeStations)[1] == 0 & CodeOperation == F & Sortie == "Simple"){
    Operations <- 
      Operations %>%
      select(nom, datedebut.x) %>% 
      dplyr::rename(Station = nom, Date = datedebut.x) %>% 
      arrange(Station, Date)}
  
  if(dim(ListeStations)[1] == 0 & CodeOperation == F & Sortie == "Propre"){
    Operations <- 
      Operations %>%
      select(nom, limiteamont, datedebut.x, modeechantillonnage, xlambert, ylambert, typelambert) %>% 
      dplyr::rename(Station = nom, Date = datedebut.x, X = xlambert, Y = ylambert, TypeCoord = typelambert) %>% 
      arrange(Station, Date)}
  
  if(dim(ListeStations)[1] == 0 & CodeOperation == F & Sortie == "Complet"){
    Operations <- 
      Operations %>%
      dplyr::rename(CodeOperation = codeoperation, Station = nom, Date = datedebut.x, X = xlambert, Y = ylambert, TypeCoord = typelambert) %>% 
      arrange(Station, Date)}
  
  if(dim(ListeStations)[1] == 0 & CodeOperation == T & Sortie == "Simple"){
    Operations <- 
      Operations %>%
      select(nom, datedebut.x, codeoperation) %>% 
      dplyr::rename(CodeOperation = codeoperation, Station = nom, Date = datedebut.x) %>% 
      arrange(Station, Date)}
  
  if(dim(ListeStations)[1] == 0 & CodeOperation == T & Sortie == "Propre"){
    Operations <- 
      Operations %>%
      select(codeoperation, nom, limiteamont, datedebut.x, modeechantillonnage, xlambert, ylambert, typelambert) %>% 
      dplyr::rename(CodeOperation = codeoperation, Station = nom, Date = datedebut.x, X = xlambert, Y = ylambert, TypeCoord = typelambert) %>% 
      arrange(Station, Date)}
  
  if(dim(ListeStations)[1] == 0 & CodeOperation == T & Sortie == "Complet"){
    Operations <- 
      Operations %>%
      dplyr::rename(CodeOperation = codeoperation, Station = nom, Date = datedebut.x, X = xlambert, Y = ylambert, TypeCoord = typelambert) %>% 
      arrange(Station, Date)}

  return(Operations)
  
} # Fin de la fonction