#' Listing des opérations pour une station
#'
#' Cette fonction permet de lister les différentes opérations pour une liste donnée de stations
#' 
#' @param ListeStations Dataframe contenant un colonne "Nom" avec le code de la station (RHJ)
#' @param CodeOperation Affichage du CodeOperation - \code{FALSE} (par défault)
#' @param Sortie Forme du dataframe de sortie - \code{Simple} (par défault), \code{Propre} (format diffusable, avec stations) ou \code{Complet} (tous les champs)
#' @keywords poissons
#' @import dplyr RSQLite DBI lubridate
#' @export
#' @examples
#' poissons.operations()
#' poissons.operations(CodeOperation = TRUE)
#' poissons.operations("SOR10-2")
#' poissons.operations("SOR10-2", CodeOperation = TRUE)
#' poissons.operations("SOR10-2", Sortie = Propre)
#' poissons.operations("SOR10-2", Sortie = Complet)

##### TODO LIST #####
# 
#####################

poissons.operations <- function(
  ListeStations,
  CodeOperation = FALSE,
  Sortie = c("Simple","Propre","Complet")
)
{

  ## Évaluation des choix
  Sortie <- match.arg(Sortie)
  
  ## Ouverture de la BDD ##
  ## Connexion à la BDD
  db <- BDD.ouverture(Type = "Poissons")

  ## Récupération des données ##
  Operations <- tbl(db,"Operations") %>% collect(n = Inf)
  Inventaires <- tbl(db,"Inventaires") %>% collect(n = Inf)
  Stations <- tbl(db,"Stations") %>% collect(n = Inf)
  
  ## Synthèse des données ##
  Inventaires <- merge(Inventaires, Stations, by = c("CodeStation"))
  Operations <- merge(Operations, Inventaires, by = c("CodeInventaire"))
  
  ## Format de Date ##
  Operations$DateDebut.x <- ymd_hms(Operations$DateDebut.x)
  Operations$DateDebut.x <- format(Operations$DateDebut.x, "%Y-%m-%d")
  
  ## Extraction des stations ##
  # Test si le nom existe bien, sinon message d'erreur et arrêt de la fonction #
  
  if(dim(Operations %>% filter(Nom %in% ListeStations$Nom))[1] == 0 & dim(ListeStations)[1] != 0)
    warning("Attention : station(s) absente(s) des opérations de la base de données")
  
  ## Simplification ##
  # Travail sur une seule station
  if(dim(ListeStations)[1] != 0 & CodeOperation == F & Sortie == "Simple"){
    Operations <- 
      Operations %>%
    select(Nom, DateDebut.x) %>% 
    filter(Nom %in% ListeStations$Nom) %>% 
    dplyr::rename(Station = Nom, Date = DateDebut.x) %>% 
    arrange(Station, Date)}
  
  if(dim(ListeStations)[1] != 0 & CodeOperation == F & Sortie == "Propre"){
    Operations <- 
      Operations %>%
      filter(Nom %in% ListeStations$Nom) %>% 
      select(Nom, Limiteamont, DateDebut.x, Modeechantillonnage, XLambert, YLambert, TypeLambert) %>% 
      dplyr::rename(Station = Nom, Date = DateDebut.x, X = XLambert, Y = YLambert, TypeCoord = TypeLambert) %>% 
      arrange(Station, Date)}
  
  if(dim(ListeStations)[1] != 0 & CodeOperation == F & Sortie == "Complet"){
    Operations <- 
      Operations %>%
      filter(Nom %in% ListeStations$Nom) %>% 
      dplyr::rename(CodeOperation = Codeoperation, Station = Nom, Date = DateDebut.x, X = XLambert, Y = YLambert, TypeCoord = TypeLambert) %>% 
      arrange(Station, Date)}  
  
  if(dim(ListeStations)[1] != 0 & CodeOperation == T & Sortie == "Simple"){
    Operations <- 
      Operations %>%
      select(Nom, DateDebut.x, Codeoperation) %>% 
      filter(Nom %in% ListeStations$Nom) %>% 
      dplyr::rename(CodeOperation = Codeoperation, Station = Nom, Date = DateDebut.x) %>% 
      arrange(Station, Date)}
  
  if(dim(ListeStations)[1] != 0 & CodeOperation == T & Sortie == "Propre"){
    Operations <- 
      Operations %>%
      filter(Nom %in% ListeStations$Nom) %>% 
      select(Codeoperation, Nom, Limiteamont, DateDebut.x, Modeechantillonnage, XLambert, YLambert, TypeLambert) %>% 
      dplyr::rename(CodeOperation = Codeoperation, Station = Nom, Date = DateDebut.x, X = XLambert, Y = YLambert, TypeCoord = TypeLambert) %>% 
      arrange(Station, Date)}
  
  if(dim(ListeStations)[1] != 0 & CodeOperation == T & Sortie == "Complet"){
    Operations <- 
      Operations %>%
      filter(Nom %in% ListeStations$Nom) %>% 
      dplyr::rename(CodeOperation = Codeoperation, Station = Nom, Date = DateDebut.x, X = XLambert, Y = YLambert, TypeCoord = TypeLambert) %>% 
      arrange(Station, Date)}
  
  # Travail sur l'ensemble des stations
  if(dim(ListeStations)[1] == 0 & CodeOperation == F & Sortie == "Simple"){
    Operations <- 
      Operations %>%
      select(Nom, DateDebut.x) %>% 
      dplyr::rename(Station = Nom, Date = DateDebut.x) %>% 
      arrange(Station, Date)}
  
  if(dim(ListeStations)[1] == 0 & CodeOperation == F & Sortie == "Propre"){
    Operations <- 
      Operations %>%
      select(Nom, Limiteamont, DateDebut.x, Modeechantillonnage, XLambert, YLambert, TypeLambert) %>% 
      dplyr::rename(Station = Nom, Date = DateDebut.x, X = XLambert, Y = YLambert, TypeCoord = TypeLambert) %>% 
      arrange(Station, Date)}
  
  if(dim(ListeStations)[1] == 0 & CodeOperation == F & Sortie == "Complet"){
    Operations <- 
      Operations %>%
      dplyr::rename(CodeOperation = Codeoperation, Station = Nom, Date = DateDebut.x, X = XLambert, Y = YLambert, TypeCoord = TypeLambert) %>% 
      arrange(Station, Date)}
  
  if(dim(ListeStations)[1] == 0 & CodeOperation == T & Sortie == "Simple"){
    Operations <- 
      Operations %>%
      select(Nom, DateDebut.x, Codeoperation) %>% 
      dplyr::rename(CodeOperation = Codeoperation, Station = Nom, Date = DateDebut.x) %>% 
      arrange(Station, Date)}
  
  if(dim(ListeStations)[1] == 0 & CodeOperation == T & Sortie == "Propre"){
    Operations <- 
      Operations %>%
      select(Codeoperation, Nom, Limiteamont, DateDebut.x, Modeechantillonnage, XLambert, YLambert, TypeLambert) %>% 
      dplyr::rename(CodeOperation = Codeoperation, Station = Nom, Date = DateDebut.x, X = XLambert, Y = YLambert, TypeCoord = TypeLambert) %>% 
      arrange(Station, Date)}
  
  if(dim(ListeStations)[1] == 0 & CodeOperation == T & Sortie == "Complet"){
    Operations <- 
      Operations %>%
      dplyr::rename(CodeOperation = Codeoperation, Station = Nom, Date = DateDebut.x, X = XLambert, Y = YLambert, TypeCoord = TypeLambert) %>% 
      arrange(Station, Date)}

  return(Operations)
  
} # Fin de la fonction