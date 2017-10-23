#' Listage des stations d'un cours d'eau
#'
#' Cette fonction permet de lister les stations d'un cours d'eau donné dans les BDD poisson et chronique
#' 
#' @param Nom du cours d'eau
#' @param shp Si \code{FALSE} (par défault), n'exporte pas de shp dans le répertoire courant.
#' @keywords stations
#' @import tidyverse lubridate
#' @export
#' @examples
#' stations.ecosysteme()
#' stations.ecosysteme("Ain")
#' stations.ecosysteme("Ain", shp=T)

##### TODO LIST #####
# Rajouter le choix de l'écosystème avec les codes (avec recherche dans le champ observation) et de même avec le code de la masse d'eau, avec un interrupteur disant où la fonction doit chercher initialement. Le reste du code reste identique.
# Option pour chercher les stations des afférences
# Si aucune capture sur la station poisson, indiquer comme type de données "autre"
# Rajouter les sorties des stations IAM, MI et PC
#####################

stations.ecosysteme <- function(
  ecosysteme="",
  shp = F)
{
  
  #### Base poissons ####
  ## Ouverture de la BDD ##
  db <- BDD.ouverture(Type = "Poissons")
  
  ## Récupération des données de l'écosystème ##
  Ecosystemes <- tbl(db,"Ecosystemes") %>% filter(Nomecosysteme == ecosysteme) %>% collect() 

  # Recherche des stations qui ont un Codeecosysteme = à ce Codeecosysteme et transformation
  Code <- as.character(Ecosystemes[1,1])
  
  if(nchar(ecosysteme) != 0){
  StationsPoissons <- 
    tbl(db,"Stations") %>% 
    filter(Codeecosysteme == Code) %>% 
    collect() %>% 
    rename(X = XLambert) %>% 
    rename(Y = YLambert) %>% 
    rename(TypeCoord = TypeLambert) %>% 
    mutate(Poisson = "Oui")}
  
  if(nchar(ecosysteme) == 0){
    StationsPoissons <- 
      tbl(db,"Stations") %>% 
      #filter(Codeecosysteme == Code) %>% 
      collect() %>% 
      rename(X = XLambert) %>% 
      rename(Y = YLambert) %>% 
      rename(TypeCoord = TypeLambert) %>% 
      mutate(Poisson = "Oui")}

  #### Base chronique ####
  ## Ouverture de la BDD ##
  db <- BDD.ouverture(Type = "Chroniques")
  
  ## Récupération des données de l'écosystème ##
  if(nchar(ecosysteme) != 0){
  StationsChroniques <- 
    tbl(db,"Stations") %>% 
    filter(Milieu == ecosysteme) %>% 
    collect() %>% 
    rename(Nom = CodeRDT) %>% 
    mutate(Chronique = "Oui")}
  
  if(nchar(ecosysteme) == 0){
    StationsChroniques <- 
      tbl(db,"Stations") %>% 
      #filter(Milieu == ecosysteme) %>% 
      collect() %>% 
      rename(Nom = CodeRDT) %>% 
      mutate(Chronique = "Oui")}
  
  #### Synthèse ####
  Synthese <- 
    StationsPoissons %>% 
    full_join(StationsChroniques) %>% 
    select(Nom, X, Y, TypeCoord, Poisson, Chronique) %>% 
    filter(TypeCoord == "L93") %>% arrange(Nom)
  
  # Test si le nom existe bien, sinon message d'erreur et arrêt de la fonction #
  if(dim(Synthese)[1] == 0) 
    stop("Attention : nom d'écosystème absent des bases de données")

  # Sortie
  if(shp == F){
    return(Synthese)
  }

  
  # Export shp
  if(shp == T){
    SIG.exportSHP(Synthese, Synthese$X, Synthese$Y, paste0(format(now(), format="%Y-%m-%d"),"_",ecosysteme,"_Export_stations"))
  }
  
} # Fin de la fonction