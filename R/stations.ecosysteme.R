#' Listage des stations d'un cours d'eau
#'
#' Cette fonction permet de lister les stations d'un cours d'eau donné
#' 
#' @param Nom du cours d'eau
#' @keywords stations
#' @import dplyr DBI RSQLite
#' @export
#' @examples
#' stations.ecosysteme("Ain")

##### TODO LIST #####
# Rajouter le choix de l'écosystème avec les codes (avec recherche dans le champ observation) et de même avec le code de la masse d'eau, avec un interrupteur disant où la fonction doit chercher initialement. Le reste du code reste identique.
#####################

stations.ecosysteme <- function(
  ecosysteme="Ain")
{
  
  ## Ouverture de la BDD ##
  db <- BDD.ouverture(Type = "Poissons")
  
  ## Récupération des données ##
  Stations <- tbl(db,"Stations") %>% collect(n = Inf)
  Ecosystemes <- tbl(db,"Ecosystemes") %>% collect(n = Inf)
  
  ## Extraction des afférences ##
  # Test si le nom existe bien, sinon message d'erreur et arrêt de la fonction #
  
  if(dim(Ecosystemes %>% filter(Nomecosysteme == ecosysteme)
  )[1] == 0) 
    stop("Attention : nom d'écosystème absent de la base de données")
  
  ecosysteme <-
    Ecosystemes %>% 
    filter(Nomecosysteme == ecosysteme)
  
  # Extraction du Codeecosysteme du CE qui nous concerne
  ecosysteme[,1]
  
  # Recherche des stations qui ont un Codeecosysteme = à ce Codeecosysteme
  Stations <-
    Stations %>% 
    filter(Codeecosysteme == as.character(ecosysteme[,1])) %>% 
    #filter(Codeecosysteme == ecosysteme[,1]) %>% # Ancienne version
    select(Nom)
  
  
  #rm(list=setdiff(ls(), "EcosystemeRechercheV6")) # Pour ne conserver que l'objet final
  #return(EcosystemeRechercheV6)
  
} # Fin de la fonction