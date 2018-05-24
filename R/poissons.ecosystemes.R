#' Extraction de données de stations
#'
#' Cette fonction permet d'extraire les données complètes de l'ensemble des écosystèmes (par défaut) ou d'un seul
#' 
#' @param Nom de l'écosystème
#' @keywords poissons
#' @import dplyr
#' @import RSQLite
#' @import DBI
#' @export
#' @examples
#' listeCE <- poissons.ecosystemes()
#' listeCE <- poissons.ecosystemes("Valouse")

##### TODO LIST #####
# 
#####################

poissons.ecosystemes <- function(
  ecosysteme="")
{
  
  
  ## Ouverture de la BDD ##
  db <- BDD.ouverture(Type = "Poissons")
  
  ## Récupération des données ##
  Ecosystemes <- tbl(db,"ecosystemes") %>% collect(n = Inf)
  
  ## Extraction des données de l'écosystème si un est spécifié ##
  if(nchar(ecosysteme) != 0) {
    # Test si le nom existe bien, sinon message d'erreur et arrêt de la fonction #
    if(dim(Ecosystemes %>% filter(Nomecosysteme == ecosysteme))[1] == 0) 
      stop("Attention : nom de l'écosystème absent de la base de données")
    
    # filtrage en tant que tel 
    ecosysteme <-
      Ecosystemes %>% 
      filter(Nomecosysteme == ecosysteme)}
  
  ## Extraction de tous les écosystèmes si aucun spécifié ##
  if(nchar(ecosysteme) == 0) {
    ecosysteme <- Ecosystemes}
  
} # Fin de la fonction