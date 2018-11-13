#' Listage des résultats de chroniques
#'
#' Cette fonction permet de lister les résultats de la BDD Chroniques
#' @name chronique.resultats
#' @param Nom Nom recherché
#' @param Recherche Type de données recherchées
#' @keywords capteurs
#' @import dplyr
#' @export
#' @examples
#' chronique.capteurs("HER0-6","Station")
#' chronique.capteurs("2017","Année biologique")

##### TODO LIST #####

#####################

chronique.resultats <- function(x = "HER0-6", 
                               Recherche = c("Station", "Année biologique")
)
{
  
  ## Évaluation des choix
  Recherche <- match.arg(Recherche)
  
  ## Connexion à la BDD ##
  dbD <- BDD.ouverture("Data")
  
  ## Chargement des données ##
  Resultats <- 
    tbl(dbD, in_schema("fd_production", "chroniques_resultats")) %>% 
    collect()
  
  ## x en tant que telle
  if(Recherche == "Station") 
    Vue <-
    Resultats %>% 
    filter(chres_coderdt == x) %>% 
    #filter(chres_anneebiol == x) %>% 
    arrange(chres_coderdt, chres_anneebiol)
  
  if(Recherche == "Année biologique") 
    Vue <-
    Resultats %>% 
    #filter(chres_coderdt == x) %>% 
    filter(chres_anneebiol == x) %>% 
    arrange(chres_coderdt, chres_anneebiol)
  
  ## Affichage des résultats ##
  return(Vue)
}