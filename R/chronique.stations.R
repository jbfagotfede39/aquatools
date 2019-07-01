#' Listage des stations de chroniques
#'
#' Cette fonction permet de lister les stations de la BDD Chroniques
#' @name chronique.stations
#' @param Nom Nom recherché
#' @param Recherche Type de données recherchées
#' @keywords stations
#' @import dplyr
#' @import sf
#' @export
#' @examples
#' chronique.stations("CD39","MO")
#' chronique.stations("Suran","Milieu")
#' chronique.stations("Ain","Bassin")
#' chronique.stations("Serpentine","Sous-bassin")
#' chronique.stations("Lons-le-Saunier","Commune")
#' chronique.stations("39","Département")

##### TODO LIST #####

#####################

chronique.stations <- function(x = "CD39", 
                                Recherche = c("MO", "Milieu", "Bassin", "Sous-bassin", "Commune", "Département")
)
{
  
  ## Évaluation des choix
  Recherche <- match.arg(Recherche)
  
  ## Connexion à la BDD ##
  dbD <- BDD.ouverture("Data")
  
  ## Chargement des données ##
  Stations <- 
    sf::st_read(dbD, query = "SELECT * FROM fd_production.chroniques_stations;") %>% 
    arrange(chsta_coderhj)
  
  ## Fermeture de la BDD ##
  DBI::dbDisconnect(dbD)
  
  ## x en tant que telle
  if(Recherche == "MO") 
    Vue <-
    Stations %>% 
    filter(grepl(x, chsta_mo)) %>% 
    #filter(chsta_milieu == x) %>% 
    #filter(chsta_bassin == x) %>% 
    #filter(chsta_sousbassin == x) %>% 
    #filter(chsta_commune == x) %>% 
    #filter(chsta_departement == x) %>% 
    arrange(chsta_coderhj)
  
  if(Recherche == "Milieu") 
    Vue <-
    Stations %>% 
    #filter(grepl(x, chsta_mo)) %>% 
    filter(chsta_milieu == x) %>% 
    #filter(chsta_bassin == x) %>% 
    #filter(chsta_sousbassin == x) %>% 
    #filter(chsta_commune == x) %>% 
    #filter(chsta_departement == x) %>% 
    arrange(chsta_coderhj)
  
  if(Recherche == "Bassin") 
    Vue <-
    Stations %>% 
    #filter(grepl(x, chsta_mo)) %>% 
    #filter(chsta_milieu == x) %>% 
    filter(chsta_bassin == x) %>% 
    #filter(chsta_sousbassin == x) %>% 
    #filter(chsta_commune == x) %>% 
    #filter(chsta_departement == x) %>% 
    arrange(chsta_coderhj)
  
  if(Recherche == "Sous-bassin") 
    Vue <-
    Stations %>% 
    #filter(grepl(x, chsta_mo)) %>% 
    #filter(chsta_milieu == x) %>% 
    #filter(chsta_bassin == x) %>% 
    filter(chsta_sousbassin == x) %>% 
    #filter(chsta_commune == x) %>% 
    #filter(chsta_departement == x) %>% 
    arrange(chsta_coderhj)
  
  if(Recherche == "Commune") 
    Vue <-
    Stations %>% 
    #filter(grepl(x, chsta_mo)) %>% 
    #filter(chsta_milieu == x) %>% 
    #filter(chsta_bassin == x) %>% 
    #filter(chsta_sousbassin == x) %>% 
    filter(chsta_commune == x) %>% 
    #filter(chsta_departement == x) %>% 
    arrange(chsta_coderhj)
  
  if(Recherche == "Département") 
    Vue <-
    Stations %>% 
    #filter(grepl(x, chsta_mo)) %>% 
    #filter(chsta_milieu == x) %>% 
    #filter(chsta_bassin == x) %>% 
    #filter(chsta_sousbassin == x) %>% 
    #filter(chsta_commune == x) %>% 
    filter(chsta_departement == x) %>% 
    arrange(chsta_coderhj)
  
  ## Affichage des résultats ##
  return(Vue)
}