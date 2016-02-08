#' Listage des stations de chroniques
#'
#' Cette fonction permet de lister les stations de la BDD Chroniques
#' 
#' @param Nom Nom recherché
#' @param Recherche Type de données recherchées
#' @keywords stations
#' @import dplyr DBI RSQLite
#' @export
#' @examples
#' chronique.stations("CD39","MO")
#' chronique.stations("Suran","Milieu")
#' chronique.stations("Ain","Bassin")
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
  db <- BDD.ouverture()
  
  ## Chargement des données ##
  Stations <- dbReadTable(db, "Stations")
  
  ## x en tant que telle
  if(Recherche == "MO") 
    Vue <-
    Stations %>% 
    filter(MO == x) %>% 
    #filter(Milieu == x) %>% 
    #filter(Bassin == x) %>% 
    #filter(Sous-bassi == x) %>% 
    #filter(Commune == x) %>% 
    #filter(Departement == x) %>% 
    arrange(CodeRDT)
  
  if(Recherche == "Milieu") 
    Vue <-
    Stations %>% 
    #filter(MO == x) %>% 
    filter(Milieu == x) %>% 
    #filter(Bassin == x) %>% 
    #filter(Sous-bassi == x) %>% 
    #filter(Commune == x) %>% 
    #filter(Departement == x) %>% 
    arrange(CodeRDT)
  
  if(Recherche == "Bassin") 
    Vue <-
    Stations %>% 
    #filter(MO == x) %>% 
    #filter(Milieu == x) %>% 
    filter(Bassin == x) %>% 
    #filter(Sous-bassi == x) %>% 
    #filter(Commune == x) %>% 
    #filter(Departement == x) %>% 
    arrange(CodeRDT)
  
  if(Recherche == "Sous-bassin") 
    Vue <-
    Stations %>% 
    #filter(MO == x) %>% 
    #filter(Milieu == x) %>% 
    #filter(Bassin == x) %>% 
    filter(Sous-bassi == x) %>% 
    #filter(Commune == x) %>% 
    #filter(Departement == x) %>% 
    arrange(CodeRDT)
  
  if(Recherche == "Commune") 
    Vue <-
    Stations %>% 
    #filter(MO == x) %>% 
    #filter(Milieu == x) %>% 
    #filter(Bassin == x) %>% 
    #filter(Sous-bassi == x) %>% 
    filter(Commune == x) %>% 
    #filter(Departement == x) %>% 
    arrange(CodeRDT)
  
  if(Recherche == "Département") 
    Vue <-
    Stations %>% 
    #filter(MO == x) %>% 
    #filter(Milieu == x) %>% 
    #filter(Bassin == x) %>% 
    #filter(Sous-bassi == x) %>% 
    #filter(Commune == x) %>% 
    filter(Departement == x) %>% 
    arrange(CodeRDT)
  
  ## Affichage des résultats ##
  return(Vue)
}