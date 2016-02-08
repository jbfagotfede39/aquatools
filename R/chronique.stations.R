#' Listage des stations de chroniques
#'
#' Cette fonction permet de lister les stations de la BDD Chroniques
#' 
#' @param Nom Nom recherché
#' @param Type Type de donnée de suivi
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
                                Type = c("MO", "Milieu", "Bassin", "Sous-bassin", "Commune", "Département")
)
{
  
  ## Évaluation des choix
  Type <- match.arg(Type)
  
  ## Connexion à la BDD ##
  db <- BDD.ouverture(Type = "Chroniques")
  
  ## Chargement des données ##
  Stations <- dbReadTable(db, "Stations")
  
  ## x en tant que telle
  if(Type == "MO") 
    Vue <-
    Stations %>% 
    filter(MO == x) %>% 
    #filter(Milieu == x) %>% 
    #filter(Bassin == x) %>% 
    #filter(Sous-bassi == x) %>% 
    #filter(Commune == x) %>% 
    #filter(Departement == x) %>% 
    arrange(CodeRDT)
  
  if(Type == "Milieu") 
    Vue <-
    Stations %>% 
    #filter(MO == x) %>% 
    filter(Milieu == x) %>% 
    #filter(Bassin == x) %>% 
    #filter(Sous-bassi == x) %>% 
    #filter(Commune == x) %>% 
    #filter(Departement == x) %>% 
    arrange(CodeRDT)
  
  if(Type == "Bassin") 
    Vue <-
    Stations %>% 
    #filter(MO == x) %>% 
    #filter(Milieu == x) %>% 
    filter(Bassin == x) %>% 
    #filter(Sous-bassi == x) %>% 
    #filter(Commune == x) %>% 
    #filter(Departement == x) %>% 
    arrange(CodeRDT)
  
  if(Type == "Sous-bassin") 
    Vue <-
    Stations %>% 
    #filter(MO == x) %>% 
    #filter(Milieu == x) %>% 
    #filter(Bassin == x) %>% 
    filter(Sous-bassi == x) %>% 
    #filter(Commune == x) %>% 
    #filter(Departement == x) %>% 
    arrange(CodeRDT)
  
  if(Type == "Commune") 
    Vue <-
    Stations %>% 
    #filter(MO == x) %>% 
    #filter(Milieu == x) %>% 
    #filter(Bassin == x) %>% 
    #filter(Sous-bassi == x) %>% 
    filter(Commune == x) %>% 
    #filter(Departement == x) %>% 
    arrange(CodeRDT)
  
  if(Type == "Département") 
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