#' Listage des capteurs de chroniques
#'
#' Cette fonction permet de lister les capteurs de la BDD Chroniques
#' @name chronique.capteurs
#' @param Nom Nom recherché
#' @param Recherche Type de données recherchées
#' @keywords capteurs
#' @import dplyr
#' @export
#' @examples
#' chronique.capteurs("CD39","Propriétaire")
#' chronique.capteurs("Thermie","Type")
#' chronique.capteurs("Hobo UA-001-64","Modèle")
#' chronique.capteurs("10165890","Numéro")
#' chronique.capteurs("PDPG","Projet")
#' chronique.capteurs("OK","État")

##### TODO LIST #####

#####################

chronique.capteurs <- function(x = "CD39", 
                               Recherche = c("Propriétaire", "Type", "Modèle", "Numéro", "État", "Projet")
)
{
  
  ## Évaluation des choix
  Recherche <- match.arg(Recherche)
  
  ## Connexion à la BDD ##
  dbD <- BDD.ouverture("Data")
  
  ## Chargement des données ##
  Capteurs <- 
    tbl(dbD, in_schema("fd_production", "chroniques_capteurs")) %>% 
    collect()
  
  ## x en tant que telle
  if(Recherche == "Propriétaire") 
    Vue <-
    Capteurs %>% 
    filter(chcap_proprietaire == x) %>% 
    #filter(chcap_typecapteur == x) %>% 
    #filter(chcap_modelecapteur == x) %>% 
    #filter(chcap_numerocapteur == x) %>% 
    #filter(chcap_etat == x) %>% 
    #filter(chcap_projet == x) %>% 
    arrange(chcap_numerocapteur)
  
  if(Recherche == "Type") 
    Vue <-
    Capteurs %>% 
    #filter(chcap_proprietaire == x) %>% 
    filter(chcap_typecapteur == x) %>% 
    #filter(chcap_modelecapteur == x) %>% 
    #filter(chcap_numerocapteur == x) %>% 
    #filter(chcap_etat == x) %>% 
    #filter(chcap_projet == x) %>% 
    arrange(chcap_numerocapteur)
  
  if(Recherche == "Modèle") 
    Vue <-
    Capteurs %>% 
    #filter(chcap_proprietaire == x) %>% 
    #filter(chcap_typecapteur == x) %>% 
    filter(chcap_modelecapteur == x) %>% 
    #filter(chcap_numerocapteur == x) %>% 
    #filter(chcap_etat == x) %>% 
    #filter(chcap_projet == x) %>% 
    arrange(chcap_numerocapteur)
  
  if(Recherche == "Numéro") 
    Vue <-
    Capteurs %>% 
    #filter(chcap_proprietaire == x) %>% 
    #filter(chcap_typecapteur == x) %>% 
    #filter(chcap_modelecapteur == x) %>% 
    filter(chcap_numerocapteur == x) %>% 
    #filter(chcap_etat == x) %>% 
    #filter(chcap_projet == x) %>% 
    arrange(chcap_numerocapteur)
  
  if(Recherche == "État") 
    Vue <-
    Capteurs %>% 
    #filter(chcap_proprietaire == x) %>% 
    #filter(chcap_typecapteur == x) %>% 
    #filter(chcap_modelecapteur == x) %>% 
    #filter(chcap_numerocapteur == x) %>% 
    filter(chcap_etat == x) %>% 
    #filter(chcap_projet == x) %>% 
    arrange(chcap_numerocapteur)
  
  if(Recherche == "Projet") 
    Vue <-
    Capteurs %>% 
    #filter(chcap_proprietaire == x) %>% 
    #filter(chcap_typecapteur == x) %>% 
    #filter(chcap_modelecapteur == x) %>% 
    #filter(chcap_numerocapteur == x) %>% 
    #filter(chcap_etat == x) %>% 
    filter(chcap_projet == x) %>% 
    arrange(chcap_numerocapteur)
  
  ## Affichage des résultats ##
  return(Vue)
}