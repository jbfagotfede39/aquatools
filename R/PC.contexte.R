#' Contexte de données de physico-chimie
#'
#' Cette fonction permet d'établir le contexte de données de physico-chimie (nombre de stations, de types de mesures, etc.)
#' @name PC.contexte
#' @param data Data.frame issu des fonctions fd_production.physicochimie_mesures
#' @keywords physico-chimie
#' @import glue
#' @import tidyverse
#' @export
#' @examples
#' Contexte <- PC.contexte(Mesures)

##### TODO LIST #####
# Il ne faudrait faire qu'une unique fonction avec PC.contexte et chronique.contexte, avec une fonction préalable de renommage des champs
# Ajouter la fonctionnalité permettant de calculer le nombre d'années biologiques
#####################

PC.contexte <- function(
  data = data
)
{
  
  #### Évaluation des choix ####
  #Recherche <- match.arg(Recherche)
  
  #### Vérification des données en entrée ####
  if("pcmes_date" %in% colnames(data) == TRUE & "pcmes_anneebiol" %in% colnames(data) == FALSE) data <- formatage.annee.biologique(data)
  
  #### Calcul des indicateurs numériques ####
  Contexte <- 
    tibble(nstation = n_distinct(data$pcmes_coderhj)) %>% 
    add_column(ntypemesure = n_distinct(data$pcmes_parametrenom)) %>% 
    # add_column(nannee = n_distinct(data$annee)) %>% 
    add_column(nmilieu = n_distinct(data$pcmes_milieu))
  
  #### Extraction sous forme de liste ####
  Contexte <- 
    Contexte %>% 
    mutate(station = unique(data$pcmes_coderhj) %>% glue_collapse(sep = ";")) %>% 
    add_column(typemesure = unique(data$pcmes_parametrenom) %>% glue_collapse(., sep = ";")) %>% 
    # add_column(annee = unique(data$annee) %>% glue_collapse(., sep = ";")) %>% 
    add_column(milieu = unique(data$pcmes_milieu) %>% glue_collapse(., sep = ";"))
  
  #### Sortie ####
  return(Contexte)
} 
