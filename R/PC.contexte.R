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
#####################

PC.contexte <- function(
  data
)
{
  
  #### Évaluation des choix ####
  #Recherche <- match.arg(Recherche)
  
  #### Vérification des données en entrée ####
  if(nrow(data) == 0) stop("Aucune donnée en entrée")
  
  #### Création des données manquantes ####
  datacompletees <-
    data %>% 
    {if("pcmes_date" %in% colnames(data) == TRUE & "pcmes_anneebiol" %in% colnames(data) == FALSE) formatage.annee.biologique(., datedebutanneebiol = "01-01") else .} %>% 
    {if("pcmes_milieu" %in% colnames(.) == FALSE) mutate(., pcmes_milieu = NA_character_) else .} %>% 
    {if("pcmes_unitenom" %in% colnames(.) == FALSE) mutate(., pcmes_unitenom = NA_character_) else .} %>% 
    arrange(pcmes_date)
  
  #### Calcul des indicateurs numériques ####
  contexte_1 <- 
    tibble(n_station = n_distinct(datacompletees$pcmes_coderhj)) %>% 
    add_column(n_typemesure = n_distinct(datacompletees$pcmes_parametrenom)) %>% 
    add_column(n_date = n_distinct(datacompletees$pcmes_date)) %>%
    add_column(n_annee = n_distinct(datacompletees$pcmes_anneebiol)) %>%
    add_column(n_unitenom = n_distinct(datacompletees$pcmes_unitenom)) %>% 
    {if(all(is.na(datacompletees$pcmes_milieu))) add_column(., n_milieu = 0) else .} %>%
    {if("n_milieu" %in% colnames(.) == FALSE) add_column(., n_milieu = n_distinct(datacompletees$pcmes_milieu)) else .}

  #### Extraction sous forme de liste ####
  contexte_2 <- 
    contexte_1 %>% 
    mutate(station = unique(datacompletees$pcmes_coderhj) %>% glue_collapse(sep = ";")) %>% 
    add_column(typemesure = unique(datacompletees$pcmes_parametrenom) %>% glue_collapse(., sep = ";")) %>% 
    add_column(date = unique(datacompletees$pcmes_date) %>% glue_collapse(., sep = ";")) %>%
    add_column(annee = unique(datacompletees$pcmes_anneebiol) %>% glue_collapse(., sep = ";")) %>%
    add_column(unitenom = unique(datacompletees$pcmes_unitenom) %>% glue_collapse(., sep = ";")) %>% 
    add_column(milieu = unique(datacompletees$pcmes_milieu) %>% glue_collapse(., sep = ";"))
  
  #### Sortie ####
  return(contexte_2)
} 
