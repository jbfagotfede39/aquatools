#' Contexte de données de chroniques
#'
#' Cette fonction permet d'établir le contexte de données de chroniques (nombre de stations, d'années biologiques, de types de mesures, etc.)
#' @name chronique.contexte
#' @param data Data.frame issu des fonctions chronique.mesures, chronique.agregation ou chronique.resultats
#' @keywords chronique
#' @import tidyverse
#' @export
#' @examples
#' chronique.mesures("DRO6-8") %>% chronique.contexte()
#' Contexte <- chronique.contexte(Mesures)
#' Contexte <- chronique.contexte(chronique.mesures("DRO6-8"))

##### TODO LIST #####
# Intérêt de créer ? pour mesures Contexte$njours <- n_distinct(syntjour$chmes_date) + total pour les résultats à partir de la colonne concernée ? -> Si modification, le retranscrire dans chronique.figure.parametres
#####################

chronique.contexte <- function(
  data = data
)
{
  
  #### Évaluation des choix ####
  #Recherche <- match.arg(Recherche)
  
  #### Vérification des données en entrée ####
  if("chmes_date" %in% colnames(data) == TRUE & "chmes_anneebiol" %in% colnames(data) == FALSE) data <- formatage.annee.biologique(data)
  
  #### Homogénéisation des noms de champs ####
  datarenomees <-
    data %>% 
    # Stations
    rename_at(vars(contains("chmes_coderhj")), list( ~ str_replace(., "chmes_coderhj", "coderhj"))) %>% 
    rename_at(vars(contains("chsta_coderhj")), list( ~ str_replace(., "chsta_coderhj", "coderhj"))) %>% 
    rename_at(vars(contains("chres_coderhj")), list( ~ str_replace(., "chres_coderhj", "coderhj"))) %>% 
    # Type de mesures
    rename_at(vars(contains("chmes_typemesure")), list( ~ str_replace(., "chmes_typemesure", "typemesure"))) %>% 
    rename_at(vars(contains("chres_typemesure")), list( ~ str_replace(., "chres_typemesure", "typemesure"))) %>% 
    # Année
    rename_at(vars(contains("chmes_anneebiol")), list( ~ str_replace(., "chmes_anneebiol", "annee"))) %>%
    rename_at(vars(contains("chres_anneevmm")), list( ~ str_replace(., "chres_anneevmm", "annee"))) %>%  
    rename_at(vars(contains("AnneeVMM")), list( ~ str_replace(., "AnneeVMM", "annee"))) %>% 
    # Milieu
    rename_at(vars(contains("chsta_milieu")), list( ~ str_replace(., "chsta_milieu", "milieu")))
  
  #### Création des données manquantes ####
  datacompletees <-
    datarenomees %>% 
    {if("coderhj" %in% colnames(.) == FALSE) mutate(., coderhj = NA_character_) else .} %>% 
    {if("typemesure" %in% colnames(.) == FALSE) mutate(., typemesure = NA_character_) else .} %>% 
    {if("milieu" %in% colnames(.) == FALSE) mutate(., milieu = NA_character_) else .}
    
  #### Calcul des indicateurs numériques ####
  Contexte <- 
    tibble(nstation = n_distinct(datacompletees$coderhj)) %>% 
    add_column(ntypemesure = n_distinct(datacompletees$typemesure)) %>% 
    add_column(nannee = n_distinct(datacompletees$annee)) %>% 
    add_column(nmilieu = n_distinct(datacompletees$milieu))
  
  #### Extraction sous forme de liste ####
  Contexte <- 
    Contexte %>% 
    mutate(station = unique(datacompletees$coderhj) %>% glue_collapse(sep = ";")) %>% 
    add_column(typemesure = unique(datacompletees$typemesure) %>% glue_collapse(., sep = ";")) %>% 
    add_column(annee = unique(datacompletees$annee) %>% glue_collapse(., sep = ";")) %>% 
    add_column(milieu = unique(datacompletees$milieu) %>% glue_collapse(., sep = ";"))
 
   #### Sortie ####
  return(Contexte)
} 