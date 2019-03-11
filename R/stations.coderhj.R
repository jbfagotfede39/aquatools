#' Extraire la distance à la source ou le code écosystème du coderhj
#'
#' Cette fonction permet d'extraire la distance à la source ou le code écosystème à partir du coderhj
#' @name stations.coderhj
#' @param data Dataframe contenant une colonne coderhj
#' @param DistSource Si \code{T} (par défaut), extrait la distance à la source - Extrait sinon le code de l'écosystème
#' @keywords stations
#' @import stringr
#' @import tidyverse
#' @export
#' @examples
#' stations.coderhj(data)
#' stations.coderhj(data, DistSource = F)
#' arrange(coderhj, Date) %>% stations.coderhj(DistSource = F)

##### TODO LIST #####
# [A-Z]* doit fonctionner, plus simple que trois lignes...
#####################

stations.coderhj <- function(
  data=data,
  DistSource = T)
{
  
  ## Reformatage ##
data <-
  data %>% 
  rename_at(vars(contains("CodeRDT")), funs(str_replace(., "CodeRDT", "coderhj"))) %>% 
  rename_at(vars(contains("coderdt")), funs(str_replace(., "coderdt", "coderhj"))) %>% 
  rename_at(vars(contains("coderhj")), funs(str_replace(., ., "coderhj")))

  ## Test de vérification ##
if(!("coderhj" %in% names(data))) stop("Pas de colonne contenant coderdt ou coderhj dans les données sources")
  
  ## Extraction de la distance à la source ##
if(DistSource == T){
  data <-
    data %>% 
    mutate(DistSource = str_extract(data$coderhj, "[0-9][0-9][0-9]-[0-9]")) %>% 
    mutate(DistSource = ifelse(is.na(DistSource), str_extract(data$coderhj, "[0-9][0-9]-[0-9]"), DistSource)) %>% 
    mutate(DistSource = ifelse(is.na(DistSource), str_extract(data$coderhj, "[0-9]-[0-9]"), DistSource)) %>% 
    mutate(DistSource = str_replace(DistSource, "-", ".")) %>% 
    mutate(DistSource = as.numeric(DistSource))
}
  
  ## Extraction du code écosystème ##
  if(DistSource == F){
    data <-
      data %>% 
      mutate(temp = str_replace_all(coderhj, "[-]","")) %>% 
      mutate(codemilieu = str_extract(temp, "[A-Z][A-Z][A-Z][A-Z]")) %>% 
      mutate(codemilieu = ifelse(is.na(codemilieu), str_extract(temp, "[A-Z][A-Z][A-Z]"), codemilieu)) %>% 
      mutate(codemilieu = ifelse(grepl(" A ", coderhj), NA, codemilieu)) %>% # Pour négliger les coderhj qui contiennent en réalité le nom DCE de la station
      select(-temp) %>% 
      mutate(codemilieu = as.character(codemilieu))
  }
  
  ## Restitution des données ##

  return(data)
  
} # Fin de la fonction