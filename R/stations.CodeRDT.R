#' Extraire la distance à la source ou le code écosystème du CodeRDT
#'
#' Cette fonction permet d'extraire la distance à la source ou le code écosystème à partir du CodeRDT
#' @name stations.CodeRDT
#' @param data Dataframe contenant une colonne CodeRDT
#' @param DistSource Si \code{T} (par défaut), extrait la distance à la source - Extrait sinon le code de l'écosystème
#' @keywords stations
#' @import stringr
#' @import tidyverse
#' @export
#' @examples
#' stations.CodeRDT(data)
#' stations.CodeRDT(data, DistSource = F)
#' arrange(CodeRDT, Date) %>% stations.CodeRDT(DistSource = F)

##### TODO LIST #####
# [A-Z]* doit fonctionner, plus simple que trois lignes...
#####################

stations.CodeRDT <- function(
  data=data,
  DistSource = T)
{
  
  ## Extraction de la distance à la source ##
if(DistSource == T){
  data <-
    data %>% 
    mutate(DistSource = str_extract(data$CodeRDT, "[0-9][0-9][0-9]-[0-9]")) %>% 
    mutate(DistSource = ifelse(is.na(DistSource), str_extract(data$CodeRDT, "[0-9][0-9]-[0-9]"), DistSource)) %>% 
    mutate(DistSource = ifelse(is.na(DistSource), str_extract(data$CodeRDT, "[0-9]-[0-9]"), DistSource)) %>% 
    mutate(DistSource = str_replace(DistSource, "-", ".")) %>% 
    mutate(DistSource = as.numeric(DistSource))
}
  
  ## Extraction du code écosystème ##
  if(DistSource == F){
    data <-
      data %>% 
      mutate(temp = str_replace_all(CodeRDT, "[-]","")) %>% 
      mutate(CodeEcos = str_extract(temp, "[A-Z][A-Z][A-Z][A-Z]")) %>% 
      mutate(CodeEcos = ifelse(is.na(CodeEcos), str_extract(temp, "[A-Z][A-Z][A-Z]"), CodeEcos)) %>% 
      mutate(CodeEcos = ifelse(grepl(" A ", CodeRDT), NA, CodeEcos)) %>% # Pour négliger les CodeRDT qui contiennent en réalisé le nom DCE de la station
      select(-temp) %>% 
      mutate(CodeEcos = as.character(CodeEcos))
  }
  
  ## Restitution des données ##

  return(data)
  
} # Fin de la fonction