#' Extraire la distance à la source du CodeRDT
#'
#' Cette fonction permet d'extraire la distance à la source à partir du CodeRDT
#' 
#' @param data Dataframe contenant une colonne CodeRDT
#' @keywords stations
#' @import stringr
#' @import tidyverse
#' @export
#' @examples
#' stations.DistSource(data)

##### TODO LIST #####
# 
#####################

stations.DistSource <- function(
  data=data)
{
  
  ## Extraction de la distance à la source ##
  data <-
    data %>% 
    mutate(DistSource = str_extract(data$CodeRDT, "[0-9][0-9][0-9]-[0-9]")) %>% 
    mutate(DistSource = ifelse(is.na(DistSource), str_extract(data$CodeRDT, "[0-9][0-9]-[0-9]"), DistSource)) %>% 
    mutate(DistSource = ifelse(is.na(DistSource), str_extract(data$CodeRDT, "[0-9]-[0-9]"), DistSource)) %>% 
    mutate(DistSource = str_replace(DistSource, "-", ".")) %>% 
    mutate(DistSource = as.numeric(DistSource))
  
  ## Restitution des données ##

  return(data)
  
} # Fin de la fonction