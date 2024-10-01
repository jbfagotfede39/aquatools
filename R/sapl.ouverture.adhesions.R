#' Ouverture des fichiers d'adhérents extraits de cartedepeche.fr
#'
#' Cette fonction permet l'ouverture des fichiers d'adhérents issus du site internet cartedepeche.fr
#' @name sapl.ouverture.adhesions
#' @param fichier Fichier à ouvrir
#' @keywords stations
#' @import janitor
#' @import tidyverse
#' @export
#' @examples
#' sapl.ouverture.adhesions(fichier)

sapl.ouverture.adhesions <- function(
  fichier
  )
{

  #### Collecte des données ####
  saison <- fichier %>% read_lines(skip = 3, n_max = 1) %>% parse_number()
  sapl <- fichier %>% read_lines(skip = 10, n_max = 1) %>% str_conv("Latin-1") %>% str_replace("\"AAPPMA : ", "") %>% str_replace("\"", "")
  data <-
    fichier %>% read_csv2(skip = 11) %>% 
    janitor::clean_names()

  #### Sortie ####
  return(data)
  
} # Fin de la fonction