#' Reformatage d'une colonne heure avec seulement les heures
#'
#' Reformate une colonne heure avec seulement l'heure plein
#' @name formatage.heure.simple
#' @param data Jeu de données contenant une colonne `_heure`
#' @keywords donnees
#' @import glue
#' @import tidyverse
#' @export
#' @examples
#' formatage.heure.simple(data)

formatage.heure.simple <- function(
  data)
  {

  #### Contexte ####
  liste_cols <- data %>% names()
  if(any(grepl("chmes", liste_cols) == TRUE)) entete <- "chmes"
  if(any(grepl("chsvi", liste_cols) == TRUE)) entete <- "chsvi"
  if(any(grepl("chmesgr", liste_cols) == TRUE)) entete <- "chmesgr"
  if(any(grepl("chres", liste_cols) == TRUE)) entete <- "chres"
  if(any(grepl("pcmes", liste_cols) == TRUE)) entete <- "pcmes"
  if(any(grepl("pcsvi", liste_cols) == TRUE)) entete <- "pcsvi"
  
  colname_heure <- glue("{entete}_heure")
  colname_heure_simple <- glue("{entete}_heure_simple")
  
  #### Test de cohérence ####
  if(all(grepl("heure", liste_cols) == FALSE)) stop("Pas de colonne heure en entrée")
  
  #### Nettoyage & reformatage ####
  data_nettoyees <- 
    data %>% 
    dplyr::select(-contains("heure_simple")) # On enlève la colonne si elle existe déjà pour être certain du résultat

  #### Calculs ####
  data_sortie <- 
    data_nettoyees %>% 
    mutate({{colname_heure_simple}} := str_sub(!!sym(glue("{colname_heure}")), 1, 2), .after = all_of(colname_heure))
  
  #### Sortie ####
return(data_sortie)

} # Fin de la fonction
