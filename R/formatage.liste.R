#' Reformatage sous forme d'une liste propre de valeurs
#'
#' Reformate les valeurs d'une colonne sous la forme d'une liste propre
#' @name formatage.liste
#' @param data Jeu de données à traiter
#' @param colonne Colonne à traiter
#' @keywords donnees
#' @import glue
#' @import tidyverse
#' @export
#' @examples
#' data %>% formatage.liste(colonne)
#' stations_exemple %>% formatage.liste("chsta_mo")

formatage.liste <- function(
    data,
    colonne = NA_character_)
  {

  #### Test de cohérence ####
  test_colonne <- colonne %>% nchar()
  if(!(test_colonne %>% is.na())) {
    if(test_colonne == 0) colonne <- NA_real_}
  if(is.na(colonne)) stop("Un nom de colonne doit être fourni")
  
  name <- data %>% rlang::get_expr()
  if(!(colonne %in% colnames(data))) stop(glue("Colonne {colonne} absente de {name}"))
  
  #### Calculs ####
  liste <- glue_collapse(data %>% arrange(.data[[colonne]]) %>% distinct(.data[[colonne]]) %>% pull(.data[[colonne]]), sep = ', ', last = ' et ')
    
  #### Sortie ####
    
return(liste)

} # Fin de la fonction
