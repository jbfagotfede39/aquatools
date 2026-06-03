#' Filtrage des périodes estivales uniquement
#'
#' Permet de ne conserver que les périodes estivales (mois 06, 07, 08 et 09)
#' @name filtrage.estival
#' @param data Jeu de données contenant une colonne \code{_date} au format Date
#' @keywords donnees
#' @import tidyverse
#' @export
#' @examples
#' filtrage.estival(data)
#' mesures_annuelles %>% filtrage.estival()

filtrage.estival <- function(
  data = data)
  {
  
  #### Test de cohérence ####
  cols_date <- 
    data %>%
    select(where(~ inherits(., "Date"))) %>%
    names()
  if(length(cols_date) == 0) stop("Absence de colonne au format date en entrée")
  if(length(cols_date) > 1) stop("Plusieurs colonnes au format date en entrée")
  
  #### Calculs ####
  data <- 
    data %>% 
    formatage.mois() %>% 
    filter(mois %in% c("06", "07", "08", "09")) %>% 
    select(-mois)
  
  #### Sortie des données  ####
  return(data)

} # Fin de la fonction