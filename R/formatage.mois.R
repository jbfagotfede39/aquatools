#' Reformatage de dates en mois
#'
#' Reformate les dates en mois dans une colonne \code{mois}
#' @name formatage.mois
#' @param data Jeu de données contenant une colonne \code{_date} au format Date
#' @keywords donnees
#' @import tidyverse
#' @export
#' @examples
#' formatage.mois(data)
#' mesures_annuelles %>% formatage.mois() %>% filter(mois %in% c("6", "7", "8", "9"))

formatage.mois <- function(
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
    mutate(mois = format(!!sym(cols_date), "%m"))
  
  #### Sortie des données  ####
  return(data)

} # Fin de la fonction