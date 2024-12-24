#' Reformatage d'une colonne time en deux colonnes de dates et heures
#'
#' Reformate une colonne time en deux colonnes de dates et heures
#' @name formatage.date.heure
#' @param data Jeu de données contenant une colonne `time`
#' @keywords donnees
#' @import glue
#' @import tidyverse
#' @export
#' @examples
#' formatage.date.heure(data)

formatage.date.heure <- function(
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
  
  colname_date <- glue("{entete}_date")
  colname_heure <- glue("{entete}_heure")
  
  #### Test de cohérence ####
  if(all(grepl("time", liste_cols) == FALSE)) stop("Pas de colonne time en entrée")
  
  #### Nettoyage & reformatage ####
  data_nettoyees <- 
    data %>% 
    dplyr::select(-contains("date"), -contains("heure")) # On enlève les colonnes si elles existent déjà pour être certain du résultat

  #### Calculs ####
  data_sortie <- 
    data_nettoyees %>% 
    mutate({{colname_heure}} := format(time, format="%H:%M:%S"), .after = "time") %>%
    mutate({{colname_date}} := ymd(format(time, format="%Y-%m-%d")), .after = "time") %>% 
    select(-time)
  
  #### Sortie ####
return(data_sortie)

} # Fin de la fonction
