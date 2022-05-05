#' Regroupement de chroniques
#'
#' Cette fonction permet de regrouper des chroniques de mesures (température, niveaux, etc.) dans un tableau au format large, avec une colonne date, une colonne heure puis des colonnes avec les différentes stations/paramètres
#' @name chronique.regroupement
#' @param data Data.frame contenant a minima une colonne chmes_date, une colonne chmes_heure, une colonne chmes_coderhj, une colonne chmes_typemesure et une colonne chmes_valeur
#' @param stations Dataframe de stations, avec au moins une colonne chsta_coderhj et une colonne chsta_distancesource. Permet le classement des stations par ordre de distance à la source
#' @param formatcle Format de la clé d'identification (\code{STU} par défaut)
#' @param projet Nom du projet
#' @param export Si \code{TRUE}, exporte les résultats (\code{FALSE} par défaut)
#' @keywords chronique
#' @import glue
#' @import openxlsx
#' @import tidyverse
#’ @importFrom dplyr select
#' @export
#' @examples
#' DataTravail <- chronique.regroupement(data)
#' DataTravail <- chronique.regroupement(data, formatcle = "MSTU")

chronique.regroupement <- function(
  data = data,
  stations = NA_character_,
  formatcle = c("STU", "MSTU"),
  projet = NA_character_,
  export = FALSE
)
{

#### Évaluation des choix ####
formatcle <- match.arg(formatcle)

#### Tests ####
interrupteur_stations <- FALSE
if(!is.null(nrow(stations)) & all(c("chsta_coderhj", "chsta_distancesource") %in% names(stations))) interrupteur_stations <- TRUE

#### Mise au format des données ####
mesures_regroupees <-
  data %>%
  formatage.annee.biologique() %>%
  chronique.cle(formatcle = formatcle) %>% 
  # Pour tri par gradient amont-aval
  {if(interrupteur_stations == TRUE) left_join(., stations %>% select(chsta_coderhj, chsta_distancesource) %>% st_drop_geometry(), by = c("chmes_coderhj" = "chsta_coderhj")) else .} %>% 
  {if(interrupteur_stations == TRUE) mutate(., Cle = fct_reorder(factor(Cle), -chsta_distancesource)) else .} %>%
  # Réorganisation
  pivot_wider(id_cols = c(chmes_date, chmes_heure), names_from = Cle, values_from = chmes_valeur, names_sort = T)

#### Export ####
## Dataframe vers R
  return(mesures_regroupees)

## Export vers xlsx ##
if(export == TRUE & !is.na(projet)){
  openxlsx::write.xlsx(mesures_regroupees, file = glue("./{projet}/Sorties/Données/Agrégations_diverses/Données_regroupées_toutes_stations_tous_paramètres.xlsx"))
}
  
if(export == TRUE & is.na(projet)){
  openxlsx::write.xlsx(mesures_regroupees, file = glue("./Données_regroupées_toutes_stations_tous_paramètres.xlsx"))
}

} # Fin de la fonction
