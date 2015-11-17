#' Détection des données aberrantes
#'
#' Cette fonction est une aide à la détection de données aberrantes dans une chronique
#' @param data
#' @keywords Données
#' @export
#' @import dplyr
#' @examples
#' donnees.aberrantes(data)

##### -------------- A FAIRE -------------- #####

# Recherche si nécessaire des lignes type "Enr" ?
# Recherche des valeurs non numériques ?
# Recherche des données journalières différentes de 24 ?

##### -------------- A FAIRE -------------- #####

donnees.aberrantes <- function(data){
  
  #data %>%
  #  filter(Temp,contains("Enr")) # recherche des "Enregistré" : NE FONCTIONNE PAS
  
  data %>%
    filter(!complete.cases(data)) # recherche des NA

  #"filter(synthese, Heure == "02:00:00" & Temp == 10.259)"
  
}
