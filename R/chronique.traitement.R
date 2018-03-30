#' Exécuter le traitement de données de chroniques
#'
#' Permet d'exécuter plusieurs opérations de traitement des données de chronique d'une station
#' @keywords chronique
#' @param data Data.frame issu de chronique.mesures, pouvant contenir différentes stations
#' @import DBI
#' @import dplyr
#' @import lubridate 
#' @import RSQLite 
#' @import stringr
#' @export
#' @examples
#' chronique.traitement(data)
#' DataTravail <- chronique.mesures("SUR0-9", "Thermie") %>% chronique.traitement()

chronique.traitement <- function(  
  data = data
  )
{
  
#### Évaluation des choix ####

#### Analyse des données ####
DataTravail <- 
  data %>% 
  formatage.annee.biologique() %>% # Calcul de l'année biologique
  #group_by(AnneeBiol) %>% # Pour supprimer les années biologiques avec moins de 30 dates différentes
  #filter(n_distinct(Date) > 30) %>% # Pour supprimer les années biologiques avec moins de 30 dates différentes
  #ungroup() %>% 
  group_by(CodeRDT, AnneeBiol) %>% 
  filter(n_distinct(Date) > 30) %>% # Pour supprimer les années biologiques avec moins de 30 dates différentes
  do(chronique.analyse(data = .)) %>% # applique la fonction à chaque station pour chaque année
  ungroup()

##### Sorties graphiques #####

##### Sortie #####
return(DataTravail)

} # Fin de la fonction