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
#' DataTravail <- tbl(db,"Mesures") %>% filter(CodeRDT == "DOU393-2" | CodeRDT == "DOU394-5") %>% collect() %>% chronique.traitement()

chronique.traitement <- function(  
  data = data
  )
{

##### -------------- A FAIRE -------------- #####
# PARVENIR À faire fonctionner tout à la suite car ce n'est actuellement pas le cas
# Voir pour faire disparaître le message d'erreur lors de l'export des figures de plusieurs stations
# Voir pour faire disparaître le message d'erreur lors de l'export des données de plusieurs stations
# Ajouter export données synthèse stationnelle
# Export lexique
# Faire pour que les différents fichiers se rangent tout seuls dans des répertoires propres (par année par station ou en vrac) : créer une convention de nommage
# -------------- A FAIRE -------------- #  

#### Évaluation des choix ####

  
#### Préparation des données ####
data <-
  data %>% 
  formatage.annee.biologique() # Calcul de l'année biologique

#### Analyse des données ####
DataTravail <- 
  data %>% 
  #group_by(AnneeBiol) %>% # Pour supprimer les années biologiques avec moins de 30 dates différentes
  #filter(n_distinct(Date) > 30) %>% # Pour supprimer les années biologiques avec moins de 30 dates différentes
  #ungroup() %>% 
  group_by(CodeRDT, AnneeBiol) %>% 
  filter(n_distinct(Date) > 30) %>% # Pour supprimer les années biologiques avec moins de 30 dates différentes
  do(chronique.analyse(data = .)) %>% # applique la fonction à chaque station pour chaque année
  ungroup()

##### Sorties graphiques #####
data %>%
  group_by(CodeRDT, AnneeBiol) %>%
  do(chronique.figure(data = ., Titre = as.character(unique(unlist(.$CodeRDT))), duree = "Complet", legendeY = "Température (°C)", save=T, format=".png")) %>% 
  ungroup()

# data %>%
#   group_by(CodeRDT, AnneeBiol) %>%
#   do(chronique.figure(data = ., Titre = as.character(unique(unlist(.$CodeRDT))), duree = "Relatif", legendeY = "Température (°C)", save=T, format=".png")) %>% 
#   ungroup()

##### Sorties agrégées #####
data %>% 
  group_by(CodeRDT) %>% 
  do(chronique.agregation(data = ., export = T)) %>% # applique la fonction à chaque station pour chaque année
  #purrr::map(chronique.agregation(data = ., export = T)) # applique la fonction à chaque station pour chaque année
  ungroup()

##### Sortie #####
return(DataTravail)

} # Fin de la fonction