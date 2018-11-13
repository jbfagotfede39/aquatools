#' Exécuter le traitement de données de chroniques
#'
#' Permet d'exécuter plusieurs opérations de traitement des données de chronique d'une station
#' @name chronique.traitement
#' @keywords chronique
#' @param data Data.frame issu de chronique.mesures, pouvant contenir différentes stations
#' @param export Si \code{TRUE} (par défault), exporte les résultats/figures.  Si \code{FALSE}, ne les exporte pas.
#' @import dplyr
#' @import lubridate 
#' @import sf
#' @import stringr
#' @import tidyverse
#' @export
#' @examples
#' chronique.traitement(data)
#' chronique.traitement(data, export = F)
#' DataTravail <- chronique.mesures("SUR0-9", "Thermie") %>% chronique.traitement()
#' DataTravail <- chronique.mesures("SUR0-9", "Thermie") %>% chronique.traitement(., export = F)
#' DataTravail <- tbl(db,"Mesures") %>% filter(CodeRDT == "DOU393-2" | CodeRDT == "DOU394-5") %>% collect() %>% chronique.traitement()

chronique.traitement <- function(  
  data = data,
  export = T
  )
{

##### -------------- A FAIRE -------------- #####
# Export lexique
# Créer un applet qui demande le nom du projet et qui nomme ensuite en conséquence les fichiers et le répertoire principal
# -------------- A FAIRE -------------- #  

#### Évaluation des choix ####

#### Vérification des répertoires ####
if(export == TRUE){
  if(file.exists("./Sorties/") == FALSE){
  dir.create("./Sorties/", showWarnings = FALSE, recursive = FALSE)
  dir.create("./Sorties/Vues/", showWarnings = FALSE, recursive = FALSE)
  dir.create("./Sorties/Données/", showWarnings = FALSE, recursive = FALSE)
}

if(file.exists("./Sorties/") == TRUE & file.exists("./Sorties/Données/") == FALSE){
    dir.create("./Sorties/Données/", showWarnings = FALSE, recursive = FALSE)
}

if(file.exists("./Sorties/") == TRUE & file.exists("./Sorties/SIG/") == FALSE){
  dir.create("./Sorties/SIG/", showWarnings = FALSE, recursive = FALSE)
}
  
if(file.exists("./Sorties/") == TRUE & file.exists("./Sorties/Vues/") == FALSE){
  dir.create("./Sorties/Vues/", showWarnings = FALSE, recursive = FALSE)
}
}

#### Préparation des données ####
data <-
  data %>% 
  formatage.annee.biologique() # Calcul de l'année biologique

#### Analyse des données ####
DataTravail <- 
  data %>% 
  #group_by(chmes_anneebiol) %>% # Pour supprimer les années biologiques avec moins de 30 dates différentes
  #filter(n_distinct(chmes_date) > 30) %>% # Pour supprimer les années biologiques avec moins de 30 dates différentes
  #ungroup() %>% 
  group_by(chmes_coderhj, chmes_anneebiol) %>% 
  filter(n_distinct(chmes_date) > 30) %>% # Pour supprimer les années biologiques avec moins de 30 dates différentes
  do(chronique.analyse(data = .)) %>% # applique la fonction à chaque station pour chaque année
  ungroup()

##### Sorties graphiques #####
## Chronique complète ##
if(export == TRUE){
data %>%
  group_by(chmes_coderhj, chmes_anneebiol) %>%
  #do(chronique.figure(data = ., Titre = as.character(unique(unlist(.$chmes_coderhj))), duree = "Complet", legendeY = "Température (°C)", save=T, format=".png")) %>% # Fonctionne uniquement si une seule année
  do({chronique.figure(data = ., Titre = as.character(paste0(unique(unlist(.$chmes_coderhj))," - ",unique(unlist(.$chmes_anneebiol)))), duree = "Complet", legendeY = "Température (°C)", save=T, format=".png") # Fonctionne si plusieurs années
    distinct(., chmes_coderhj, chmes_anneebiol) # Pour permettre à la fonction do de sortir un dataframe, sinon erreur
    }) %>%
  ungroup()
}

## Chronique incomplète ##
if(export == TRUE){
data %>%
  group_by(chmes_coderhj, chmes_anneebiol) %>%
  #do(chronique.figure(data = ., Titre = as.character(unique(unlist(.$chmes_coderhj))), duree = "Relatif", legendeY = "Température (°C)", save=T, format=".png")) %>% # Fonctionne uniquement si une seule année
  do({chronique.figure(data = ., Titre = as.character(paste0(unique(unlist(.$chmes_coderhj))," - ",unique(unlist(.$chmes_anneebiol)))), duree = "Relatif", legendeY = "Température (°C)", save=T, format=".png") # Fonctionne si plusieurs années
    distinct(., chmes_coderhj, chmes_anneebiol) # Pour permettre à la fonction do de sortir un dataframe, sinon erreur
    }) %>%
  ungroup()
}

##### Sorties agrégées #####
if(export == TRUE){
data %>% 
  group_by(chmes_coderhj) %>% 
  do({chronique.agregation(data = ., export = T)
    distinct(., chmes_coderhj) # Pour permettre à la fonction do de sortir un dataframe, sinon erreur
    }) %>% # applique la fonction à chaque station pour chaque année
  #purrr::map(chronique.agregation(data = ., export = T)) # applique la fonction à chaque station pour chaque année
  ungroup()
}

##### Sortie stations #####
if(export == TRUE){
listeStations <- data %>% distinct(chmes_coderhj)

## Connexion à la BDD ##
dbD <- BDD.ouverture("Data")
listeStations <- sf::st_read(dbD, query = "SELECT * FROM fd_production.chroniques_stations;") %>% filter(chsta_coderhj %in% listeStations$chmes_coderhj) %>% collect() %>% select(chsta_coderhj:chsta_departement, chsta_coord_x:chsta_coord_type, chsta_fonctionnement:chsta_reseauthermietype)

## Excel ##
openxlsx::write.xlsx(listeStations, file = paste0("./Sorties/", format(now(), format="%Y-%m-%d"), "_Stations.xlsx"))

## Shapefile ##
SIG.exportSHP(listeStations, paste0("./Sorties/SIG/", format(now(), format="%Y-%m-%d"), "_Stations.shp"))
}

##### Sortie résultats élaborés #####
if(export == TRUE){
## Excel ##
openxlsx::write.xlsx(DataTravail, file = paste0("./Sorties/", format(now(), format="%Y-%m-%d"), "_Résultats_calculés.xlsx"))
  
## Shapefile ##
DataTravailSIG <- listeStations %>% left_join(DataTravail %>% mutate(intervalMax = as.numeric(sub(",", ".", intervalMax))), by = c("chsta_coderhj" = "chmes_coderhj"))
SIG.exportSHP(DataTravailSIG, paste0("./Sorties/SIG/", format(now(), format="%Y-%m-%d"), "_Résultats.shp"))
}

return(DataTravail)

} # Fin de la fonction