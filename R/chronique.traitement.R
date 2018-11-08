#' Exécuter le traitement de données de chroniques
#'
#' Permet d'exécuter plusieurs opérations de traitement des données de chronique d'une station
#' @name chronique.traitement
#' @keywords chronique
#' @param data Data.frame issu de chronique.mesures, pouvant contenir différentes stations
#' @param export Si \code{TRUE} (par défault), exporte les résultats/figures.  Si \code{FALSE}, ne les exporte pas.
#' @import DBI
#' @import dplyr
#' @import lubridate 
#' @import rgdal
#' @import RSQLite 
#' @import sp
#' @import stringr
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
  #group_by(AnneeBiol) %>% # Pour supprimer les années biologiques avec moins de 30 dates différentes
  #filter(n_distinct(Date) > 30) %>% # Pour supprimer les années biologiques avec moins de 30 dates différentes
  #ungroup() %>% 
  group_by(CodeRDT, AnneeBiol) %>% 
  filter(n_distinct(Date) > 30) %>% # Pour supprimer les années biologiques avec moins de 30 dates différentes
  do(chronique.analyse(data = .)) %>% # applique la fonction à chaque station pour chaque année
  ungroup()

##### Sorties graphiques #####
## Chronique complète ##
if(export == TRUE){
data %>%
  group_by(CodeRDT, AnneeBiol) %>%
  #do(chronique.figure(data = ., Titre = as.character(unique(unlist(.$CodeRDT))), duree = "Complet", legendeY = "Température (°C)", save=T, format=".png")) %>% # Fonctionne uniquement si une seule année
  do({chronique.figure(data = ., Titre = as.character(paste0(unique(unlist(.$CodeRDT))," - ",unique(unlist(.$AnneeBiol)))), duree = "Complet", legendeY = "Température (°C)", save=T, format=".png") # Fonctionne si plusieurs années
    distinct(., CodeRDT, AnneeBiol) # Pour permettre à la fonction do de sortir un dataframe, sinon erreur
    }) %>%
  ungroup()
}

## Chronique incomplète ##
if(export == TRUE){
data %>%
  group_by(CodeRDT, AnneeBiol) %>%
  #do(chronique.figure(data = ., Titre = as.character(unique(unlist(.$CodeRDT))), duree = "Relatif", legendeY = "Température (°C)", save=T, format=".png")) %>% # Fonctionne uniquement si une seule année
  do({chronique.figure(data = ., Titre = as.character(paste0(unique(unlist(.$CodeRDT))," - ",unique(unlist(.$AnneeBiol)))), duree = "Relatif", legendeY = "Température (°C)", save=T, format=".png") # Fonctionne si plusieurs années
    distinct(., CodeRDT, AnneeBiol) # Pour permettre à la fonction do de sortir un dataframe, sinon erreur
    }) %>%
  ungroup()
}

##### Sorties agrégées #####
if(export == TRUE){
data %>% 
  group_by(CodeRDT) %>% 
  do({chronique.agregation(data = ., export = T)
    distinct(., CodeRDT) # Pour permettre à la fonction do de sortir un dataframe, sinon erreur
    }) %>% # applique la fonction à chaque station pour chaque année
  #purrr::map(chronique.agregation(data = ., export = T)) # applique la fonction à chaque station pour chaque année
  ungroup()
}

##### Sortie stations #####
if(export == TRUE){
listeStations <- data %>% distinct(CodeRDT)
## Connexion à la BDD ##
dbC <- BDD.ouverture(Type = "Chroniques")
listeStations <- tbl(dbC,"Stations") %>% filter(CodeRDT %in% listeStations$CodeRDT) %>% collect() %>% select(CodeRDT:Departement, X:TypeCoord, Fonctionnement:ReseauThermie)

## Excel ##
openxlsx::write.xlsx(Stations, file = paste0("./Sorties/", format(now(), format="%Y-%m-%d"), "_Stations.xlsx"))

## Shapefile ##
Stations <- listeStations %>% filter(TypeCoord == "L93")
coordinates(Stations) <- ~X+Y # Permet de créer un SpatialPointsDataFrame à partir d'un dataframe Data contenant les coordonnées dans les colonnes XLambert et YLambert
proj4string(Stations) <- CRS("+init=epsg:2154") # Définition de la projection, ici pour Lambert 93
setCPLConfigOption("SHAPE_ENCODING", "UTF-8") # Afin de définir l'encodage en UT8
writeOGR(Stations, ".", paste0("./Sorties/SIG/", format(now(), format="%Y-%m-%d"), "_Stations"), driver="ESRI Shapefile", layer_options= c(encoding= "UTF-8"), overwrite_layer=T)
}

##### Sortie résultats élaborés #####
if(export == TRUE){
## Excel ##
openxlsx::write.xlsx(DataTravail, file = paste0("./Sorties/", format(now(), format="%Y-%m-%d"), "_Résultats_calculés.xlsx"))
  
## Shapefile ##
Stations <- listeStations %>% filter(TypeCoord == "L93")
DataTravailSIG <- Stations %>% left_join(DataTravail %>% mutate(intervalMax = as.numeric(sub(",", ".", intervalMax))), by = "CodeRDT")
coordinates(DataTravailSIG) <- ~X+Y # Permet de créer un SpatialPointsDataFrame à partir d'un dataframe Data contenant les coordonnées dans les colonnes XLambert et YLambert
proj4string(DataTravailSIG) <- CRS("+init=epsg:2154") # Définition de la projection, ici pour Lambert 93
setCPLConfigOption("SHAPE_ENCODING", "UTF-8") # Afin de définir l'encodage en UT8
writeOGR(DataTravailSIG, ".", paste0("./Sorties/SIG/", format(now(), format="%Y-%m-%d"), "_Résultats"), driver="ESRI Shapefile", layer_options= c(encoding= "UTF-8"), overwrite_layer=T)
}

return(DataTravail)

} # Fin de la fonction