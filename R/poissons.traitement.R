#' Traitement semi-automatisé de données piscicoles
#'
#' Cette fonction permet de traiter de manière semi-automatisée des données piscicoles (histogrammes, graphiques CAA/CAR, etc.)
#' @name poissons.traitement
#' @keywords poissons
#' @export
#' @import ggplot2
#' @examples
#' poissons.traitement(data)
#' poissons.traitement(data, export=T)
#' poissons.captures("AIN18-4") %>% poissons.traitement(export=T)

##### -------------- A FAIRE -------------- #####
# Intégrer dans une chaîne cette fonction et poissons.CAA au sein de poissons.exportation
# -------------- A FAIRE -------------- #

poissons.traitement <- function(
  data = data,
  export = F
)
{
  
  #### Contexte des données ####
  Contexte <- tibble(nStations = n_distinct(data$nom))
if(Contexte$nStations > 1) stop("Différentes stations dans les captures à analyser - Cas à développer")
  Contexte <- 
    data %>% 
    distinct(nom) %>% 
    bind_cols(Contexte)
  
  #### Vérification des répertoires ####
  if(export == TRUE){
    if(file.exists(paste0("./",Contexte$nom)) == FALSE){
      dir.create(paste0("./",Contexte$nom), showWarnings = FALSE, recursive = FALSE)
    }
  if(file.exists(paste0("./",Contexte$nom)) == TRUE){
    dir.create(paste0("./",Contexte$nom), showWarnings = FALSE, recursive = FALSE)
    dir.create(paste0("./",Contexte$nom,"/PoissonsCA"), showWarnings = FALSE, recursive = FALSE)
    dir.create(paste0("./",Contexte$nom,"/PoissonsHistogramme"), showWarnings = FALSE, recursive = FALSE)
    dir.create(paste0("./",Contexte$nom,"/PoissonsTableau"), showWarnings = FALSE, recursive = FALSE)
    dir.create(paste0("./",Contexte$nom,"/PoissonsHistogramme/Annee/"), showWarnings = FALSE, recursive = FALSE)
    dir.create(paste0("./",Contexte$nom,"/PoissonsHistogramme/Annee/Classe/"), showWarnings = FALSE, recursive = FALSE)
    dir.create(paste0("./",Contexte$nom,"/PoissonsHistogramme/Annee/SansClasse/"), showWarnings = FALSE, recursive = FALSE)
    dir.create(paste0("./",Contexte$nom,"/PoissonsHistogramme/Interannuelle/"), showWarnings = FALSE, recursive = FALSE)
  }
}
  
  #### Collecte des données #####
  
  #### Sortie des CA ####
  CAA <- poissons.CAA(Contexte$nom)
  if(export == T){CAA %>% poissons.CAAvue(export = T)}
  if(export == F){CAAvue <- CAA %>% poissons.CAAvue(export = F)}
  
  #### Sortie des histogrammes ####
  
  ####### Histogramme annuelle
  data %>%
    group_split(codeespece, year(datedebut)) %>% 
    purrr::map_dfr(~ poissons.histogramme(., export = T)) 
  
  ####### Histogramme interannuelle
  data %>% 
    group_split(codeespece) %>% 
    purrr::map_dfr(~ poissons.histogramme(., export = T))
  
} # Fin de la fonction