#' Traitement semi-automatisé de données piscicoles
#'
#' Cette fonction permet de traiter de manière semi-automatisée des données piscicoles (histogrammes, graphiques CAA/CAR, etc.)
#' @name poissons.traitement
#' @param data Jeu de données en entrée (captures issues de poissons.captures)
#' @param export \code{FALSE} par défault. Permet d'exporter les données
#' @param commentaires \code{FALSE} par défault. Permet d'extraire le commentaire associé aux opérations
#' @keywords poissons
#' @export
#' @import ggplot2
#' @import tidyverse
#' @examples
#' poissons.traitement(data)
#' poissons.traitement(data, export=T)
#' poissons.captures("AIN18-4") %>% poissons.traitement(export=T)

##### -------------- A FAIRE -------------- #####
# 
# -------------- A FAIRE -------------- #

poissons.traitement <- function(
  data = data,
  export = F,
  commentaires = F
)
{
  
  #### Contexte des données ####
  ## Contexte général ##
  Contexte <- tibble(nStations = n_distinct(data$nom))
  
  Contexte <-
    data %>%
    distinct(nom) %>%
    rename(Nom = nom) %>%
    bind_cols(Contexte)
  
  ## Listing des opérations ##
  listeOperations <- data %>% distinct(nom, datedebut) %>% rename(Nom = nom, Date = datedebut)# %>% mutate(Date = as.character(Date)) # pour n'avoir que les opérations présentes dans le jeu de données envoyé (data)
  
  #### Vérifications générales ####
  if(Contexte$nStations > 1) stop("Différentes stations dans les captures à analyser - Cas à développer")
  
  #### Vérification des répertoires ####
  if(export == TRUE){
    if(file.exists(paste0("./",Contexte$Nom)) == FALSE){
      dir.create(paste0("./",Contexte$Nom), showWarnings = FALSE, recursive = FALSE)
    }
  if(file.exists(paste0("./",Contexte$Nom)) == TRUE){
    dir.create(paste0("./",Contexte$Nom), showWarnings = FALSE, recursive = FALSE)
    dir.create(paste0("./",Contexte$Nom,"/PoissonsCA"), showWarnings = FALSE, recursive = FALSE)
    dir.create(paste0("./",Contexte$Nom,"/PoissonsHistogramme"), showWarnings = FALSE, recursive = FALSE)
    dir.create(paste0("./",Contexte$Nom,"/PoissonsTableau"), showWarnings = FALSE, recursive = FALSE)
    dir.create(paste0("./",Contexte$Nom,"/PoissonsHistogramme/Annee/"), showWarnings = FALSE, recursive = FALSE)
    dir.create(paste0("./",Contexte$Nom,"/PoissonsHistogramme/Annee/Classe/"), showWarnings = FALSE, recursive = FALSE)
    dir.create(paste0("./",Contexte$Nom,"/PoissonsHistogramme/Annee/SansClasse/"), showWarnings = FALSE, recursive = FALSE)
    dir.create(paste0("./",Contexte$Nom,"/PoissonsHistogramme/Interannuelle/"), showWarnings = FALSE, recursive = FALSE)
  }
}
  
  #### Sortie des CA ####
  CA <- poissons.CAA(Contexte$Nom)
  # suppression temporaire le temps qu'Adrien répare mon mail du 2019-11-15_____CAA <- CA %>% filter(grepl("NTT", Annee)) # Afin de ne conserver que les opérations présentes dans le jeu de données de captures initial (data)
  # suppression temporaire le temps qu'Adrien répare mon mail du 2019-11-15_____if(nrow(CAA) == 0)stop("Pas de CAA")
  # suppression temporaire le temps qu'Adrien répare mon mail du 2019-11-15_____CAR <- CA %>% filter(!grepl("NTT", Annee)) %>% filter(Date %in% as.character(listeOperations$Date)) # Afin de ne conserver que les opérations présentes dans le jeu de données de captures initial (data)
  # suppression temporaire le temps qu'Adrien répare mon mail du 2019-11-15_____if(nrow(CAR) == 0)stop("Pas de CAR")
  #CA <- CAA %>% union(CAR) # NE FONCTIONNE PAS LORS D'une utilisation en fonction, mais à la main si ??? Afin de ne conserver que les opérations présentes dans le jeu de données de captures initial (data)
  # suppression temporaire le temps qu'Adrien répare mon mail du 2019-11-15_____CA <- CAA %>% bind_rows(CAR) # Afin de ne conserver que les opérations présentes dans le jeu de données de captures initial (data)
  if(nrow(CA) == 0)stop("Jointure de CAA et de CAR vide")
  if(export == T){CA %>% poissons.CAAvue(export = T)}
  if(export == F){CAAvue <- CA %>% poissons.CAAvue(export = F)}
  
  #### Sortie des histogrammes ####
  ### Histogramme annuel ###
  data %>%
    group_split(codeespece, year(datedebut)) %>% 
    purrr::map_dfr(~ poissons.histogramme(., export = T))
  
  ### Histogramme inter-annuel ###
  data %>% 
    group_split(codeespece) %>% 
    purrr::map_dfr(~ poissons.histogramme(., export = T))
  
  #### Sortie des données excel et pdf ####
  ## Déplacement dans le bon répertoire ##
  setwd(paste0("./",Contexte$Nom,"/PoissonsTableau"))

  ## Exportation propre ##
  # Fichier pdf
  if(commentaires == FALSE){
    purrr::map2(as.list(listeOperations$Nom), as.list(listeOperations$Date), poissons.fiche, commentaires = F) # Fonctionne
  }
  
  if(commentaires == TRUE){
    purrr::map2(as.list(listeOperations$Nom), as.list(listeOperations$Date), poissons.fiche, commentaires = T) # Ne fonctionne pas à cause d'un cat ?
    }
  
  # Fichiers excel
  purrr::map2(as.list(listeOperations$Nom), as.list(listeOperations$Date), poissons.brut) # Fonctionne 
  #purrr::map2(as.list(listeOperations$Nom), as.list(listeOperations$Date), ~poissons.brut(.x,.y)) # Fonctionne également
  
  #### Remise du répertoire courant ####
  setwd("..") # On remonte dans le répertoire de la station
  setwd("..") # On remonte au répertoire courant initial
  
} # Fin de la fonction
