#' Affichage de graphes piscicoles
#'
#' Cette fonction permet de créer des comparaisons entre les espèces attendues et observées. Le jeu de données doit contenir un champ codeespece, un champ CA et un champ date
#' @name poissons.CAAvue
#' @keywords poissons
#' @export
#' @import ggplot2
#' @examples
#' poissons.CAAvue(data)
#' poissons.CAAvue(data, save=T)

##### -------------- A FAIRE -------------- #####
# 
# -------------- A FAIRE -------------- #

poissons.CAAvue <- function(
  data = data,
  save=F,
  format=".png")
  {

  ## Vérifications ##
  station <- data %>% distinct(coderhj) %>% as.character()
  if(length(station) == 0) stop("Aucune station au sein du jeu de données")
  if(length(station) != 1) stop("Plusieurs stations au sein du jeu de données")

  listeStations <- stations.ecosysteme()
  listeCE <- poissons.ecosystemes()
  Resultats <- poissons.resultats.BDD()
  listeInventaire <- poissons.inventaires()
  #######importCA REF###########
  codeRHJ = "MAD"
  Station = "MAD6-2"
  NTT = "5"
  
  BddNTT <- read_excel(adresse.switch("NAS-DATA/Poissons/BDDNTT.xlsx")) %>%
    rename(coderhj = RDT) %>% 
    rename(codeespece = ESP) %>% 
    rename(typetheorique = NTT) %>%
    mutate(date = "Référence") %>%
    select(date,codeespece,coderhj,typetheorique,CA) %>%
    filter(coderhj == codeRHJ) %>%
    filter(typetheorique == NTT)
  
  Resultats <- poissons.resultats.BDD() %>% 
    select(nom, datedebut.x, codeespece, coderdt,coteabondancenumerique,coteabondanceponderale,typetheorique) %>%
    arrange(nom, datedebut.x, codeespece) %>% 
    rename(date = datedebut.x) %>% 
    rename(coderhj = coderdt) %>% 
    rowwise() %>% # Pour grouper les données par ligne pour avoir le min
    mutate(CA = min(coteabondancenumerique, coteabondanceponderale)) 
  
  Resultats$date <- as.character(Resultats$date)
  
  Resultatsvue <- 
    Resultats %>%
    filter(nom == Station) %>%  # Pour ne conserver que la station qui nous intéresse
    select(codeespece, date, coderhj, typetheorique,CA) %>% 
    union(BddNTT)
  
} # Fin de la fonction