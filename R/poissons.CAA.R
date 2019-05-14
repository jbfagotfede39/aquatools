#' Collecte des données de CAA et CAR pour une station donnée
#'
#' Cette fonction permet de synthétiser les données de cotes d'abondance attendues et observées pour une station donnée
#' @name poissons.CAA
#' @keywords poissons
#' @param station Code RHJ de la station ("MAD6-2")
#' @param NTT NTT de la station, au format numérique (5 par exemple)
#' @export
#' @import tidyverse
#' @examples
#' poissons.CAA("MAD6-2", 5)

##### -------------- A FAIRE -------------- #####
# Intégrer dans une chaîne cette fonction et poissons.CAA au sein de poissons.exportation
# -------------- A FAIRE -------------- #

poissons.CAA <- function(
  station = as.character(NA),
  NTT = as.numeric(NA))
  {
  
  ## Vérifications ##
  if(is.na(station)) stop("Aucune station saisie")
  if(is.na(NTT)) stop("Aucun NTT saisi")
  
  ## Transformations de format ##
  codeRHJ <- stations.coderhj(as.data.frame(station) %>% rename(CodeRDT = station), DistSource = F) %>% select(codemilieu) %>% as.character()

  ## Import CAA REF ##
  BddNTT <- read_excel(adresse.switch("NAS-DATA/Poissons/BDDNTT.xlsx")) %>%
    rename(coderhj = RDT) %>% 
    rename(codeespece = ESP) %>% 
    rename(typetheorique = NTT) %>%
    mutate(date = "Référence") %>%
    select(date,codeespece,coderhj,typetheorique,CA) %>%
    filter(coderhj == codeRHJ) %>%
    filter(typetheorique == NTT)
  
  ## Import CAR observées ##
  Resultats <- poissons.resultats.BDD() %>% 
    select(nom, datedebut.x, codeespece, coderdt,coteabondancenumerique,coteabondanceponderale,typetheorique) %>%
    arrange(nom, datedebut.x, codeespece) %>% 
    rename(date = datedebut.x) %>% 
    rename(coderhj = coderdt) %>% 
    rowwise() %>% # Pour grouper les données par ligne pour avoir le min
    mutate(CA = min(coteabondancenumerique, coteabondanceponderale)) %>% 
    ungroup() %>% 
    mutate(date = as.character(date)) %>% 
    filter(nom == station) %>%  # Pour ne conserver que la station qui nous intéresse
    select(codeespece, date, coderhj, typetheorique,CA)
  
  ## Regroupement des données attendues et observées ##
  Resultatsvue <- 
    Resultats %>%
    #union(BddNTT %>% select(codeespece, date, coderhj:CA)) # Engendre ensuite une sortie de la fonction au format liste, alors que ça fonctionne parfaitement en test
    bind_rows(BddNTT %>% select(codeespece, date, coderhj:CA)) %>% 
    mutate(station = station) %>% 
    mutate(CA = ifelse(codeespece == "OCL", 0, CA)) %>% 
    mutate(CA = ifelse(codeespece == "PFL", 0, CA)) %>% 
    mutate(CA = ifelse(codeespece == "APP", 0, CA))
  
  ## Sortie ##
  #return(Resultats) # Ok
  return(Resultatsvue)
  #return(as.data.frame(Resultatsvue))
  
} # Fin de la fonction