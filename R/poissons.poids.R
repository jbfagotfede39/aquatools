#' Calcul des poids attendus en fonction de la taille pour un jeu de captures
#'
#' Calcule, à partir d'une jeu de données de captures, les poids attendus pour des tailles données
#' @name poissons.poids
#' @keywords donnees
#' @import FSA
#' @import dplyr
#' @export
#' @examples
#' poissons.poids(Captures)

# @import plyr

poissons.poids <- function(  
  Captures=Captures)
{
  
  #library("FSA");library(car);library(magrittr);library("dplyr")
  
##### Importation-Transformation des données #####
Captures <-
  Captures %>% 
  #filter(CodeEspece == "TRF") %>% 
  #filter(Nombre == 1) %>% 
  dplyr::rename(tl = TailleMaximum) %>% 
  dplyr::rename(wt = Poids) %>% 
  dplyr::mutate(logW=log10(wt),logL=log10(tl))
  
##### Modélisation ######
fit1 <- lm(logW~logL,data=Captures)

###### Tableau de prédiction #####
limites <- range(Captures$tl)
limites[1] <- round_any(limites[1], 25, floor)
limites[2] <- round_any(limites[2], 25, ceiling)

lens <- seq(from = limites[1], by = 25, length = (limites[2]-limites[1])/25+1)
  
nd <- data.frame(logL=log10(lens))  # df of log(lengths)
plogW <- predict(fit1,nd,interval="prediction") # Permet d'obtenir l'enveloppe afin de deviner de futures valeurs
cf <- FSA::logbtcf(fit1,10) # correction factor

estimations <- data.frame(lens, cf*10^plogW) # Dataframe contenant les poids pour une longueur donnée, avec l'intervalle de confiance à 95 %
estimations <-
  estimations %>% 
  dplyr::mutate(fit= round(fit,1)) %>% 
  dplyr::mutate(lwr= round(lwr,1)) %>% 
  dplyr::mutate(upr= round(upr,1)) %>% 
  dplyr::rename('Taille (mm)' = lens) %>% 
  dplyr::rename('Poids estimé (g)'= fit) %>% 
  dplyr::rename('Poids minimum à 95% (g)'= lwr) %>%
  dplyr::rename('Poids maximum à 95% (g)'= upr) 

return(estimations)
} # Fin de la fonction