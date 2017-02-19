#' Tendance d'évolution d'une chroniques
#'
#' Cette fonction permet d'analyser les tendances d'évolution de chroniques de mesures (température, niveaux, etc.)
#' 
#' @param data Jeu de données
#' @param N 
#' @keywords chronique
#' @import tidyverse
#' @import lubridate
#' @export
#' @examples
#' chronique.tendance(data)
#' chronique.tendance(data, N=5)

chronique.tendance <- function(
  data = data,
  N=2
)
{
  
  ##### -------------- A FAIRE -------------- #####
  # Ajout de la dernière valeur afin de pouvoir la comparer à une valeur de référence
  # 
  # -------------- A FAIRE -------------- #

  
  ##### Mise au format des données #####

  ##### Calcul de la tendance #####
Analyse <-
  data %>% 
  mutate(Datefine = ymd_hms(paste(Date,Heure,"_"))) %>% 
  arrange(desc(CodeRDT,Datefine)) %>% 
  mutate(Difference = Valeur - lead(Valeur)) %>% 
  group_by(CodeRDT) %>% 
  top_n(n = N, wt = Datefine) %>% 
  summarise(Moy = mean(Difference)) %>% 
  mutate(n = N)
  
} # Fin de la fonction