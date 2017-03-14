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
  # 
  # 
  # -------------- A FAIRE -------------- #

  
  ##### Mise au format des données #####

  ##### Calcul de la tendance #####
Analyse <-
  data %>% 
  mutate(Datefine = ymd_hms(paste(Date,Heure,"_"))) %>% 
  arrange(desc(CodeRDT,Datefine)) %>% 
  mutate(Difference = lead(Valeur) - Valeur) %>% 
  group_by(CodeRDT) %>%
  top_n(n = N, wt = Datefine) %>% 
  #summarise(Moy = mean(Difference, na.rm = T)) %>% Recherche de la moyenne des valeurs de différence -> moins juste
  summarise(Difference = first(Difference), # Recherche de la première valeur de différence
            Valeur = last(Valeur), # Affichage de la dernière valeur mesurée
            DateValeur = last(Date), # Date de la dernière valeur mesurée
            HeureValeur = as.character(last(Heure)), # Heure de la dernière valeur mesurée
            DateDifference = first(Date), # Date de la dernière valeur de différence valide
            HeureDifference = as.character(first(Heure))) %>% # Heure de la dernière valeur de différence valide
  mutate(n = N) %>% # Affichage du nombre d'itération afin de juger de l'intensité de la tendance
  mutate(DiffSurQ = Difference/Valeur) # Rapport entre la différence de débit et le débit en lui-même (intensité)
  
} # Fin de la fonction