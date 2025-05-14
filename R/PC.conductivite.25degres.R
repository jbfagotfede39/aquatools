#' Conductivité corrigée à 25°C
#'
#' Cette fonction permet de corriger (correction de température non linéaire) des conductivité vers des conductivités corrigées à 25°C au sens des normes ISO/DIN 7888 ou AFNOR NF en 2788
#' @name PC.conductivite.25degres
#' @param data Jeu de données (contenant une colonne Thermie et une colonne Conductivité)
#' @keywords Physico-chimie
#' @import tidyverse
#' @export
#' @examples
#' PC.conductivite.25degres(data)
#' dataaimporter %>% PC.conductivite.25degres()

PC.conductivite.25degres <- function(data)
{
  #### Données de référence ####
  data(facteur_conversion_conductivite)
  
  #### Calcul ####
  data_v2 <- 
    data %>% 
    mutate(Thermie_arrondie = as.character(round(Thermie / 0.1) * 0.1), .after = "Thermie") %>% 
    left_join(facteur_conversion_conductivite %>% 
                mutate(Thermie = as.character(Thermie)), 
              join_by(Thermie_arrondie == "Thermie")) %>% 
    mutate(Conductivité_25_degrés = round(as.numeric(Conductivité) * coefficient, 0)) %>%
    dplyr::select(-coefficient)
  
  #### Sortie ####
  return(data_v2)
  
} # Fin de la fonction