#' Saturation en oxygène
#'
#' Cette fonction permet de calculer des saturations en oxygène à partir des chroniques de mesures de concentration et de température de l'eau
#' @name PC.saturationO2
#' @param data Jeu de données (contenant une colonne Thermie et une colonne Concentration)
#' @keywords Physico-chimie
#' @import tidyverse
#' @export
#' @examples
#' PC.saturationO2(data)
#' dataaimporter %>% PC.saturationO2()
#' # Calcul à partir de https://mdm.sandre.eaufrance.fr/node/414781

PC.saturationO2 <- function(data)
{
  # Calcul
  data <- 
    data %>% 
    mutate(O2sat = 14.64 - 0.4227*Thermie + 0.009937*Thermie^2 - 0.0001575*Thermie^3 + 0.000001125*Thermie^4) %>% 
    mutate(Saturation = Concentration/O2sat*100) %>% 
    dplyr::select(-O2sat)
  
  return(data)
  
} # Fin de la fonction