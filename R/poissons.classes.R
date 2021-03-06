#' Calcul de classes de taille piscicoles
#'
#' Cette fonction permet de calculer les classes de taille des espèces piscicoles
#' @name poissons.classes
#' @param Captures Dataframe contenant les captures
#' @keywords poissons
#' @import tidyverse
#' @export
#' @examples
#' poissons.classes(Captures)
#' Captures <- poissons.classes(Captures)

##### TODO LIST #####
# Pour essai :
# Station <- "CTR1-0";Date <- "2019-10-02"
# Captures <- poissons.captures(Station, Date)
# 
#####################

poissons.classes <- function(
  Captures = Captures
)
{

  #### Calcul des classes de taille ####
  Captures <-
    Captures %>% 
    mutate(classetaille = case_when(.$taillemoy < 100 ~ "< 100",
                                    .$taillemoy >= 100 & .$taillemoy < 150 ~ "100 - 150",
                                    .$taillemoy >= 150 & .$taillemoy < 200 ~ "150 - 200",
                                    .$taillemoy >= 200 ~ "200 et +",
                                    TRUE ~ "autre")) %>% 
    mutate(classetaille = ifelse(is.na(taillemoy), NA_character_, classetaille)) # Cas des tailles moyennes vides quand non saisies
  
  #### Sortie des résultats ####
  return(Captures)
  
} # Fin de la fonction