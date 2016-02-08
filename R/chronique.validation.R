#' Validation des chroniques
#'
#' Cette fonction permet d'obtenir des éléments afin de valider les chroniques
#' 
#' @param data Chronique à valider
#' @keywords 
#' @import dplyr DBI RSQLite
#' @export
#' @examples
#' chronique.validation(data)

##### TODO LIST #####
# 
#####################

chronique.validation <- function(data)
{
  data <-
    data %>% 
    mutate(DateFine = ymd_hms(paste(Date, Heure, sep = "_"))) %>% 
    arrange(DateFine) %>% 
    mutate(Difference = DateFine - lag(DateFine)) # calcule l'écart de temps entre une valeur et la valeur précédente
  
  data %>% 
    filter(Difference != 1, Validation != "Rejeté") 
  
  verif <-
    data %>% 
    complete(Date, Heure) %>% 
    filter(is.na(Niveau) == T)
  if(length(levels(as.factor(verif$Date))) > 2) print("Vérification nécessaire") else print("Seulement 2 journées incomplètes")
  levels(as.factor(verif$Date)) # faire la différence entre ce vecteur et les min et max de ensemble
  
} # Fin de la fonction