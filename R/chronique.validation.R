#' Validation des chroniques
#'
#' Cette fonction permet d'obtenir des éléments afin de valider les chroniques
#' 
#' @param data Chronique à valider
#' @param ValMax Valeur maximale dont les dépassements sont pris en compte
#' @param ValEcart Écart entre deux valeurs successives dont les dépassements sont pris en compte
#' @keywords 
#' @import dplyr DBI RSQLite
#' @export
#' @examples
#' chronique.validation(data)
#' chronique.validation(data, 21)
#' chronique.validation(data, 21, 1.5)

##### TODO LIST #####
# La valeur d'écart de référence (ici 1 heure) devrait être personnalisable
# Améliorer l'affichage des valeurs supérieures ou égales à une valeur donnée en changeant le format tbl_df vers dataframe pê ?
# Changer titre journées complètes et Complétude des jours et la première valeur à s'afficher
# Créer un nouveau test qui affiche les valeurs NA
#####################

chronique.validation <- function(data, ValMax = 21, ValEcart = 1)
{
  # Transformation du format de date et calcul des écarts
  data <-
    data %>% 
    mutate(DateFine = ymd_hms(paste(Date, Heure, sep = "_"))) %>% 
    arrange(DateFine) %>% 
    mutate(Difference = DateFine - lag(DateFine)) # calcule l'écart de temps entre une valeur et la valeur précédente
  
  # Écarts différents de 1 heure
  "Écarts différents de 1 heure :"
  a <-
    data %>% 
    filter(Difference != 1 | is.na(Difference))
  
  # Complétude des jours
  verif <-
    data %>% 
    complete(Date, Heure) %>% 
    filter(is.na(Valeur) == T)
  if(length(levels(as.factor(verif$Date))) > 2) print("Vérification nécessaire") else print("Seulement 2 journées incomplètes")
  "Complétude des jours :"
  b <- 
    levels(as.factor(verif$Date)) # faire la différence entre ce vecteur et les min et max de ensemble
  
  # Nombre de jours cohérent avec les dates minimales et maximales (au cas où il manque une journée complète) #
  if(max(data$Date) - min(data$Date) == nlevels(as.factor(data$Date)) - 1) c <- "Pas de journée complète manquante" else c <- "Journée complète manquante"

  # Sélection des données supérieures à une valeur donnée ValeurMax #
  paste("Valeurs supérieures ou égales à",ValMax)
  d <- 
    data %>% 
    filter(Valeur >= ValMax) %>% 
    arrange(desc(Valeur)) # %>% 
    #head(25)
  
  # Recherche des valeurs différentes ValEcart avec la précédente #
  paste("Écart entre deux valeurs supérieur à",ValEcart)
  e <- 
    data %>% 
    mutate(diff = Valeur - lag(Valeur)) %>% 
    filter(abs(diff) > ValEcart) %>% 
    arrange(desc(abs(diff))) %>% 
    arrange(Date)
  
  output <- list("Écarts différents de 1 heure :",a,
                 "Complétude des jours :",b,
                 "Journées complètes :",c,
                 paste("Valeurs supérieures ou égales à",ValMax,":"),d,
                 paste("Écart entre deux valeurs supérieur à",ValEcart,":"),e)
return(output)
  
} # Fin de la fonction