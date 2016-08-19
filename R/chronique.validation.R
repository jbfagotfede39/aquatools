#' Validation des chroniques
#'
#' Cette fonction permet d'obtenir des éléments afin de valider les chroniques
#' 
#' @param data Chronique à valider (contenant un champ date défini en tant que tel)
#' @param ValMax Valeur maximale dont les dépassements sont pris en compte
#' @param ValMin Valeur maximale dont les dépassements sont pris en compte
#' @param ValEcart Écart entre deux valeurs successives dont les dépassements sont pris en compte
#' @param TempsDiff Différence de temps attendue entre 2 valeurs successives (1 par défault)
#' @keywords 
#' @import dplyr DBI RSQLite
#' @export
#' @examples
#' chronique.validation(data)
#' chronique.validation(data, 21)
#' chronique.validation(data, 21, 0, 1.5)
#' chronique.validation(data, 21, 0, 1.5, 3600)
#' chronique.validation(DataToAdd, ValMax = 1080, ValMin = 1040, ValEcart = 10, TempsDiff = 4)

##### TODO LIST #####
# Améliorer l'affichage des valeurs supérieures ou égales à une valeur donnée en changeant le format tbl_df vers dataframe pê ?
# Changer titre journées complètes et Complétude des jours et la première valeur à s'afficher
# Créer un nouveau test qui affiche les valeurs NA
#####################

chronique.validation <- function(data, ValMax = 21, ValMin = 0, ValEcart = 1, TempsDiff = 1)
{
  # Transformation du format de date et calcul des écarts
  data <-
    data %>% 
    mutate(DateFine = ymd_hms(paste(Date, Heure, sep = "_"))) %>% 
    arrange(DateFine) %>% 
    mutate(Difference = DateFine - lag(DateFine)) # calcule l'écart de temps entre une valeur et la valeur précédente
  
  # Écarts différents de 1 heure (ou autre)
  paste("Écarts différents de", TempsDiff, "heure(s) :")
  a <-
    data %>% 
    filter(Difference != TempsDiff | is.na(Difference))
  
  # Complétude des jours
  verif <-
    data %>% 
    complete(Date, Heure) %>% 
    filter(is.na(Valeur) == T)
  if(length(levels(as.factor(verif$Date))) > 2) print("Plus de 2 journées incomplètes") else print("Seulement 2 journées incomplètes")
  "Complétude des jours :"
  b <- 
    levels(as.factor(verif$Date)) # faire la différence entre ce vecteur et les min et max de ensemble
  
  # Nombre de jours cohérent avec les dates minimales et maximales (au cas où il manque une journée complète) #
  if (class(data$Date) != "Date") data$Date <- ymd(data$Date)
  if(max(data$Date) - min(data$Date) == nlevels(as.factor(data$Date)) - 1) c <- "Pas de journée complète manquante" else c <- "Journée complète manquante"

  # Sélection des données supérieures à une valeur donnée ValeurMax #
  paste("Valeurs supérieures ou égales à ",ValMax)
  d <- 
    data %>% 
    filter(Valeur >= ValMax) %>% 
    arrange(desc(Valeur)) # %>% 
    #head(25)
  
  # Sélection des données inférieures à une valeur donnée ValeurMin #
  paste("Valeurs inférieures ou égales à",ValMin)
  e <- 
    data %>% 
    filter(Valeur <= ValMin) %>% 
    arrange(Valeur) # %>% 
  #head(25)
  
  # Recherche des valeurs différentes ValEcart avec la précédente #
  paste("Écart entre deux valeurs supérieur à",ValEcart)
  f <- 
    data %>% 
    mutate(diff = Valeur - lag(Valeur)) %>% 
    filter(abs(diff) > ValEcart) %>% 
    arrange(desc(abs(diff))) %>% 
    arrange(Date)
  
  output <- list(paste("Écarts différents de", TempsDiff, " heure(s) :"),a,
                 "Complétude des jours :",b,
                 "Journées complètes :",c,
                 paste("Valeurs supérieures ou égales à",ValMax,":"),d,
                 paste("Valeurs inférieures ou égales à",ValMin,":"),e,
                 paste("Écart entre deux valeurs supérieur à",ValEcart,":"),f)
return(output)
  
} # Fin de la fonction