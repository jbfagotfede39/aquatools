#' Nettoyage des données de MI
#'
#' Cette fonction permet de corriger des codes erronés des données de MI
#' 
#' @param data Chronique à valider
#' @keywords MI
#' @import dplyr DBI
#' @export
#' @examples
#' MI.nettoyage(data)

MI.nettoyage <- function(data)
{

  ## Connexion à la BDD ##
db <- BDD.ouverture("Macroinvertébrés")
  
  ## Récupération des données ##
HabitatsReference <- dbReadTable(db, "HabitatsReference")
Prelevements <- dbReadTable(db, "Prelevements")
Captures <- dbReadTable(db, "Captures")

if(all(colnames(data) == colnames(Prelevements))) {
  # Substrats #
data$HabitatAbrevSub[data$HabitatAbrevSub == "BRYO"] <- "BRY"
data$HabitatAbrevSub[data$HabitatAbrevSub == "SUB LIGN"] <- "CHV"
data$HabitatAbrevSub[data$HabitatAbrevSub == "BRC"] <- "CHV"
data$HabitatAbrevSub[data$HabitatAbrevSub == "VAS"] <- "FIN"
data$HabitatAbrevSub <- toupper(data$HabitatAbrevSub) # Pour tout mettre en majuscule
  # Vitesses #
data$HabitatAbrevVit[data$HabitatAbrevVit == "<5"] <- "V1"
data$HabitatAbrevVit[data$HabitatAbrevVit == "5-25"] <- "V3"
data$HabitatAbrevVit[data$HabitatAbrevVit == "25-75"] <- "V5"
data$HabitatAbrevVit[data$HabitatAbrevVit == "75-150"] <- "V4"
data$HabitatAbrevVit[data$HabitatAbrevVit == ">150"] <- "V2"
  # Hauteurs #
data$HabitatAbrevHau[data$HabitatAbrevHau == "<5"] <- "H1"
data$HabitatAbrevHau[data$HabitatAbrevHau == "5-25"] <- "H2"
data$HabitatAbrevHau[data$HabitatAbrevHau == "25-50"] <- "H3"
data$HabitatAbrevHau[data$HabitatAbrevHau == "50-100"] <- "H4"
data$HabitatAbrevHau[data$HabitatAbrevHau == ">100"] <- "H5"
  # Vérif #
if (all(data$HabitatAbrevSub %in% HabitatsReference$HabitatAbrev) == FALSE) stop("Erreur avec un code substrat")
if (all(data$HabitatAbrevVit %in% HabitatsReference$HabitatAbrev) == FALSE) stop("Erreur avec un code vitesse")
if (all(data$HabitatAbrevHau %in% HabitatsReference$HabitatAbrev) == FALSE) stop("Erreur avec un code hauteur")
}

  return(data)
  
} # Fin de la fonction