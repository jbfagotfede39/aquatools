#' Nettoyage des données de MI
#'
#' Cette fonction permet de corriger des codes erronés des données de MI
#' 
#' @param data Chronique à valider
#' @keywords MI
#' @import dplyr DBI stringr
#' @export
#' @examples
#' MI.nettoyage(data)

###### À faire #####
# Ré-écriture de noms de taxons qui n'existent plus, avec ancien nom indiqué en remarque
####################

MI.nettoyage <- function(data)
{

  ## Connexion à la BDD ##
db <- BDD.ouverture("Macroinvertébrés")
  
  ## Récupération des données ##
HabitatsReference <- dbReadTable(db, "HabitatsReference")
Prelevements <- dbReadTable(db, "Prelevements")
Captures <- dbReadTable(db, "Captures")

# Travail sur les prélèvements #
if(all(colnames(data) %in% colnames(Prelevements))) {
  # Substrats #
data$HabitatAbrevSub[data$HabitatAbrevSub == "BRYO"] <- "BRY"
data$HabitatAbrevSub[data$HabitatAbrevSub == "SUB LIGN"] <- "CHV"
data$HabitatAbrevSub[data$HabitatAbrevSub == "BRC"] <- "CHV"
data$HabitatAbrevSub[data$HabitatAbrevSub == "VAS"] <- "FIN"
data$HabitatAbrevSub <- toupper(data$HabitatAbrevSub) # Pour tout mettre en majuscule
data$HabitatAbrevSub <- str_trim(data$HabitatAbrevSub) # Pour enlever les espaces de début et de fin
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

# Travail sur les captures #
if(all(colnames(data) %in% colnames(Captures))) {
  
  # Nettoyage des taxons sans effectif #
  data <- 
    data %>% 
    filter(!is.na(Effectif))
  
  # Ajout du stade larvaire si non ajouté #
  data$Stade[is.na(data$Stade)] <- "Larve"
  
  # Nettoyage des fautes de saisie #
  data$Taxon <- str_trim(data$Taxon) # Pour enlever les espaces de début et de fin de taxon
  data$Taxon[data$Taxon == "Glossossoma"] <- "Glossosoma"
  data$Taxon[data$Taxon == "Elodes"] <- "Helodes"
  data$Taxon[data$Taxon == "Onichogomphus"] <- "Onychogomphus"
  data$Taxon[data$Taxon == "Psychomia"] <- "Psychomyia"
  data$Taxon[data$Taxon == "Bythinia"] <- "Bithynia"
  data$Taxon[data$Taxon == "Procleon"] <- "Procloeon"
  data$Taxon[data$Taxon == "Cordulegastridae"] <- "Cordulegasteridae"
  data$Taxon[data$Taxon == "Polymitarcidae"] <- "Polymitarcyidae"
  data$Taxon[data$Taxon == "Hydracariens"] <- "Hydracarina"
  
}

  return(data)
  
} # Fin de la fonction