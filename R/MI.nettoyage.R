#' Nettoyage des données de MI
#'
#' Cette fonction permet de corriger des codes erronés des données de MI
#' @name MI.nettoyage
#' @param data Chronique à valider
#' @param larve Si \code{FALSE} (par défault), n'ajoute pas le stade larvaire si non complété
#' @keywords MI
#' @import dplyr DBI stringr
#' @export
#' @examples
#' MI.nettoyage(data)
#' MI.nettoyage(data, larve = T)

###### À faire #####
# Ré-écriture de noms de taxons qui n'existent plus, avec ancien nom indiqué en remarque
####################

MI.nettoyage <- function(
  data,
  larve = F)
{

  ## Connexion à la BDD ##
  dbMI <- BDD.ouverture(Type = "Macroinvertébrés")
  
  ## Récupération des données ##
HabitatsReference <- tbl(dbMI,"HabitatsReference") %>% collect(n = Inf)
Habitats <- tbl(dbMI,"Habitats") %>% collect(n = Inf)
Prelevements <- tbl(dbMI,"Prelevements") %>% collect(n = Inf)
Captures <- tbl(dbMI,"Captures") %>% collect(n = Inf)

## Fermeture de la BDD ##
DBI::dbDisconnect(dbMI)

##### Travail sur les habitats #####
if(all(colnames(data) %in% colnames(Habitats))) {
  # DomMarg #
  data$DomMarg[data$DomMarg == "marginal représentatif (M)"] <- "Marginal représentatif"
  data$DomMarg[data$DomMarg == "Marginal"] <- "Marginal représentatif"
  data$DomMarg[data$DomMarg == "dominant (D)"] <- "Dominant"
  data$DomMarg[data$DomMarg == "D"] <- "Dominant"
  data$DomMarg[data$DomMarg == "M"] <- "Marginal représentatif"
}

##### Travail sur les prélèvements #####
if(all(colnames(data) %in% colnames(Prelevements))) {
  # Substrats #
data$HabitatAbrevSub[data$HabitatAbrevSub == "BRYO"] <- "BRY"
data$HabitatAbrevSub[data$HabitatAbrevSub == "ROC"] <- "DAL"
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

##### Travail sur les captures #####
if(all(colnames(data) %in% colnames(Captures))) {
  
  # Nettoyage des taxons sans effectif #
  data <- 
    data %>% 
    filter(!is.na(Abondance))
  
  # Ajout du stade larvaire si non ajouté #
  if(larve == T) data$Stade[is.na(data$Stade)] <- "Larve"
  
  # Nettoyage des fautes de saisie #
  data$Taxon <- str_trim(data$Taxon) # Pour enlever les espaces de début et de fin de taxon
  data$Taxon <- str_to_title(data$Taxon) # Mise en majuscule du premier caractère
  data$Taxon[data$Taxon == "Achete"] <- "Achètes"
  data$Taxon[data$Taxon == "Amphinemoura"] <- "Amphinemura"
  data$Taxon[data$Taxon == "Anthomyiidae"] <- "Anthomyidae"
  data$Taxon[data$Taxon == "Ancylus Fluviatilis"] <- "Ancylus fluviatilis"
  data$Taxon[data$Taxon == "Aplexa Hypnorum"] <- "Aplexa hypnorum"
  data$Taxon[data$Taxon == "Autre Baetidae"] <- "Baetidae"
  data$Taxon[data$Taxon == "Autre Limnephilinae"] <- "Limnephilinae"
  data$Taxon[data$Taxon == "Bathyomphalus Contortus"] <- "Bathyomphalus contortus"
  data$Taxon[data$Taxon == "Blepharecidae"] <- "Blephariceridae"
  data$Taxon[data$Taxon == "Bythinia"] <- "Bithynia"
  data$Taxon[data$Taxon == "Centroptilum Luteolum"] <- "Centroptilum"
  data$Taxon[data$Taxon == "Cloeon"] <- "Cloëon"
  data$Taxon[data$Taxon == "Coléoptère"] <- "Coléoptères"
  data$Taxon[data$Taxon == "Colymbetinae (Ilybius)"] <- "Colymbetinae"
  data$Taxon[data$Taxon == "Conchostracés"] <- "Conchostraca"
  data$Taxon[data$Taxon == "Cordulegastridae"] <- "Cordulegasteridae"
  data$Taxon[data$Taxon == "Diptère"] <- "Diptères"
  data$Taxon[data$Taxon == "Dystiscinae"] <- "Dytiscinae"
  data$Taxon[data$Taxon == "Echdyonurus"] <- "Ecdyonurus"
  data$Taxon[data$Taxon == "Elodes"] <- "Helodes"
  data$Taxon[data$Taxon == "Ephémeroptère"] <- "Éphéméroptères"
  data$Taxon[data$Taxon == "Galba Truncatula"] <- "Galba truncatula"
  data$Taxon[data$Taxon == "Gastéropode"] <- "Gastéropodes"
  data$Taxon[data$Taxon == "Glossossoma"] <- "Glossosoma"
  data$Taxon[data$Taxon == "Haementeria Costata"] <- "Haementeria costata"
  data$Taxon[data$Taxon == "Haprophlebia"] <- "Habrophlebia"
  data$Taxon[data$Taxon == "Hydracarien"] <- "Hydracarina"
  data$Taxon[data$Taxon == "Hydracariens"] <- "Hydracarina"
  data$Taxon[data$Taxon == "Lepidostoma Hirtum"] <- "Lepidostoma hirtum"
  data$Taxon[data$Taxon == "Limonidae"] <- "Limoniidae"
  data$Taxon[data$Taxon == "Nématocère"] <- "Diptères"
  data$Taxon[data$Taxon == "Notidobia Cialiris"] <- "Notidobia"
  data$Taxon[data$Taxon == "Oligochetes"] <- "Oligochaeta"
  data$Taxon[data$Taxon == "Oligochètes"] <- "Oligochaeta"
  data$Taxon[data$Taxon == "Oligochaetae"] <- "Oligochaeta"
  data$Taxon[data$Taxon == "Onichogomphus"] <- "Onychogomphus"
  data$Taxon[data$Taxon == "Orconectes Limosus"] <- "Orconectes limosus"
  data$Taxon[data$Taxon == "Piscicola Geometra"] <- "Piscicola geometra"
  data$Taxon[data$Taxon == "Planorbiidae"] <- "Planorbidae"
  data$Taxon[data$Taxon == "Plécoptère"] <- "Plécoptères"
  data$Taxon[data$Taxon == "Polymitarcidae"] <- "Polymitarcyidae"
  data$Taxon[data$Taxon == "Potamopyrgus Antipodarum"] <- "Potamopyrgus"
  data$Taxon[data$Taxon == "Procleon"] <- "Procloeon"
  data$Taxon[data$Taxon == "Procloëon Bifidum"] <- "Procloeon bifidum"
  data$Taxon[data$Taxon == "Protonemoura"] <- "Protonemura"
  data$Taxon[data$Taxon == "Psychomia"] <- "Psychomyia"
  data$Taxon[data$Taxon == "Stratiomydae"] <- "Stratiomyidae"
  data$Taxon[data$Taxon == "Tidones"] <- "Tinodes"
  data$Taxon[data$Taxon == "Trichoptère"] <- "Trichoptères"
}

  return(data)
  
} # Fin de la fonction