#' Nettoyage des données de MI
#'
#' Cette fonction permet de corriger des codes erronés des données de MI
#' @name MI.nettoyage
#' @param data Chronique à valider
#' @param larve Si \code{FALSE} (par défault), n'ajoute pas le micapt_stade larvaire si non complété
#' @keywords MI
#' @import DBI
#' @import tidyverse
#' @export
#' @examples
#' MI.nettoyage(data)
#' MI.nettoyage(data, larve = T)

###### À faire #####
# Ré-écriture de noms de micapt_taxons qui n'existent plus, avec ancien nom indiqué en remarque
####################
MI.nettoyage <- function(
  data,
  larve = F)
{
  
  ## Récupération des structures de données ##
HabitatsReference <- 
  structure(list(id = integer(0), mihabref_abreviation = character(0), 
                 mihabref_type = character(0), mihabref_nomcomplet = character(0), 
                 mihabref_dce_habitat = character(0), mihabref_mag20_habitat = character(0), 
                 mihabref_dce_habitabilite = character(0), mihabref_mag20_habitabilite = character(0), 
                 mihabref_remarques = character(0), `_modif_utilisateur` = character(0), 
                 `_modif_type` = character(0), `_modif_date` = structure(numeric(0), tzone = "", class = c("POSIXct", 
                                                                                                           "POSIXt"))), row.names = integer(0), class = c("tbl_df", 
                                                                                                                                                          "tbl", "data.frame"))
Habitats <- structure(list(id = integer(0), mihab_miop_id = integer(0), mihab_mihabref_id = integer(0), 
                           mihab_recouvrement = numeric(0), mihab_margdom = character(0), 
                           mihab_remarques = character(0), `_modif_utilisateur` = character(0), 
                           `_modif_type` = character(0), `_modif_date` = structure(numeric(0), tzone = "", class = c("POSIXct", 
                                                                                                                     "POSIXt"))), row.names = integer(0), class = c("tbl_df", 
                                                                                                                                                                    "tbl", "data.frame"))
Prelevements <- structure(list(id = integer(0), miprlvt_miop_id = integer(0), 
                               miprlvt_numech_dce = integer(0), miprlvt_numech_mag20 = integer(0), 
                               miprlvt_numech_commun = integer(0), miprlvt_mihabref_id_substrat = integer(0), 
                               miprlvt_mihabref_id_vitesse = integer(0), miprlvt_mihabref_id_hauteur = integer(0), 
                               miprlvt_phasedce = character(0), miprlvt_ibgn = logical(0), 
                               miprlvt_intensitecolmatage = numeric(0), miprlvt_stabilite = character(0), 
                               miprlvt_naturevegetation = character(0), miprlvt_micapt_abondancevegetation = numeric(0), 
                               miprlvt_remarques = character(0), `_modif_utilisateur` = character(0), 
                               `_modif_type` = character(0), `_modif_date` = structure(numeric(0), tzone = "", class = c("POSIXct", 
                                                                                                                         "POSIXt"))), row.names = integer(0), class = c("tbl_df", 
                                                                                                                                                                        "tbl", "data.frame"))
Captures <- structure(list(id = integer(0), micapt_miprlvt_id = integer(0), 
                           micapt_taxon = character(0), micapt_abondance = numeric(0), 
                           micapt_typeabondance = character(0), micapt_volumeabondance = character(0), 
                           micapt_stade = character(0), micapt_sexe = character(0), 
                           micapt_remarques = character(0), `_modif_utilisateur` = character(0), 
                           `_modif_type` = character(0), `_modif_date` = structure(numeric(0), tzone = "", class = c("POSIXct", 
                                                                                                                     "POSIXt"))), row.names = integer(0), class = c("tbl_df", 
                                                                                                                                                                    "tbl", "data.frame"))

##### Travail sur les habitats #####
if(all(colnames(data) %in% colnames(Habitats))) {
  # mihab_margdom #
  data$mihab_margdom[data$mihab_margdom == "marginal représentatif (M)"] <- "Marginal représentatif"
  data$mihab_margdom[data$mihab_margdom == "Marginal"] <- "Marginal représentatif"
  data$mihab_margdom[data$mihab_margdom == "dominant (D)"] <- "Dominant"
  data$mihab_margdom[data$mihab_margdom == "D"] <- "Dominant"
  data$mihab_margdom[data$mihab_margdom == "M"] <- "Marginal représentatif"
}

##### Travail sur les prélèvements #####
if(all(colnames(data) %in% colnames(Prelevements))) {
  stop("Syntaxe de la fonction à revoir pour les prélèvements")
  # Substrats #
data$miprlvt_mihabref_id_substrat[data$miprlvt_mihabref_id_substrat == "BRYO"] <- "BRY"
data$miprlvt_mihabref_id_substrat[data$miprlvt_mihabref_id_substrat == "ROC"] <- "DAL"
data$miprlvt_mihabref_id_substrat[data$miprlvt_mihabref_id_substrat == "SUB LIGN"] <- "CHV"
data$miprlvt_mihabref_id_substrat[data$miprlvt_mihabref_id_substrat == "BRC"] <- "CHV"
data$miprlvt_mihabref_id_substrat[data$miprlvt_mihabref_id_substrat == "VAS"] <- "FIN"
data$miprlvt_mihabref_id_substrat <- toupper(data$miprlvt_mihabref_id_substrat) # Pour tout mettre en majuscule
data$miprlvt_mihabref_id_substrat <- str_trim(data$miprlvt_mihabref_id_substrat) # Pour enlever les espaces de début et de fin
  # Vitesses #
data$miprlvt_mihabref_id_vitesse[data$miprlvt_mihabref_id_vitesse == "<5"] <- "V1"
data$miprlvt_mihabref_id_vitesse[data$miprlvt_mihabref_id_vitesse == "5-25"] <- "V3"
data$miprlvt_mihabref_id_vitesse[data$miprlvt_mihabref_id_vitesse == "25-75"] <- "V5"
data$miprlvt_mihabref_id_vitesse[data$miprlvt_mihabref_id_vitesse == "75-150"] <- "V4"
data$miprlvt_mihabref_id_vitesse[data$miprlvt_mihabref_id_vitesse == ">150"] <- "V2"
  # Hauteurs #
data$miprlvt_mihabref_id_hauteur[data$miprlvt_mihabref_id_hauteur == "<5"] <- "H1"
data$miprlvt_mihabref_id_hauteur[data$miprlvt_mihabref_id_hauteur == "5-25"] <- "H2"
data$miprlvt_mihabref_id_hauteur[data$miprlvt_mihabref_id_hauteur == "25-50"] <- "H3"
data$miprlvt_mihabref_id_hauteur[data$miprlvt_mihabref_id_hauteur == "50-100"] <- "H4"
data$miprlvt_mihabref_id_hauteur[data$miprlvt_mihabref_id_hauteur == ">100"] <- "H5"
  # Vérif #
if (all(data$miprlvt_mihabref_id_substrat %in% HabitatsReference$miprlvt_mihabref_id_substrat) == FALSE) stop("Erreur avec un code substrat")
if (all(data$miprlvt_mihabref_id_vitesse %in% HabitatsReference$miprlvt_mihabref_id_vitesse) == FALSE) stop("Erreur avec un code vitesse")
if (all(data$miprlvt_mihabref_id_hauteur %in% HabitatsReference$miprlvt_mihabref_id_hauteur) == FALSE) stop("Erreur avec un code hauteur")
}

##### Travail sur les captures #####
if(all(colnames(data) %in% colnames(Captures))) {
  
  # Nettoyage des micapt_taxons sans effectif #
  data <- 
    data %>% 
    filter(!is.na(micapt_abondance))
  
  # Ajout du micapt_stade larvaire si non ajouté #
  if(larve == T) data$micapt_stade[is.na(data$micapt_stade)] <- "Larve"
  
  # Nettoyage des fautes de saisie #
  data <-
    data %>% 
    mutate(micapt_taxon = str_trim(micapt_taxon)) %>%  # Pour enlever les espaces de début et de fin de micapt_taxon
    mutate(micapt_taxon = str_to_title(micapt_taxon)) %>% # Mise en majuscule du premier caractère
    mutate(micapt_taxon = ifelse(micapt_taxon == "Achete", "Achètes", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Amphinemoura", "Amphinemura", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Anthomyiidae", "Anthomyidae", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Ancylus Fluviatilis", "Ancylus fluviatilis", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Aplexa Hypnorum", "Aplexa hypnorum", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Autre Baetidae", "Baetidae", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Autre Limnephilinae", "Limnephilinae", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Bathyomphalus Contortus", "Bathyomphalus contortus", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Blepharecidae", "Blephariceridae", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Bythinia", "Bithynia", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Centroptilum Luteolum", "Centroptilum", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Cloeon", "Cloëon", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Coléoptère", "Coléoptères", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Colymbetinae (Ilybius)", "Colymbetinae", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Conchostracés", "Conchostraca", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Cordulegastridae", "Cordulegasteridae", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Diptère", "Diptères", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Dystiscinae", "Dytiscinae", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Echdyonurus", "Ecdyonurus", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Elodes", "Helodes", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Ephémeroptère", "Éphéméroptères", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Galba Truncatula", "Galba truncatula", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Gastéropode", "Gastéropodes", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Glossossoma", "Glossosoma", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Haementeria Costata", "Haementeria costata", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Haprophlebia", "Habrophlebia", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Hydracarien", "Hydracarina", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Hydracariens", "Hydracarina", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "HYDRACARIENS", "Hydracarina", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Sf Hydroporinae", "Hydroporinae", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Lepidostoma Hirtum", "Lepidostoma hirtum", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Limonidae", "Limoniidae", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Microcnemia", "Micronecta", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Nématocère", "Diptères", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Notidobia Cialiris", "Notidobia", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Oligochetes", "Oligochaeta", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Oligochètes", "Oligochaeta", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "OLIGOCHETES", "Oligochaeta", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Oligochaetae", "Oligochaeta", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Onichogomphus", "Onychogomphus", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Orconectes Limosus", "Orconectes limosus", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Seratella", "Serratella", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "serratella", "Serratella", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Piscicola Geometra", "Piscicola geometra", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Planorbiidae", "Planorbidae", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Plécoptère", "Plécoptères", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Polymitarcidae", "Polymitarcyidae", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Potamopyrgus Antipodarum", "Potamopyrgus", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Procleon", "Procloeon", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Procloëon Bifidum", "Procloeon bifidum", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Protonemoura", "Protonemura", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Psychomia", "Psychomyia", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Stratiomydae", "Stratiomyidae", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Tidones", "Tinodes", micapt_taxon)) %>% 
    mutate(micapt_taxon = ifelse(micapt_taxon == "Trichoptère", "Trichoptères", micapt_taxon))
}


#### Sortie des données ####
  return(data)
  
} # Fin de la fonction
