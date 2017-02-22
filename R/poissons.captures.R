#' Extraction des données de captures de poissons pour une opération
#'
#' Récupère les données de captures de poissons d'une opération
#' @keywords donnees
#' @import dplyr RSQLite DBI lubridate
#' @export
#' @examples
#' poissons.captures("SOR10-2", "2015-05-19")

poissons.captures <- function(  
  station="SOR10-2",
  date="2015-05-19")
{
  
#library("RSQLite");library("dplyr")
  
  
## Ouverture de la BDD ##
db <- BDD.ouverture(Type = "Poissons")

##### Récupération des données #####
Captures <- dbReadTable(db, "Captures")
#Operations <- dbReadTable(db, "Operations")
Inventaires <- dbReadTable(db, "Inventaires")
Stations <- dbReadTable(db, "Stations")

##### Synthèse des données #####
Captures <- merge(Captures, Inventaires, by = c("CodeInventaire"))
Captures <- merge(Captures, Stations, by = c("CodeStation"))

##### Transformation des formats de dates
Captures$DateDebut <- ymd_hms(Captures$DateDebut)
Captures$DateDebut <- format(Captures$DateDebut, "%Y-%m-%d")

##### Filtrage #####
Captures <-
  Captures %>% 
  filter(Nom == station, DateDebut == date) %>%
  arrange(NumeroDePassage, CodeEspece)

##### Simplification #####
Captures <- 
  Captures %>%
  select(Nom, DateDebut, NumeroDePassage, CodeEspece, TailleMinimum, TailleMaximum, Nombre, Poids)

##### Calcul d'une colonne taille (avec la taille moyenne pour les lots) #####
Captures <- 
Captures %>% 
  rowwise() %>% # groupement par ligne
  mutate(TailleMoy = as.integer(mean(c(TailleMinimum,TailleMaximum)))) %>% # Afin de calculer la taille moyenne pour les lots
  ungroup() %>% # Afin d'enlever le groupement par ligne lié à rowwise
  mutate(TailleMoy = case_when(.$Nombre == 1 ~ .$TailleMaximum,
                               .$Nombre != 1 ~ .$TailleMoy))# Afin de compléter les tailles pour les poissons individuels

##### Nettoyage des 0 #####
Captures[Captures == 0] <- ""

return(Captures)
} # Fin de la fonction