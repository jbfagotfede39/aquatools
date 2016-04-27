#' Extraction de l'ensemble des données de captures de poissons
#'
#' Récupère l'ensemble des données de captures de poissons de Maxifish dans un dataframe
#' @keywords donnees
#' @import dplyr RSQLite DBI
#' @export
#' @examples
#' poissons.captures.BDD()

poissons.captures.BDD <- function(){
  
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

##### Simplification #####
Captures <- 
  Captures %>%
  select(Nom, DateDebut, NumeroDePassage, CodeEspece, TailleMinimum, TailleMaximum, Nombre, Poids)

##### Calcul d'une colonne taille (avec la taille moyenne pour les lots) #####
Captures$TailleMoy <- rowMeans(Captures[,5:6]) # Afin de calculer la taille moyenne pour les lots
for (i in 1:length(Captures$Nombre)){
  if (Captures$Nombre[i] == 1) Captures$TailleMoy[i] <- Captures$TailleMaximum[i] else Captures$TailleMoy[i] <- Captures$TailleMoy[i]
} # Afin de compléter les tailles pour les poissons individuels

##### Nettoyage des 0 #####
Captures[Captures == 0] <- ""

#return(Captures)
} # Fin de la fonction

# Captures <- poissons.captures.BDD() pour avoir les données en utilisant la fonction