#' Vérification existence MI
#'
#' Cette fonction permet de vérifier la présence des taxons de MI
#' 
#' @param data Jeu de données à compléter
#' @keywords MI
#' @import dplyr DBI
#' @export
#' @examples
#' MI.systematique.presence(data)

###### À faire #####
# 
####################

MI.systematique.presence <- function(data)
{
  
  ## Connexion à la BDD ##
  db <- BDD.ouverture("Macroinvertébrés")
  
  ## Récupération des données ##
  HabitatsReference <- dbReadTable(db, "HabitatsReference")
  Prelevements <- dbReadTable(db, "Prelevements")
  Captures <- dbReadTable(db, "Captures")
  EspecesReference <- dbReadTable(db, "EspecesReference")
  GenresReference <- dbReadTable(db, "GenresReference")
  SousFamillesReference <- dbReadTable(db, "SousFamillesReference")
  FamillesReference <- dbReadTable(db, "FamillesReference")
  OrdresReference <- dbReadTable(db, "OrdresReference")
  
  # Assemblage des morceaux de systématique
  Systematique <- full_join(EspecesReference, GenresReference, "GenreID")
  Systematique <- bind_rows(Systematique, Systematique %>% filter(!is.na(Espece)) %>% select(3:9))
  Systematique <- full_join(Systematique, SousFamillesReference, "SousFamilleID")
  Systematique$FamilleID <- ifelse(!is.na(Systematique$FamilleID.x), Systematique$FamilleID.x, Systematique$FamilleID.y) # Pour tout remettre les FamilleID dans la même colonne
  Systematique <- select(Systematique, -FamilleID.x, -FamilleID.y)
  #Systematique <- bind_rows(Systematique, Systematique %>% filter(!is.na(Genre)) %>% select(3:9))
  Systematique <- full_join(Systematique, FamillesReference, "FamilleID")
  Systematique <- bind_rows(Systematique, Systematique %>% filter(!is.na(Genre)) %>% select(10:17))
  Systematique <- full_join(Systematique, OrdresReference, "OrdreID")
  Systematique <- bind_rows(Systematique, Systematique %>% filter(!is.na(Ordre)) %>% select(18:23))
  
  Systematique <- distinct(Systematique)
  
  # Travail sur les captures #
  if(all(colnames(data) %in% colnames(Captures))) {
    
    # Vérification de l'existence des taxons dans la BDD #
    Absents <- setdiff(unique(data$Taxon), Systematique$Espece)
    Absents <- setdiff(Absents, Systematique$Genre)
    Absents <- setdiff(Absents, Systematique$SousFamille) #
    Absents <- setdiff(Absents, Systematique$Famille)
    Absents <- setdiff(Absents, Systematique$Ordre)
    if(length(Absents) > 0)  Absents <- sort(Absents)
    
  }
  
  return(Absents)
  
} # Fin de la fonction