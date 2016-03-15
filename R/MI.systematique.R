#' Complément des données de MI
#'
#' Cette fonction permet de compléter les données de systématique des MI
#' 
#' @param data Jeu de données à compléter
#' @keywords MI
#' @import dplyr DBI
#' @export
#' @examples
#' MI.systematique(data)

###### À faire #####
# 
####################

MI.systematique <- function(data)
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
  #if(all(colnames(data) %in% colnames(Captures))) {
    
    # Ajout de la systématique #
    SyntEsp <- merge(Systematique, data, by.x="Espece", by.y="Taxon")
    SyntGen <- merge(select(Systematique, 5:23), data, by.x="Genre", by.y="Taxon")
    SyntSSFam <- merge(select(Systematique, 9:23), data, by.x="SousFamille", by.y="Taxon")
    SyntFam <- merge(select(Systematique, 12:23), data, by.x="Famille", by.y="Taxon")
    SyntOrd <- merge(select(Systematique, 18:23), data, by.x="Ordre", by.y="Taxon")
    
    data <- full_join(SyntEsp, SyntGen)
    data <- full_join(data, SyntSSFam)
    data <- full_join(data, SyntFam)
    data <- full_join(data, SyntOrd)
    
    data <- distinct(data)
    
  #}
  
  return(data)
  
} # Fin de la fonction