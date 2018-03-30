#' Calcul initial des coûts de projet
#'
#' Cette fonction permet de calculer les coûts d'un projet au moment de son montage. Elle s'appuie sur le format saisi dans la table RecapTpsW et réalise les conversions à partir des volumes unitaires, et ordonne les sujets
#' 
#' @param NomProjet Nom du dataframe contenant les données "attendues" à traiter, dans le format de RecapTpsW
#' @import dplyr stringr
#' @export
#' @examples
#' RecapDataToAdd <- projet.calculInitial(NomProjet)

##### TODO LIST #####
# 
#####################

projet.calculInitial <- function(
  NomProjet
)
{
  #### Vérifications ####
  if(dim(NomProjet)[1] == 0) stop("Saisir un dataframe en entrée")
  if(dim(distinct(NomProjet, Projet))[1] != 1) stop("Plusieurs projets dans le dataframe d'entrée")
  if(dim(distinct(NomProjet, Programmation))[1] != 1) stop("Plusieurs statuts de programmation dans le dataframe d'entrée")
  
  db <- BDD.ouverture(Type = "Temps de travail")
  TpsW <- tbl(db, "TempsDeTravail") %>% collect(n = 1000)
  RecapTpsW <- tbl(db, "RecapTempsDeTravail") %>% filter(Projet %in% NomProjet$Projet) %>% collect(n = Inf)
  CoutAnnuel <- tbl(db, "CoutAnnuel") %>% collect(n = Inf) %>% arrange(Poste)
  CoutTypePrestation <- tbl(db, "CoutTypePrestation") %>% collect(n = Inf)
  Poste <- tbl(db, "Poste") %>% collect(n = Inf) %>% arrange(Poste)
  Projets <- tbl(db, "Projets") %>% collect(n = Inf) %>% arrange(Etat,DateLancement)
  SuiviBDD <- tbl(db, "SuiviBDD") %>% collect(n = Inf)
  TypologiePrestation <- tbl(db, "TypologiePrestation") %>% collect(n = Inf)
  
  if(all(colnames(NomProjet) != colnames(RecapTpsW))) stop("Dataframe d'entrée différent de RecapTpsW")
  if(CoutAnnuel %>% filter(Annee == year(now())) %>% filter(Type == "Estimé N-1") %>% select(Poste, CoutJournalierMajore) %>% nrow() == 0) stop(paste0("Pas de coûts annuels estimés pour l'année ",year(now())))
  
  ##### Préparation ####
  NomProjetFJPPMA <-
    NomProjet %>% 
    filter(MOE == "FJPPMA")
  
  ##### Calculs ####
  RecapDataToAdd <-
    NomProjetFJPPMA %>% 
    bind_rows(filter(CoutTypePrestation, CoutTypePrestation$Prestation %in% NomProjetFJPPMA$Detail) %>% rename (Detail = Prestation) %>% rename (Jours = Temps) %>% select(-CoutTypePrestationID) %>% left_join(NomProjetFJPPMA %>% filter(is.na(Argent)) %>% select(Detail, Quantite), by = "Detail")) %>%
    left_join(CoutAnnuel %>% filter(Annee == year(now())) %>% filter(Type == "Estimé N-1") %>% select(Poste, CoutJournalierMajore) %>% rename(CoutUnitaire = CoutJournalierMajore), by = "Poste") %>%
    mutate(CoutUnitaire = ifelse(is.na(CoutUnitaire.x), CoutUnitaire.y, CoutUnitaire.x)) %>%
    select(-CoutUnitaire.x,-CoutUnitaire.y) %>% 
    mutate(Programmation = ifelse(dim(distinct(NomProjetFJPPMA, Programmation))[1] == 1, as.character(distinct(NomProjetFJPPMA, Programmation)), "STOP")) %>% 
    mutate(NatureOutil = ifelse(dim(distinct(NomProjetFJPPMA, NatureOutil))[1] == 1, as.character(distinct(NomProjetFJPPMA, NatureOutil)), "STOP")) %>% 
    mutate(MOE = ifelse(dim(distinct(NomProjetFJPPMA, MOE))[1] == 1, as.character(distinct(NomProjetFJPPMA, MOE)), "STOP")) %>% 
    mutate(Projet = ifelse(dim(distinct(NomProjetFJPPMA, Projet))[1] == 1, as.character(distinct(NomProjetFJPPMA, Projet)), "STOP")) %>% 
    filter(!(!is.na(Quantite) & is.na(CoutUnitaire) & is.na(Jours) & is.na(Argent))) %>%
    mutate(QuantitePersonnel = ifelse(is.na(Argent) & is.na(QuantitePersonnel) & !is.na(Jours), 1, QuantitePersonnel)) %>% # Si Argent et QuantitePersonnel vides et Jours rempli, alors un seul bonhomme
    mutate(Argent = ifelse(is.na(Argent), Jours * CoutUnitaire * QuantitePersonnel * Quantite, Argent)) %>% # pour les tâches avec quantité
    mutate(Argent = ifelse(is.na(Argent), Jours * CoutUnitaire * QuantitePersonnel, Argent)) %>% # pour les tâches sans quantité
    mutate(Argent = ifelse(is.na(Argent), Quantite * CoutUnitaire, Argent)) %>% # pour les objets sans personnel
    mutate(Temps = ifelse(!is.na(Jours) & !is.na(Quantite) & !is.na(QuantitePersonnel), Jours * Quantite * QuantitePersonnel, NA)) %>% 
    mutate(Temps = ifelse(!is.na(Jours) & is.na(Quantite) & !is.na(QuantitePersonnel), Jours * QuantitePersonnel, Temps)) %>% 
    dplyr::union(NomProjet %>% filter(Programmation == "Attendu") %>% filter(MOE != "FJPPMA")) %>% 
    select(match(colnames(RecapTpsW),names(.))) %>% 
    mutate(CodeTache = NA) %>% 
    left_join(TypologiePrestation %>% select(-PrestationID, -Groupe) %>% rename(Detail = Prestation), by = "Detail") %>% 
    mutate(Argent = round(Argent,2)) %>% 
    arrange(Ordre, Detail, MOE, desc(Poste)) %>% 
    select(-Ordre)
  
  if(length(which(RecapDataToAdd == "STOP")) != 0) stop("Problème : vérification nécessaire")
  
return(RecapDataToAdd)

} # Fin de la fonction
