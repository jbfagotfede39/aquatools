#' Calcul d'indices et de listes de MI
#'
#' Calcule d'indices synthétiques à partir de données de captures d'une opération ou plusieurs opérations de suivi MI
#' @param DataTravail Dataframe contenant les données de captures issues de MI.indices()
#' @param Sortie Forme du dataframe de sortie - \code{indices} (par défault), \code{listefaunistiqueGlobale}, \code{listefaunistiqueMAG20}, \code{listefaunistiqueDCEPrelevement}, \code{listefaunistiqueDCEPhase}, \code{listefaunistiqueDCEequivalentIBGN}, \code{listefaunistiqueIBGN}
#' @keywords donnees
#' @import dplyr RSQLite DBI lubridate
#' @export
#' @examples
#' MI.indices(DataTravail)
#' MI.indices(DataTravail, Sortie = listefaunistiqueMAG20)

##### TODO LIST #####
# Ajout du calcul de la robustesse (avec fct nth (ou top_n) dans summarise <-> fctt à voir pour prendre la deuxième valeur après tri ? Et première pour GIIBGN)
# Ajout du calcul du cb2
#####################

MI.indices <- function(
  DataTravail = MI.captures(MI.operations(Sortie = "Complet")),
  Sortie = c("indices","listefaunistiqueGlobale","listefaunistiqueMAG20", "listefaunistiqueDCEPrelevement", "listefaunistiqueDCEPhase", "listefaunistiqueDCEequivalentIBGN", "listefaunistiqueIBGN")
  )
{
  ##### Évaluation des choix #####
  Sortie <- match.arg(Sortie)
  
  ##### Nettoyage ######
  DataTravail <- DataTravail %>% mutate(Abondance = as.numeric(Abondance))
  
  ##### Systématique et code SANDRE ######
  DataTravail <- DataTravail %>% MI.systematique() %>% MI.SANDRE()

  ##### Évaluation du nombre d'opérations #####
  N <- DataTravail %>% distinct(CodeRHJ,Date) %>% n_distinct()
  
  ##### Liste indicielle #####
  if(Sortie == "indices"){
  indices <-
    DataTravail %>% 
    filter(!is.na(Abondance)) %>% 
    rename(GIIBGN = 'GI-IBGN.x') %>% 
    rename(GICB2 = 'GI-CB2.x') %>% 
    left_join(., DataTravail %>% filter(!is.na(Abondance)) %>% group_by(CodeRHJ, Date, Ordre, FamilleSensIBGN) %>% summarise(TotalIBGN = sum(na.omit(Abondance[IBGN == "Oui"]))) %>% filter(TotalIBGN != 0) %>% mutate(EtatGI = case_when(
                                                                                                                                                                                                   TotalIBGN >= 3 & FamilleSensIBGN == "Perlodidae" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 3 & FamilleSensIBGN == "Chloroperlidae" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 3 & FamilleSensIBGN == "Perlidae" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 3 & FamilleSensIBGN == "Taeniopterygidae" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 3 & FamilleSensIBGN == "Brachycentridae" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 3 & FamilleSensIBGN == "Capniidae" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 3 & FamilleSensIBGN == "Odontoceridae" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 3 & FamilleSensIBGN == "Aeshnidae" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 3 & FamilleSensIBGN == "Beraeidae" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 3 & FamilleSensIBGN == "Goeridae" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 3 & FamilleSensIBGN == "Glossosomatidae" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 3 & FamilleSensIBGN == "Leptophlebiidae" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 3 & FamilleSensIBGN == "Leuctridae" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 3 & FamilleSensIBGN == "Ephemeridae" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 3 & FamilleSensIBGN == "Sericostomatidae" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 3 & FamilleSensIBGN == "Nemouridae" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 3 & FamilleSensIBGN == "Heptageniidae" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 3 & FamilleSensIBGN == "Polymitarcyidae" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 3 & FamilleSensIBGN == "Hydroptilidae" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 3 & FamilleSensIBGN == "Polycentropodidae" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 3 & FamilleSensIBGN == "Rhyacophilidae" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 3 & FamilleSensIBGN == "Psychomyiidae" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 10 & FamilleSensIBGN == "Ephemerellidae" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 10 & FamilleSensIBGN == "Limnephilidae" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 3 & FamilleSensIBGN == "Aphelocheiridae" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 3 & FamilleSensIBGN == "Hydropsychidae" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 10 & FamilleSensIBGN == "Elmidae" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 10 & FamilleSensIBGN == "Gammaridae" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 10 & FamilleSensIBGN == "Caenidae" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 10 & FamilleSensIBGN == "Baetidae" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 10 & FamilleSensIBGN == "Chironomidae" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 10 & FamilleSensIBGN == "Asellidae" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 10 & FamilleSensIBGN == "Oligochaeta" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 3 & FamilleSensIBGN == "Achètes" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 3 & Ordre == "Gastéropodes" ~ "GIIBGNOK",
                                                                                                                                                                                                   TotalIBGN >= 3 & Ordre == "Bivalves" ~ "GIIBGNOK",
                                                                                                                                                                                                   TRUE ~ "GIIBGNpasOK")),
              by = c("Ordre", "CodeRHJ", "Date", "FamilleSensIBGN")
    ) %>% 
    ungroup() %>% 
    mutate(EtatGI = ifelse(is.na(EtatGI), "GIIBGNpasOK", EtatGI)) %>% 
    mutate(TotalIBGN = ifelse(is.na(TotalIBGN), 0, TotalIBGN)) %>% 
    group_by(CodeRHJ, Date) %>% 
    #View()
    summarise(
      TotalMAG20 = sum(Abondance[NumEchMAG20 <= 20]),
      TotalTEP = sum(Abondance[Ordre == "Plécoptères" | Ordre == "Trichoptères" | Ordre == "Éphéméroptères"]),
      TotalCO = sum(Abondance[Taxon == "Chironomidae" | Taxon == "Oligochaeta"]),
      TotalDCE = sum(na.omit(Abondance[PhaseDCE == "A" | PhaseDCE == "B" | PhaseDCE == "C"])),
      TotalPhaseA = sum(na.omit(Abondance[PhaseDCE == "A"])),
      TotalPhaseB = sum(na.omit(Abondance[PhaseDCE == "B"])),
      TotalPhaseC = sum(na.omit(Abondance[PhaseDCE == "C"])),
      TotalEqIBGN = sum(na.omit(Abondance[PhaseDCE == "A" | PhaseDCE == "B"])),
      TotalIBGN = sum(na.omit(Abondance[IBGN == "Oui"])),
      GIIBGN = max(na.omit(GIIBGN[IBGN == "Oui" & EtatGI == "GIIBGNOK"])),
      #RobustesseIBGN = nth(GIIBGN[IBGN == "Oui" & EtatGI == "GIIBGNOK"], 1 ,order_by = GIIBGN),
      VarieteIBGN = length(unique(FamilleSensIBGN[IBGN == "Oui"])),
      #GICB2 = max(na.omit(GICB2[IBGN == "Oui"])),
      #VarieteCB2 = length(unique(FamilleSensIBGN[IBGN == "Oui"])),
      VarieteTotale = length(unique(Taxon)),
      VarieteFamiliale = length(unique(FamilleSensIBGN)),
      VarietePleco = length(unique(Taxon[Ordre == "Plécoptères"])),
      VarieteTricho = length(unique(Taxon[Ordre == "Trichoptères"])),
      VarieteEphemero = length(unique(Taxon[Ordre == "Éphéméroptères"])),
      VarieteColeo = length(unique(Taxon[Ordre == "Coléoptères"]))
      ) %>% 
    mutate_all(funs(replace(., . == 0, NA))) %>% 
    mutate(NoteIBGN = case_when(VarieteIBGN >= 50 & GIIBGN == 9 ~ 20,
                                VarieteIBGN <= 49 & VarieteIBGN >= 45 & GIIBGN == 9 ~ 20,
                                VarieteIBGN <= 44 & VarieteIBGN >= 41 & GIIBGN == 9 ~ 20,
                                VarieteIBGN <= 40 & VarieteIBGN >= 37 & GIIBGN == 9 ~ 19,
                                VarieteIBGN <= 36 & VarieteIBGN >= 33 & GIIBGN == 9 ~ 18,
                                VarieteIBGN <= 32 & VarieteIBGN >= 29 & GIIBGN == 9 ~ 17,
                                VarieteIBGN <= 28 & VarieteIBGN >= 25 & GIIBGN == 9 ~ 16,
                                VarieteIBGN <= 24 & VarieteIBGN >= 21 & GIIBGN == 9 ~ 15,
                                VarieteIBGN <= 20 & VarieteIBGN >= 17 & GIIBGN == 9 ~ 14,
                                VarieteIBGN <= 16 & VarieteIBGN >= 13 & GIIBGN == 9 ~ 13,
                                VarieteIBGN <= 12 & VarieteIBGN >= 10 & GIIBGN == 9 ~ 12,
                                VarieteIBGN <= 9 & VarieteIBGN >= 7 & GIIBGN == 9 ~ 11,
                                VarieteIBGN <= 6 & VarieteIBGN >= 4 & GIIBGN == 9 ~ 10,
                                VarieteIBGN <= 3 & VarieteIBGN >= 1 & GIIBGN == 9 ~ 9,
                                VarieteIBGN >= 50 & GIIBGN == 8 ~ 20,
                                VarieteIBGN <= 49 & VarieteIBGN >= 45 & GIIBGN == 8 ~ 20,
                                VarieteIBGN <= 44 & VarieteIBGN >= 41 & GIIBGN == 8 ~ 19,
                                VarieteIBGN <= 40 & VarieteIBGN >= 37 & GIIBGN == 8 ~ 18,
                                VarieteIBGN <= 36 & VarieteIBGN >= 33 & GIIBGN == 8 ~ 17,
                                VarieteIBGN <= 32 & VarieteIBGN >= 29 & GIIBGN == 8 ~ 16,
                                VarieteIBGN <= 28 & VarieteIBGN >= 25 & GIIBGN == 8 ~ 15,
                                VarieteIBGN <= 24 & VarieteIBGN >= 21 & GIIBGN == 8 ~ 14,
                                VarieteIBGN <= 20 & VarieteIBGN >= 17 & GIIBGN == 8 ~ 13,
                                VarieteIBGN <= 16 & VarieteIBGN >= 13 & GIIBGN == 8 ~ 12,
                                VarieteIBGN <= 12 & VarieteIBGN >= 10 & GIIBGN == 8 ~ 11,
                                VarieteIBGN <= 9 & VarieteIBGN >= 7 & GIIBGN == 8 ~ 10,
                                VarieteIBGN <= 6 & VarieteIBGN >= 4 & GIIBGN == 8 ~ 9,
                                VarieteIBGN <= 3 & VarieteIBGN >= 1 & GIIBGN == 8 ~ 8,
                                VarieteIBGN >= 50 & GIIBGN == 7 ~ 20,
                                VarieteIBGN <= 49 & VarieteIBGN >= 45 & GIIBGN == 7 ~ 19,
                                VarieteIBGN <= 44 & VarieteIBGN >= 41 & GIIBGN == 7 ~ 18,
                                VarieteIBGN <= 40 & VarieteIBGN >= 37 & GIIBGN == 7 ~ 17,
                                VarieteIBGN <= 36 & VarieteIBGN >= 33 & GIIBGN == 7 ~ 16,
                                VarieteIBGN <= 32 & VarieteIBGN >= 29 & GIIBGN == 7 ~ 15,
                                VarieteIBGN <= 28 & VarieteIBGN >= 25 & GIIBGN == 7 ~ 14,
                                VarieteIBGN <= 24 & VarieteIBGN >= 21 & GIIBGN == 7 ~ 13,
                                VarieteIBGN <= 20 & VarieteIBGN >= 17 & GIIBGN == 7 ~ 12,
                                VarieteIBGN <= 16 & VarieteIBGN >= 13 & GIIBGN == 7 ~ 11,
                                VarieteIBGN <= 12 & VarieteIBGN >= 10 & GIIBGN == 7 ~ 10,
                                VarieteIBGN <= 9 & VarieteIBGN >= 7 & GIIBGN == 7 ~ 9,
                                VarieteIBGN <= 6 & VarieteIBGN >= 4 & GIIBGN == 7 ~ 8,
                                VarieteIBGN <= 3 & VarieteIBGN >= 1 & GIIBGN == 7 ~ 7,
                                VarieteIBGN >= 50 & GIIBGN == 6 ~ 19,
                                VarieteIBGN <= 49 & VarieteIBGN >= 45 & GIIBGN == 6 ~ 18,
                                VarieteIBGN <= 44 & VarieteIBGN >= 41 & GIIBGN == 6 ~ 17,
                                VarieteIBGN <= 40 & VarieteIBGN >= 37 & GIIBGN == 6 ~ 16,
                                VarieteIBGN <= 36 & VarieteIBGN >= 33 & GIIBGN == 6 ~ 15,
                                VarieteIBGN <= 32 & VarieteIBGN >= 29 & GIIBGN == 6 ~ 14,
                                VarieteIBGN <= 28 & VarieteIBGN >= 25 & GIIBGN == 6 ~ 13,
                                VarieteIBGN <= 24 & VarieteIBGN >= 21 & GIIBGN == 6 ~ 12,
                                VarieteIBGN <= 20 & VarieteIBGN >= 17 & GIIBGN == 6 ~ 11,
                                VarieteIBGN <= 16 & VarieteIBGN >= 13 & GIIBGN == 6 ~ 10,
                                VarieteIBGN <= 12 & VarieteIBGN >= 10 & GIIBGN == 6 ~ 9,
                                VarieteIBGN <= 9 & VarieteIBGN >= 7 & GIIBGN == 6 ~ 8,
                                VarieteIBGN <= 6 & VarieteIBGN >= 4 & GIIBGN == 6 ~ 7,
                                VarieteIBGN <= 3 & VarieteIBGN >= 1 & GIIBGN == 6 ~ 6,
                                VarieteIBGN >= 50 & GIIBGN == 5 ~ 18,
                                VarieteIBGN <= 49 & VarieteIBGN >= 45 & GIIBGN == 5 ~ 17,
                                VarieteIBGN <= 44 & VarieteIBGN >= 41 & GIIBGN == 5 ~ 16,
                                VarieteIBGN <= 40 & VarieteIBGN >= 37 & GIIBGN == 5 ~ 15,
                                VarieteIBGN <= 36 & VarieteIBGN >= 33 & GIIBGN == 5 ~ 14,
                                VarieteIBGN <= 32 & VarieteIBGN >= 29 & GIIBGN == 5 ~ 13,
                                VarieteIBGN <= 28 & VarieteIBGN >= 25 & GIIBGN == 5 ~ 12,
                                VarieteIBGN <= 24 & VarieteIBGN >= 21 & GIIBGN == 5 ~ 11,
                                VarieteIBGN <= 20 & VarieteIBGN >= 17 & GIIBGN == 5 ~ 10,
                                VarieteIBGN <= 16 & VarieteIBGN >= 13 & GIIBGN == 5 ~ 9,
                                VarieteIBGN <= 12 & VarieteIBGN >= 10 & GIIBGN == 5 ~ 8,
                                VarieteIBGN <= 9 & VarieteIBGN >= 7 & GIIBGN == 5 ~ 7,
                                VarieteIBGN <= 6 & VarieteIBGN >= 4 & GIIBGN == 5 ~ 6,
                                VarieteIBGN <= 3 & VarieteIBGN >= 1 & GIIBGN == 5 ~ 5,
                                VarieteIBGN >= 50 & GIIBGN == 4 ~ 17,
                                VarieteIBGN <= 49 & VarieteIBGN >= 45 & GIIBGN == 4 ~ 16,
                                VarieteIBGN <= 44 & VarieteIBGN >= 41 & GIIBGN == 4 ~ 15,
                                VarieteIBGN <= 40 & VarieteIBGN >= 37 & GIIBGN == 4 ~ 14,
                                VarieteIBGN <= 36 & VarieteIBGN >= 33 & GIIBGN == 4 ~ 13,
                                VarieteIBGN <= 32 & VarieteIBGN >= 29 & GIIBGN == 4 ~ 12,
                                VarieteIBGN <= 28 & VarieteIBGN >= 25 & GIIBGN == 4 ~ 11,
                                VarieteIBGN <= 24 & VarieteIBGN >= 21 & GIIBGN == 4 ~ 10,
                                VarieteIBGN <= 20 & VarieteIBGN >= 17 & GIIBGN == 4 ~ 9,
                                VarieteIBGN <= 16 & VarieteIBGN >= 13 & GIIBGN == 4 ~ 8,
                                VarieteIBGN <= 12 & VarieteIBGN >= 10 & GIIBGN == 4 ~ 7,
                                VarieteIBGN <= 9 & VarieteIBGN >= 7 & GIIBGN == 4 ~ 6,
                                VarieteIBGN <= 6 & VarieteIBGN >= 4 & GIIBGN == 4 ~ 5,
                                VarieteIBGN <= 3 & VarieteIBGN >= 1 & GIIBGN == 4 ~ 4,
                                VarieteIBGN >= 50 & GIIBGN == 3 ~ 16,
                                VarieteIBGN <= 49 & VarieteIBGN >= 45 & GIIBGN == 3 ~ 15,
                                VarieteIBGN <= 44 & VarieteIBGN >= 41 & GIIBGN == 3 ~ 14,
                                VarieteIBGN <= 40 & VarieteIBGN >= 37 & GIIBGN == 3 ~ 13,
                                VarieteIBGN <= 36 & VarieteIBGN >= 33 & GIIBGN == 3 ~ 12,
                                VarieteIBGN <= 32 & VarieteIBGN >= 29 & GIIBGN == 3 ~ 11,
                                VarieteIBGN <= 28 & VarieteIBGN >= 25 & GIIBGN == 3 ~ 10,
                                VarieteIBGN <= 24 & VarieteIBGN >= 21 & GIIBGN == 3 ~ 9,
                                VarieteIBGN <= 20 & VarieteIBGN >= 17 & GIIBGN == 3 ~ 8,
                                VarieteIBGN <= 16 & VarieteIBGN >= 13 & GIIBGN == 3 ~ 7,
                                VarieteIBGN <= 12 & VarieteIBGN >= 10 & GIIBGN == 3 ~ 6,
                                VarieteIBGN <= 9 & VarieteIBGN >= 7 & GIIBGN == 3 ~ 5,
                                VarieteIBGN <= 6 & VarieteIBGN >= 4 & GIIBGN == 3 ~ 4,
                                VarieteIBGN <= 3 & VarieteIBGN >= 1 & GIIBGN == 3 ~ 3,
                                VarieteIBGN >= 50 & GIIBGN == 2 ~ 15,
                                VarieteIBGN <= 49 & VarieteIBGN >= 45 & GIIBGN == 2 ~ 14,
                                VarieteIBGN <= 44 & VarieteIBGN >= 41 & GIIBGN == 2 ~ 13,
                                VarieteIBGN <= 40 & VarieteIBGN >= 37 & GIIBGN == 2 ~ 12,
                                VarieteIBGN <= 36 & VarieteIBGN >= 33 & GIIBGN == 2 ~ 11,
                                VarieteIBGN <= 32 & VarieteIBGN >= 29 & GIIBGN == 2 ~ 10,
                                VarieteIBGN <= 28 & VarieteIBGN >= 25 & GIIBGN == 2 ~ 9,
                                VarieteIBGN <= 24 & VarieteIBGN >= 21 & GIIBGN == 2 ~ 8,
                                VarieteIBGN <= 20 & VarieteIBGN >= 17 & GIIBGN == 2 ~ 7,
                                VarieteIBGN <= 16 & VarieteIBGN >= 13 & GIIBGN == 2 ~ 6,
                                VarieteIBGN <= 12 & VarieteIBGN >= 10 & GIIBGN == 2 ~ 5,
                                VarieteIBGN <= 9 & VarieteIBGN >= 7 & GIIBGN == 2 ~ 4,
                                VarieteIBGN <= 6 & VarieteIBGN >= 4 & GIIBGN == 2 ~ 3,
                                VarieteIBGN <= 3 & VarieteIBGN >= 1 & GIIBGN == 2 ~ 2,
                                VarieteIBGN >= 50 & GIIBGN == 1 ~ 14,
                                VarieteIBGN <= 49 & VarieteIBGN >= 45 & GIIBGN == 1 ~ 13,
                                VarieteIBGN <= 44 & VarieteIBGN >= 41 & GIIBGN == 1 ~ 12,
                                VarieteIBGN <= 40 & VarieteIBGN >= 37 & GIIBGN == 1 ~ 11,
                                VarieteIBGN <= 36 & VarieteIBGN >= 33 & GIIBGN == 1 ~ 10,
                                VarieteIBGN <= 32 & VarieteIBGN >= 29 & GIIBGN == 1 ~ 9,
                                VarieteIBGN <= 28 & VarieteIBGN >= 25 & GIIBGN == 1 ~ 8,
                                VarieteIBGN <= 24 & VarieteIBGN >= 21 & GIIBGN == 1 ~ 7,
                                VarieteIBGN <= 20 & VarieteIBGN >= 17 & GIIBGN == 1 ~ 6,
                                VarieteIBGN <= 16 & VarieteIBGN >= 13 & GIIBGN == 1 ~ 5,
                                VarieteIBGN <= 12 & VarieteIBGN >= 10 & GIIBGN == 1 ~ 4,
                                VarieteIBGN <= 9 & VarieteIBGN >= 7 & GIIBGN == 1 ~ 3,
                                VarieteIBGN <= 6 & VarieteIBGN >= 4 & GIIBGN == 1 ~ 2,
                                VarieteIBGN <= 3 & VarieteIBGN >= 1 & GIIBGN == 1 ~ 1
                                )
           )
    return(indices)
  }
  
  ##### Liste faunistiques #####
  
  #### listefaunistiqueGlobale ####
  if(N != 1 & Sortie == "listefaunistiqueGlobale") stop("Établissement des listes faunistiques de plusieurs opérations non encore développé")
  if(N == 1 & Sortie == "listefaunistiqueGlobale") stop("Établissement de la listefaunistiqueGlobale d'une seule opération non encore développé")
  
  #### listefaunistiqueMAG20 ####
  if(N != 1 & Sortie == "listefaunistiqueMAG20"){
    listefaunistiqueMAG20 <- 
    DataTravail %>% 
      filter(NumEchMAG20 <= 20) %>% 
      dcast(Taxon ~ CodeRHJ , value.var = "Abondance", sum) %>% 
      right_join(DataTravail %>% distinct(Taxon, CodeSANDRE, AffichageSensibilite, Ordre, Famille, Genre, Espece), by = "Taxon") %>% 
      arrange(AffichageSensibilite, Ordre, Famille, Genre, Espece)  %>% 
      select(Ordre, Famille, Genre, Espece, Taxon, CodeSANDRE, everything(), -AffichageSensibilite) %>% 
      #rename('Taxon/Prelevement' = Taxon) %>% 
      mutate_all(funs(replace(., . == 0, NA))) %>% 
      mutate(Ordre2 = ifelse(lag(Ordre) == Ordre, NA, Ordre)) %>% # Pour clarifier l'affichage
      mutate(Ordre2 = ifelse(row_number() == 1, Ordre , Ordre2)) %>% 
      mutate(Ordre = Ordre2) %>% 
      select(-Ordre2) %>% 
      mutate(Famille2 = ifelse(lag(Famille) == Famille, NA, Famille)) %>% # Pour clarifier l'affichage
      mutate(Famille2 = ifelse(row_number() == 1, Famille , Famille2)) %>% 
      mutate(Famille = Famille2) %>% 
      select(-Famille2)
      # mutate(Genre2 = ifelse(lag(Genre) == Genre, NA, Genre)) %>% # Pour clarifier l'affichage
      # mutate(Genre2 = ifelse(row_number() == 1, Genre , Genre2)) %>% 
      # mutate(Genre = Genre2) %>% 
      #select(-Genre2) %>% 
      # mutate(Espece2 = ifelse(lag(Espece) == Espece, NA, Espece)) %>% # Pour clarifier l'affichage
      # mutate(Espece2 = ifelse(row_number() == 1, Espece , Espece2)) %>% 
      # mutate(Espece = Espece2) %>% 
      #select(-Espece2)
    
    return(listefaunistiqueMAG20)
  }
  
  if(N == 1 & Sortie == "listefaunistiqueMAG20"){
    listefaunistiqueMAG20 <- 
    DataTravail %>% 
    filter(NumEchMAG20 <= 20) %>% 
    dcast(Taxon ~ NumEchMAG20 , value.var = "Abondance", sum) %>% 
    right_join(DataTravail %>% distinct(Taxon, CodeSANDRE), by = "Taxon") %>% 
    select(Taxon, CodeSANDRE, everything()) %>% 
    rename('Taxon/Prelevement' = Taxon) %>% 
    mutate_all(funs(replace(., . == 0, NA)))
    
    return(listefaunistiqueMAG20)
  }
  
  #### listefaunistiqueDCEPrelevement ####
  if(N != 1 & Sortie == "listefaunistiqueDCEPrelevement") stop("Établissement des listes faunistiques de plusieurs opérations non encore développé")
  
  if(N == 1 & Sortie == "listefaunistiqueDCEPrelevement"){
    listefaunistiqueDCEPrelevement <- 
      DataTravail %>% 
      filter(!is.na(NumEchDCE)) %>% 
      dcast(Taxon ~ NumEchDCE , value.var = "Abondance", sum) %>% 
      right_join(DataTravail %>% distinct(Taxon, CodeSANDRE), by = "Taxon") %>% 
      select(Taxon, CodeSANDRE, everything()) %>% 
      rename('Taxon/Prelevement' = Taxon) %>% 
      mutate_all(funs(replace(., . == 0, NA)))
    
    return(listefaunistiqueDCEPrelevement)
  }
  
  #### listefaunistiqueDCEPhase ####
  if(N != 1 & Sortie == "listefaunistiqueDCEPhase") stop("Établissement des listes faunistiques de plusieurs opérations non encore développé")
  
  if(N == 1 & Sortie == "listefaunistiqueDCEPhase"){
    listefaunistiqueDCEPhase <- 
      DataTravail %>% 
      filter(!is.na(NumEchDCE)) %>% 
      mutate(Abondance = as.numeric(Abondance)) %>% 
      dcast(Taxon ~ PhaseDCE , value.var = "Abondance", sum) %>% 
      right_join(DataTravail %>% distinct(Taxon, CodeSANDRE), by = "Taxon") %>% 
      select(Taxon, CodeSANDRE, everything()) %>% 
      rename('Taxon/Phase' = Taxon) %>% 
      mutate_all(funs(replace(., . == 0, NA)))
      
    return(listefaunistiqueDCEPhase)
  }
  
  
  #### listefaunistiqueDCEequivalentIBGN ####
  if(N != 1 & Sortie == "listefaunistiqueDCEequivalentIBGN") stop("Établissement des listes faunistiques de plusieurs opérations non encore développé")
  if(N == 1 & Sortie == "listefaunistiqueDCEequivalentIBGN") stop("Établissement de la listefaunistiqueDCEequivalentIBGN d'une seule opération non encore développé")
  
  #### listefaunistiqueIBGN ####
  if(N != 1 & Sortie == "listefaunistiqueIBGN") stop("Établissement des listes faunistiques de plusieurs opérations non encore développé")
  if(N == 1 & Sortie == "listefaunistiqueIBGN") stop("Établissement de la listefaunistiqueIBGN d'une seule opération non encore développé")

} # Fin de la fonction
