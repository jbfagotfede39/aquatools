#' Extraction des données de temps de travail par projet
#'
#' Extrait au format excel les données plus ou moins détaillées des coûts du personnel par projet
#' @keywords personnel
#' @import dplyr RSQLite DBI lubridate xlsx
#' @export
#' @examples
#' personnel.projet("Étude Leue")

personnel.projet <- function(
  projet="Étude Leue")
{
  
  #library("RSQLite");library("dplyr");library(lubridate);library(xlsx)
  
  ##### Récupération des données #####
  ## Connexion à la BDD ##
  db <- BDD.ouverture(Type = "Temps de travail")
  
  ## Récupération des données ##
  TpsW <- dbReadTable(db, "TempsDeTravail")
  RecapTpsW <- dbReadTable(db, "RecapTempsDeTravail")
  
  ##### Transformation des formats #####
  TpsW$Date <- ymd(TpsW$Date)

  ##### Vérification de l'existence de ce projet dans les données élaborées #####
  if(dim(TpsW %>% filter(Identification == projet))[1] == 0) stop("Pas de données détaillées pour ce projet")
  if(dim(RecapTpsW %>% filter(Identification == projet))[1] == 0) stop("Pas de données regroupées pour ce projet")
  
  ##### Extraction des données de synthèse par poste ####
  SynthèsePoste <- 
    RecapTpsW %>% 
    filter(Identification == projet) %>% 
    select(Identification:Jours)
    arrange(Détail,Poste)
  
  ##### Extraction des données de synthèse par personnel ####
  SynthèsePersonnel <- 
    TpsW %>%
    filter(Identification == projet) %>% 
    group_by(Identification, Personnel) %>%
    summarise(Journées = sum(Temps))
  
  SynthèsePersonnel <- as.data.frame(SynthèsePersonnel)
  
  ##### Extraction des données détaillées par personnel ####
  Détail <- 
    TpsW %>% 
    filter(Identification == projet) %>% 
    select(Personnel, Date, Poste, Statut, Identification, Détail, CE, Temps) %>% 
    arrange(Personnel, Date)
  
  Détail$Date <- as.character(Détail$Date)
  
  ##### Écriture du fichier excel #####
  tempsprojet <- createWorkbook()
  feuilleSynthèsePersonnel <- createSheet(wb=tempsprojet, sheetName="SynthèsePersonnel")
  feuilleSynthèsePoste <- createSheet(wb=tempsprojet, sheetName="SynthèsePoste")
  feuilleDétail <- createSheet(wb=tempsprojet, sheetName="Détail")
  addDataFrame(x=SynthèsePersonnel, sheet=feuilleSynthèsePersonnel, row.names=FALSE)
  addDataFrame(x=SynthèsePoste, sheet=feuilleSynthèsePoste, row.names=FALSE)
  addDataFrame(x=Détail, sheet=feuilleDétail, row.names=FALSE)
  saveWorkbook(tempsprojet, paste0(format(now(), format="%Y-%m-%d"), "_", projet, "_récapitulatif_coût_personnel.xlsx"))
  
} # Fin de la fonction