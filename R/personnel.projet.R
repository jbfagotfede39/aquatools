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
  TpsW <- tbl(db,"TempsDeTravail") %>% collect(n = Inf)
  RecapTpsW <- tbl(db,"RecapTempsDeTravail") %>% collect(n = Inf)
  
  ##### Transformation des formats #####
  TpsW$Date <- ymd(TpsW$Date)

  ##### Vérification de l'existence de ce projet dans les données élaborées #####
  if(dim(TpsW %>% filter(Identification == projet))[1] == 0) stop("Pas de données détaillées pour ce projet")
  if(dim(RecapTpsW %>% filter(Identification == projet))[1] == 0) stop("Pas de données regroupées pour ce projet")
  
  ##### Extraction des données de synthèse par poste ####
  SynthesePoste <- 
    RecapTpsW %>% 
    filter(Identification == projet) %>% 
    select(Identification:Jours)
    arrange(Detail,Poste)
  
  ##### Extraction des données de synthèse par personnel ####
  SynthesePersonnel <- 
    TpsW %>%
    filter(Identification == projet) %>% 
    group_by(Identification, Personnel) %>%
    summarise(Journees = sum(Temps))
  
  SynthesePersonnel <- as.data.frame(SynthesePersonnel)
  
  ##### Extraction des données détaillées par personnel ####
  Detail <- 
    TpsW %>% 
    filter(Identification == projet) %>% 
    select(Personnel, Date, Poste, Statut, Identification, Detail, CE, Temps) %>% 
    arrange(Personnel, Date)
  
  Detail$Date <- as.character(Detail$Date)
  
  ##### Écriture du fichier excel #####
  tempsprojet <- createWorkbook()
  feuilleSynthesePersonnel <- createSheet(wb=tempsprojet, sheetName="SynthèsePersonnel")
  feuilleSynthesePoste <- createSheet(wb=tempsprojet, sheetName="SynthèsePoste")
  feuilleDetail <- createSheet(wb=tempsprojet, sheetName="Détail")
  addDataFrame(x=SynthesePersonnel, sheet=feuilleSynthesePersonnel, row.names=FALSE)
  addDataFrame(x=SynthesePoste, sheet=feuilleSynthesePoste, row.names=FALSE)
  addDataFrame(x=Detail, sheet=feuilleDetail, row.names=FALSE)
  saveWorkbook(tempsprojet, paste0(format(now(), format="%Y-%m-%d"), "_", projet, "_récapitulatif_coût_personnel.xlsx"))
  
} # Fin de la fonction