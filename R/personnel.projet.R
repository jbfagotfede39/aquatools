#' Extraction des données de temps de travail par projet
#'
#' Extrait au format excel les données plus ou moins détaillées des coûts du personnel par projet
#' @name personnel.projet
#' @keywords personnel
#' @import dbplyr
#' @import dplyr
#' @import lubridate
#' @import openxlsx
#' @import tidyverse
#' @export
#' @examples
#' personnel.projet("Étude Valouse")

###### À faire #####
# déplacer moe/client de tpswrecap vers tpswprj et ajouter moa (maître d'oeuvre vs maître d'ouvrage)
# Développer la sortie au format excel pour convention AERMC
####################

personnel.projet <- function(
  projet = NA_character_
  )
{
  
  ##### Récupération des données #####
  ## Connexion à la BDD ##
  dbD <- BDD.ouverture("Data")
  
  ## Récupération des données ##
  TpsW <- tbl(dbD, dbplyr::in_schema("fd_production", "tpstravail_detail")) %>% filter(tpswdetail_projet == projet) %>% collect(n = Inf)
  RecapTpsW <- tbl(dbD, dbplyr::in_schema("fd_production", "tpstravail_recapitulatif")) %>% filter(tpswrecap_projet == projet) %>% collect(n = Inf)
  Projets <- tbl(dbD, dbplyr::in_schema("fd_production", "tpstravail_projets")) %>% filter(tpswprj_projet == projet) %>% collect(n = Inf)
  
  id_max <- as.numeric(tbl(dbD,in_schema("fd_production", "tpstravail_recapitulatif")) %>% summarise(max = max(id, na.rm = TRUE)) %>% collect())

  ##### Vérification de l'existence de ce projet #####
  if(dim(Projets)[1] == 0) stop("Projet non répertorié")
  if(dim(TpsW)[1] == 0) stop("Pas de données détaillées pour ce projet")
  if(dim(RecapTpsW)[1] == 0) stop("Pas de données récapitulatives (au moins projetées) pour ce projet")
  
  ##### Extration des coûts unitaires #####
  CoutPersonnel <- 
  RecapTpsW %>% 
    filter(tpswrecap_programmation == "Attendu") %>% 
    filter(!is.na(tpswrecap_poste)) %>% 
    filter(tpswrecap_poste != "") %>% 
    distinct(tpswrecap_poste, tpswrecap_coutunitaire)

  #### Calcul des données élaborées si absentes ####
if(dim(RecapTpsW %>% filter(tpswrecap_programmation == "Réalisé"))[1] == 0){
  if(!grepl("AERMC", projet)){ # Cas où ce n'est pas une convention avec l'AE
  DataToAdd <- 
    TpsW %>% 
    filter(tpswdetail_projet == projet) %>% 
    filter(!is.na(tpswdetail_temps)) %>% 
    group_by(tpswdetail_projet, tpswdetail_poste, tpswdetail_personnel, tpswdetail_detail) %>%
    summarise(tpswrecap_jours = sum(tpswdetail_temps)) %>% 
    ungroup() %>% 
    left_join(CoutPersonnel, by = c("tpswdetail_poste" = "tpswrecap_poste")) %>% 
    mutate(tpswrecap_programmation = "Réalisé") %>% 
    mutate(tpswrecap_natureprojet = as.character(NA)) %>% 
    mutate(tpswrecap_moe = as.character(NA)) %>% 
    mutate(tpswrecap_client = as.character(NA)) %>% 
    mutate(tpswrecap_actionaermc = as.character(NA)) %>%  
    mutate(tpswrecap_sousactionaermc = as.character(NA)) %>%  
    mutate(tpswrecap_argent = tpswrecap_coutunitaire * tpswrecap_jours) %>% 
    mutate(tpswrecap_quantite = as.numeric(NA)) %>%  
    mutate(tpswrecap_quantitepersonnel = as.numeric(NA))
  }
  
  if(grepl("AERMC",projet)){ # Cas où c'est une convention de l'AE
    DataToAdd <- 
      TpsW %>% 
      filter(tpswdetail_projet == projet) %>% 
      filter(!is.na(tpswdetail_temps)) %>% 
      group_by(tpswdetail_projet, tpswdetail_actionaermc, tpswdetail_sousactionaermc, tpswdetail_poste, tpswdetail_personnel) %>%
      summarise(tpswrecap_jours = sum(tpswdetail_temps)) %>% 
      ungroup() %>% 
      left_join(CoutPersonnel, by = c("tpswdetail_poste" = "tpswrecap_poste")) %>% 
      mutate(tpswrecap_programmation = "Réalisé") %>% 
      mutate(tpswrecap_natureprojet = as.character(NA)) %>% 
      mutate(tpswrecap_moe = as.character(NA)) %>% 
      mutate(tpswrecap_client = as.character(NA)) %>% 
      mutate(tpswdetail_detail = as.character(NA)) %>% 
      mutate(tpswrecap_argent = tpswrecap_coutunitaire * tpswrecap_jours) %>% 
      mutate(tpswrecap_quantite = as.numeric(NA)) %>%  
      mutate(tpswrecap_quantitepersonnel = as.numeric(NA))
  }
  
colnames(DataToAdd) <- 
  DataToAdd %>% 
  colnames() %>% 
  str_replace("tpswdetail", "tpswrecap")

DataToAdd <-
  DataToAdd %>% 
  left_join(Projets %>% ungroup() %>% select(tpswprj_projet, tpswprj_natureprojet), by = c("tpswrecap_projet" = "tpswprj_projet")) %>% # 
  mutate(tpswrecap_natureprojet = tpswprj_natureprojet) %>% 
  #mutate(id = NA) %>% # Car met le bazar dans l'incrémentation automatique
  mutate(id = row_number() + id_max) %>%  # Pour incrémenter les id à partir du dernier
  mutate('_modif_utilisateur' = NA) %>% 
  mutate('_modif_date' = NA) %>% 
  mutate('_modif_type' = NA) %>% 
  select(match(names(RecapTpsW),colnames(.)))

# Écriture des données #
RPostgreSQL::dbWriteTable(conn = dbD,
                          name = c("fd_production","tpstravail_recapitulatif"),
                          value = DataToAdd,
                          #overwrite=F,
                          append=T,
                          row.names=FALSE)

# Mise à jour de la séquence des id #

dbGetQuery(dbD, "
SELECT setval('fd_production.tpstravail_recapitulatif_id_seq', COALESCE((SELECT MAX(id)+1 FROM fd_production.tpstravail_recapitulatif), 1), false);
           ")

}

#### Extraction des données élaborées ####
RecapTpsW <- tbl(dbD, dbplyr::in_schema("fd_production", "tpstravail_recapitulatif")) %>% filter(tpswrecap_projet == projet)  %>% filter(tpswrecap_programmation == "Réalisé") %>% collect(n = Inf)

#### Arrêt d'extraction si convention AERMC car non développé ####
if(grepl("AERMC",projet)) stop("Extraction en xlsx non développée pour les conventions AE")

##### Extraction des données de synthèse par poste ####
SynthesePoste <- 
  RecapTpsW %>% 
  filter(tpswrecap_projet == projet) %>% 
  filter(!is.na(tpswrecap_jours)) %>% 
  group_by(tpswrecap_projet, tpswrecap_detail,tpswrecap_personnel) %>% 
  summarise(Jours = sum(tpswrecap_jours)
  ) %>% 
  arrange(tpswrecap_detail,tpswrecap_personnel) %>% 
  rename(Projet = tpswrecap_projet) %>% 
  rename(Detail = tpswrecap_detail) %>% 
  rename(Personnel = tpswrecap_personnel)
  
SynthesePoste <- as.data.frame(SynthesePoste)
  
##### Extraction des données de synthèse par personnel ####
SynthesePersonnel <- 
  RecapTpsW %>% 
  filter(tpswrecap_projet == projet) %>% 
  filter(!is.na(tpswrecap_jours)) %>% 
  group_by(tpswrecap_projet, tpswrecap_personnel) %>%
  summarise(Journées = sum(tpswrecap_jours)) %>% 
  rename(Projet = tpswrecap_projet) %>% 
  rename(Personnel = tpswrecap_personnel)
  
SynthesePersonnel <- as.data.frame(SynthesePersonnel)
  
  ##### Extraction des données détaillées par personnel ####
  Detail <- 
    TpsW %>% 
    filter(tpswdetail_projet == projet) %>% 
    select(tpswdetail_personnel, tpswdetail_date, tpswdetail_poste, tpswdetail_statut, tpswdetail_projet, tpswdetail_detail, tpswdetail_ecosysteme, tpswdetail_temps) %>% 
    arrange(tpswdetail_personnel, tpswdetail_date) %>% 
    filter(!is.na(tpswdetail_temps)) %>% 
    select(-tpswdetail_detail) %>% 
    rename(Personnel = tpswdetail_personnel) %>% 
    rename(Date = tpswdetail_date) %>% 
    rename(Poste = tpswdetail_poste) %>% 
    rename(Statut = tpswdetail_statut) %>% 
    rename(Temps = tpswdetail_temps) %>% 
    rename(Projet = tpswdetail_projet) %>% 
    select(-tpswdetail_ecosysteme)
  
  Detail$Date <- as.character(Detail$Date)
  Detail <- as.data.frame(Detail)
  
  ##### Écriture du fichier excel #####
  tempsprojet <- createWorkbook()
  feuilleSynthesePersonnel <- createSheet(wb=tempsprojet, sheetName="SynthèsePersonnel")
  feuilleSynthesePoste <- createSheet(wb=tempsprojet, sheetName="SynthèsePoste")
  feuilleDetail <- createSheet(wb=tempsprojet, sheetName="Détail")
  addDataFrame(x=SynthesePersonnel, sheet=feuilleSynthesePersonnel, row.names=FALSE)
  addDataFrame(x=SynthesePoste, sheet=feuilleSynthesePoste, row.names=FALSE)
  addDataFrame(x=Detail, sheet=feuilleDetail, row.names=FALSE)
  saveWorkbook(tempsprojet, paste0(format(now(), format="%Y-%m-%d"), "_", projet, "_récapitulatif_coût_personnel.xlsx"))
  
  l <- list(SynthesePoste = SynthesePoste, SynthesePersonnel = SynthesePersonnel, Detail = Detail)
  openxlsx::write.xlsx(l, file = paste0(format(now(), format="%Y-%m-%d"), "_", projet, "_récapitulatif_coût_personnel.xlsx"))
  
} # Fin de la fonction