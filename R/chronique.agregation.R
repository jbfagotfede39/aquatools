#' Agrégation de chroniques
#'
#' Cette fonction permet d'agréger des chroniques de mesures (température, niveaux, etc.) à différentes fréquences (dans l'ordre : pas d'agrégation, jours, mois, années, chronique complète)
#' @name chronique.agregation
#' @param data Data.frame contenant a minima une colonne chmes_date, une colonne chmes_heure et une colonne chmes_valeur
#' @param projet Nom du projet
#' @param complement Si \code{TRUE}, complément de la chronique avec les données manquantes (\code{FALSE} par défaut). Uniquement pour les valeurs journalières
#' @param export Si \code{TRUE}, exporte les résultats (\code{FALSE} par défaut)
#' @keywords chronique
#' @import lubridate
#' @import openxlsx
#' @import tidyverse
#' @export
#' @examples
#' DataTravail <- chronique.agregation(data)
#' DataTravail[[1]];DataTravail[[2]];DataTravail[[3]];DataTravail[[4]];DataTravail[[5]]
#' chronique.agregation(data, export = T) # Export sous forme de fichier excel avec 5 onglets

chronique.agregation <- function(
  data = data,
  projet = as.character(NA),
  complement = FALSE,
  export = FALSE
)
{

##### -------------- A FAIRE -------------- #####
# Ajout d'un paramètre saison ? Il semble nécessaire de réaliser le calcul à la main pour établir la saison, pas inclus dans lubridate : https://github.com/tidyverse/lubridate/issues/611
# Ajout d'un paramètre année civile ?
# Ajout d'un paramètre saison naturelle (équinoxe et solstice) ? https://stackoverflow.com/questions/9500114/find-which-season-a-particular-date-belongs-to
# Il faudra faire une fonction commune (entre chronique.figure, chronique.figure.cumul, chronique.agregation et chronique.analyse) pour créer un contexte propre de chronique
# -------------- A FAIRE -------------- #


##### Mise au format des données #####
## Transformation du format des dates
data <-
  data %>% 
  mutate(chmes_date = ymd(chmes_date)) %>% 
  mutate(Time = ymd_hms(paste(chmes_date, chmes_heure, sep = "_"))) %>% 
  filter(is.na(chmes_valeur) == F) %>% 
  arrange(Time) %>% 
  select(-Time)

##### Contexte de la chronique #####
Contexte <- 
  data %>% 
  distinct(chmes_coderhj)
if(dim(Contexte)[1] == 0) stop("Aucune donnée dans la chronique à analyser")
if(dim(Contexte)[1] > 1) stop("Différentes stations dans la chronique à analyser")

Contexte <- 
  data %>% 
  summarise(
    Annee = median(year(chmes_date))
  ) %>% 
  bind_cols(Contexte)

if("chmes_typemesure" %in% colnames(data) == FALSE){
  data <- data %>% mutate(chmes_typemesure = NA_character_)
}

#### Pas d'agrégation ####
ValInstantanees <-
  data
if(complement == TRUE){
  # ValInstantanees <-
  # ValInstantanees %>%
  # complete(chmes_date = seq.Date(min(chmes_date), max(chmes_date), by="hour"))
}

#### Agrégation par jour ####
ValJours <- 
  data %>% 
  mutate(Time = ymd_hms(paste(chmes_date,chmes_heure,"_"))) %>% 
  group_by(chmes_coderhj, chmes_typemesure, chmes_date) %>%
  filter(chmes_valeur == max(chmes_valeur) | chmes_valeur == min(chmes_valeur)) %>%
  arrange(Time) %>%
  summarise(
    VAmpliSigneJ = last(chmes_valeur) - first(chmes_valeur)
  ) %>% 
  left_join(
    data %>% 
      group_by(chmes_coderhj, chmes_typemesure, chmes_date) %>% 
      summarise(
        VMinJ = min(chmes_valeur),
        VMedJ = median(chmes_valeur),
        VMoyJ = mean(chmes_valeur),
        VMaxJ = max(chmes_valeur),
        VAmpliJ = VMaxJ-VMinJ,
        VarJ = var(chmes_valeur),
        NMesuresJ = n(),
      ), by = c("chmes_coderhj", "chmes_typemesure", "chmes_date")
  ) %>% 
  ungroup() %>% 
  formatage.annee.biologique() %>% 
  group_by(chmes_anneebiol) %>% 
  mutate(SommeMoyJ = cumsum(round(VMoyJ,0))) %>% 
  ungroup() %>% 
  group_by(chmes_coderhj, chmes_typemesure, chmes_date) %>%
  select(chmes_coderhj, chmes_typemesure, chmes_date, VMinJ:VAmpliJ, VAmpliSigneJ, VarJ, NMesuresJ, SommeMoyJ)

if(complement == TRUE){
  ValJours <-
    ValJours %>%
    complete(chmes_date = seq.Date(min(chmes_date), max(chmes_date), by="day"))
}

#### Agrégation par mois ####
ValMois <- 
  data %>% 
  mutate(chmes_mois = paste0(year(chmes_date), "-", month(chmes_date))) %>% 
  group_by(chmes_coderhj, chmes_typemesure, chmes_mois) %>%
  filter(chmes_valeur == max(chmes_valeur) | chmes_valeur == min(chmes_valeur)) %>%
  arrange(chmes_date) %>%
  summarise(
    VAmpliSigneM = last(chmes_valeur) - first(chmes_valeur)
  ) %>% 
  left_join(
    data %>% 
    mutate(chmes_mois = paste0(year(chmes_date), "-", month(chmes_date))) %>% 
    group_by(chmes_coderhj, chmes_typemesure, chmes_mois) %>% 
    summarise(
      VMinM = min(chmes_valeur),
      VMedM = median(chmes_valeur),
      VMoyM = mean(chmes_valeur),
      VMaxM = max(chmes_valeur),
      VAmpliM = VMaxM-VMinM,
      VarM = var(chmes_valeur),
      NMesuresM = n()
    ), by = c("chmes_coderhj", "chmes_typemesure", "chmes_mois")
  ) %>% 
  select(chmes_coderhj, chmes_typemesure, chmes_mois, VMinM:VAmpliM, VAmpliSigneM, VarM, NMesuresM) %>% 
  left_join(
    data %>% 
      group_by(chmes_coderhj, chmes_typemesure, chmes_date) %>% 
      summarise(
        VMoyJ = mean(chmes_valeur)) %>% 
      ungroup() %>% 
      mutate(chmes_mois = paste0(year(chmes_date), "-", month(chmes_date))) %>% 
      group_by(chmes_coderhj, chmes_typemesure, chmes_mois) %>% 
      summarise(
        SommeMoyJM = sum(VMoyJ)) %>% 
      select(chmes_coderhj, chmes_typemesure, chmes_mois, SommeMoyJM)
      , by = c("chmes_coderhj", "chmes_typemesure", "chmes_mois"))

if(complement == TRUE){
  # ValMois <-
  #   ValMois %>%
  #   complete(chmes_mois = seq.Date(min(chmes_mois), max(chmes_mois), by="month"))
  warning("Le complément par mois n'est pas développé")
}

#### Agrégation par année biologique ####
ValAnneeBiol <-
  data %>% 
  formatage.annee.biologique() %>% 
  group_by(chmes_coderhj, chmes_typemesure, chmes_anneebiol) %>%
  filter(chmes_valeur == max(chmes_valeur) | chmes_valeur == min(chmes_valeur)) %>%
  arrange(chmes_date) %>%
  summarise(
    VAmpliSigneAB = last(chmes_valeur) - first(chmes_valeur)
  ) %>% 
  left_join(
    data %>% 
    formatage.annee.biologique() %>% 
    group_by(chmes_coderhj, chmes_typemesure, chmes_anneebiol) %>% 
    summarise(
      VMinAB = min(chmes_valeur),
      VMedAB = median(chmes_valeur),
      VMoyAB = mean(chmes_valeur),
      VMaxAB = max(chmes_valeur),
      VAmpliAB = VMaxAB-VMinAB,
      VarAB = var(chmes_valeur),
      NMesuresAB = n(),
      DateVMinAB = paste(chmes_date, chmes_heure, sep = " ")[chmes_valeur == min(chmes_valeur)][1],
      DateVMaxAB = paste(chmes_date, chmes_heure, sep = " ")[chmes_valeur == max(chmes_valeur)][1]
    ), by = c("chmes_coderhj", "chmes_typemesure", "chmes_anneebiol")
  ) %>% 
  select(chmes_coderhj, chmes_typemesure, chmes_anneebiol, VMinAB:VAmpliAB, VAmpliSigneAB, VarAB, NMesuresAB, DateVMinAB, DateVMaxAB) %>% 
  left_join(
    data %>% 
      group_by(chmes_coderhj, chmes_typemesure, chmes_date) %>% 
      summarise(
        VMoyJ = mean(chmes_valeur)) %>% 
      ungroup() %>% 
      formatage.annee.biologique() %>% 
      group_by(chmes_coderhj, chmes_typemesure, chmes_anneebiol) %>% 
      summarise(
        SommeMoyJAB = sum(VMoyJ)) %>% 
      select(chmes_coderhj, chmes_typemesure, chmes_anneebiol, SommeMoyJAB)
  , by = c("chmes_coderhj", "chmes_typemesure", "chmes_anneebiol"))

## Calcul des percentiles ##
# Calcul manuel du percentile 90 diurne #
PercentilesCalcules <-
  data %>% 
  group_by(chmes_anneebiol) %>% 
  mutate(Time = ymd_hms(paste(chmes_date, chmes_heure, sep = "_"))) %>% 
  filter(between(hour(Time), 8, 18)) %>% 
  select(-Time) %>% 
  mutate(rang = percent_rank(chmes_valeur)) %>% 
  filter(rang <= 0.9) %>% 
  arrange(desc(rang)) %>% 
  filter(row_number() == 1) %>% 
  select(chmes_anneebiol, chmes_valeur) %>% 
  rename(Percentile90diurneAB = chmes_valeur)

# Calcul des autres percentiles #
Percentiles <- rev(c(0.1,0.25,0.5,0.75,0.9)) # on inverse pour ensuite avoir les résultats dans l'ordre logique

for(i in 1:length(Percentiles)){
  Percentile <- Percentiles[i]
  
  PercentilesCalcules <-
    data %>% 
    group_by(chmes_anneebiol) %>% 
    mutate(rang = percent_rank(chmes_valeur)) %>% 
    filter(rang <= Percentile) %>% 
    arrange(desc(rang)) %>% 
    filter(row_number() == 1) %>% 
    select(chmes_anneebiol, chmes_valeur) %>% 
    rename(!!paste0('Percentile',quo_name(Percentile*100),'AB') := chmes_valeur) %>% 
    left_join(PercentilesCalcules, by = "chmes_anneebiol")
}
ValAnneeBiol <-
  ValAnneeBiol %>% 
  left_join(PercentilesCalcules, by = "chmes_anneebiol")

if(complement == TRUE){
  # ValAnneeBiol <-
  #   ValAnneeBiol %>%
  #   complete(chmes_anneebiol = seq.Date(min(chmes_anneebiol), max(chmes_anneebiol), by="year"))
  warning("Le complément par année biologique n'est pas développé")
  }

#### Agrégation de l'intégralité de la chronique ####
ValComplet <-
  data %>% 
  group_by(chmes_coderhj, chmes_typemesure) %>%
  summarise(
    VMinI = round(min(chmes_valeur),1),
    VMedI = round(median(chmes_valeur),1),
    VMoyI = round(mean(chmes_valeur),1),
    VMaxI = round(max(chmes_valeur),1),
    VAmpliI = VMaxI-VMinI,
    VarI = round(var(chmes_valeur),2),
    NMesuresI = n()
  )

#### Sortie des résultats ####
## Dataframe vers R
if(export == FALSE){
  return(list(ValInstantanees, ValJours, ValMois, ValAnneeBiol, ValComplet))
}

## Export vers xlsx ##
if(export == TRUE){
  l <- list(ValInstantanees = ValInstantanees, ValJours = ValJours, ValMois = ValMois, ValAnneeBiol = ValAnneeBiol, ValComplet = ValComplet)
  openxlsx::write.xlsx(l, file = paste0("./",projet, "/Sorties/Données/",Contexte$chmes_coderhj, "_données.xlsx"))
}
} # Fin de la fonction
