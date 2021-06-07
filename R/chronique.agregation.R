#' Agrégation de chroniques
#'
#' Cette fonction permet d'agréger des chroniques de mesures (température, niveaux, etc.) à différentes fréquences (dans l'ordre : pas d'agrégation, jours, mois, années, chronique complète)
#' @name chronique.agregation
#' @param data Data.frame contenant a minima une colonne chmes_date, une colonne chmes_heure et une colonne chmes_valeur
#' @param projet Nom du projet
#' @param instantanne Si \code{TRUE} (par défaut), sortie des données instantannées
#' @param quotidien Si \code{TRUE} (par défaut), agrégation journalière
#' @param mensuel Si \code{TRUE} (par défaut), agrégation mensuelle
#' @param annuel Si \code{TRUE} (par défaut), agrégation annuelle
#' @param integral Si \code{TRUE} (par défaut), agrégation sur l'intégralité de la chronique
#' @param complement Si \code{TRUE}, complément de la chronique avec les données manquantes (\code{FALSE} par défaut). Uniquement pour les valeurs journalières
#' @param datedebutanneebiol Date de démarrage de l'année biologique : 10-01 (par défaut - 1er octobre)
#' @param export Si \code{TRUE}, exporte les résultats (\code{FALSE} par défaut)
#' @keywords chronique
#' @import openxlsx
#' @import tidyverse
#' @export
#' @examples
#' DataTravail <- chronique.agregation(data)
#' DataTravail[[1]];DataTravail[[2]];DataTravail[[3]];DataTravail[[4]];DataTravail[[5]]
#' DataTravail %>% purrr::pluck(2)
#' chronique.agregation() %>% magrittr::extract2(2)
#' chronique.agregation(instantanne = F, mensuel = F, annuel = F, integral = F)
#' chronique.agregation(data, export = T) # Export sous forme de fichier excel avec 5 onglets
#' Mesures %>% group_split(chmes_coderhj) %>% purrr::map_dfr(~ chronique.agregation(.) %>% purrr::pluck(2))

chronique.agregation <- function(
  data = data,
  projet = NA_character_,
  instantanne = TRUE,
  quotidien = TRUE,
  mensuel = TRUE,
  annuel = TRUE,
  integral = TRUE,
  complement = FALSE,
  datedebutanneebiol = "10-01",
  export = FALSE
)
{

##### -------------- A FAIRE -------------- #####
# Traitement multi-site en ajoutant une paramètre de group_by()
# Ajout d'un paramètre saison naturelle (équinoxe et solstice) ? https://stackoverflow.com/questions/9500114/find-which-season-a-particular-date-belongs-to
# Utiliser chronique.renommage.variables() pour pouvoir agréger les données chmesgr_date
# Faire un outil de regroupement des données brutes au format large à partir de la fin du code de 2020-02-13_Export_suivi_FUR_format_DCE.R  
# -------------- A FAIRE -------------- #


##### Mise au format des données #####
## Transformation du format des dates
data <-
  data %>% 
  mutate(chmes_date = ymd(chmes_date)) %>% 
  mutate(Time = ymd_hms(paste(chmes_date, chmes_heure, sep = "_"))) %>% 
  dplyr::filter(is.na(chmes_valeur) == F) %>% 
  arrange(Time) %>% 
  dplyr::select(-Time)

data <- 
  data %>% 
  formatage.annee.biologique(datedebutanneebiol = datedebutanneebiol) # nécessaire pour le calcul des percentiles et les degrés-jours cumulés

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
if(instantanne == TRUE){
  ValInstantanees <-
    data
  if(complement == TRUE){
    # ValInstantanees <-
    # ValInstantanees %>%
    # ungroup() %>% 
    # complete(chmes_date = seq.Date(min(chmes_date), max(chmes_date), by="hour"))
  }
}

#### Agrégation par jour ####
if(quotidien == TRUE){
  ValJours <- 
    data %>% 
    mutate(Time = ymd_hms(paste(chmes_date,chmes_heure,"_"))) %>% 
    group_by(chmes_coderhj, chmes_typemesure, chmes_date) %>%
    dplyr::filter(chmes_valeur == max(chmes_valeur) | chmes_valeur == min(chmes_valeur)) %>%
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
    formatage.annee.biologique(datedebutanneebiol = datedebutanneebiol) %>% 
    group_by(chmes_anneebiol) %>% 
    mutate(SommeMoyJ = cumsum(round(VMoyJ,0))) %>% 
    ungroup() %>% 
    group_by(chmes_coderhj, chmes_typemesure, chmes_date) %>%
    dplyr::select(chmes_coderhj, chmes_typemesure, chmes_date, VMinJ:VAmpliJ, VAmpliSigneJ, VarJ, NMesuresJ, SommeMoyJ)
  
  if(complement == TRUE){
    ValJours <-
      ValJours %>%
      ungroup() %>% 
      group_by(chmes_coderhj, chmes_typemesure) %>% 
      complete(chmes_date = seq.Date(min(chmes_date), max(chmes_date), by="day")) %>% 
      ungroup()
  }
}

#### Agrégation par mois ####
if(mensuel == TRUE){
  ValMois <- 
    data %>% 
    mutate(chmes_mois = paste0(year(chmes_date), "-", month(chmes_date))) %>% 
    group_by(chmes_coderhj, chmes_typemesure, chmes_mois) %>%
    dplyr::filter(chmes_valeur == max(chmes_valeur) | chmes_valeur == min(chmes_valeur)) %>%
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
    dplyr::select(chmes_coderhj, chmes_typemesure, chmes_mois, VMinM:VAmpliM, VAmpliSigneM, VarM, NMesuresM) %>% 
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
        dplyr::select(chmes_coderhj, chmes_typemesure, chmes_mois, SommeMoyJM)
      , by = c("chmes_coderhj", "chmes_typemesure", "chmes_mois"))
  
  if(complement == TRUE){
    # ValMois <-
    #   ValMois %>%
    #   ungroup() %>% 
    #   group_by(chmes_coderhj, chmes_typemesure) %>% 
    #   complete(chmes_mois = seq.Date(min(chmes_mois), max(chmes_mois), by="month")) %>%
    #   ungroup()
    warning("Le complément par mois n'est pas développé")
  }
}

#### Agrégation par année biologique ####
if(annuel == TRUE){
  ValAnneeBiol <-
    data %>% 
    formatage.annee.biologique(datedebutanneebiol = datedebutanneebiol) %>% 
    group_by(chmes_coderhj, chmes_typemesure, chmes_anneebiol) %>%
    dplyr::filter(chmes_valeur == max(chmes_valeur) | chmes_valeur == min(chmes_valeur)) %>%
    arrange(chmes_date) %>%
    summarise(
      VAmpliSigneAB = last(chmes_valeur) - first(chmes_valeur)
    ) %>% 
    left_join(
      data %>% 
        formatage.annee.biologique(datedebutanneebiol = datedebutanneebiol) %>% 
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
    dplyr::select(chmes_coderhj, chmes_typemesure, chmes_anneebiol, VMinAB:VAmpliAB, VAmpliSigneAB, VarAB, NMesuresAB, DateVMinAB, DateVMaxAB) %>% 
    left_join(
      data %>% 
        group_by(chmes_coderhj, chmes_typemesure, chmes_date) %>% 
        summarise(
          VMoyJ = mean(chmes_valeur)) %>% 
        ungroup() %>% 
        formatage.annee.biologique(datedebutanneebiol = datedebutanneebiol) %>% 
        group_by(chmes_coderhj, chmes_typemesure, chmes_anneebiol) %>% 
        summarise(
          SommeMoyJAB = sum(VMoyJ)) %>% 
        dplyr::select(chmes_coderhj, chmes_typemesure, chmes_anneebiol, SommeMoyJAB)
      , by = c("chmes_coderhj", "chmes_typemesure", "chmes_anneebiol"))
  
  ## Calcul des percentiles ##
  # Calcul manuel du percentile 90 diurne #
  PercentilesCalcules <-
    data %>% 
    group_by(chmes_anneebiol) %>% 
    mutate(Time = ymd_hms(paste(chmes_date, chmes_heure, sep = "_"))) %>% 
    dplyr::filter(between(hour(Time), 8, 18)) %>% 
    dplyr::select(-Time) %>% 
    mutate(rang = percent_rank(chmes_valeur)) %>% 
    dplyr::filter(rang <= 0.9) %>% 
    arrange(desc(rang)) %>% 
    dplyr::filter(row_number() == 1) %>% 
    dplyr::select(chmes_anneebiol, chmes_valeur) %>% 
    rename(Percentile90diurneAB = chmes_valeur)
  
  # Calcul des autres percentiles #
  Percentiles <- rev(c(0.1,0.25,0.5,0.75,0.9)) # on inverse pour ensuite avoir les résultats dans l'ordre logique
  
  for(i in 1:length(Percentiles)){
    Percentile <- Percentiles[i]
    
    PercentilesCalcules <-
      data %>% 
      group_by(chmes_anneebiol) %>% 
      mutate(rang = percent_rank(chmes_valeur)) %>% 
      dplyr::filter(rang <= Percentile) %>% 
      arrange(desc(rang)) %>% 
      dplyr::filter(row_number() == 1) %>% 
      dplyr::select(chmes_anneebiol, chmes_valeur) %>% 
      rename(!!paste0('Percentile',quo_name(Percentile*100),'AB') := chmes_valeur) %>% 
      left_join(PercentilesCalcules, by = "chmes_anneebiol")
  }
  ValAnneeBiol <-
    ValAnneeBiol %>% 
    left_join(PercentilesCalcules, by = "chmes_anneebiol")
  
  if(complement == TRUE){
    # ValAnneeBiol <-
    #   ValAnneeBiol %>%
    #   ungroup() %>% 
    #   group_by(chmes_coderhj, chmes_typemesure) %>% 
    #   complete(chmes_anneebiol = seq.Date(min(chmes_anneebiol), max(chmes_anneebiol), by="year")) %>%
    #   ungroup()
    warning("Le complément par année biologique n'est pas développé")
  }
}

#### Agrégation de l'intégralité de la chronique ####
if(integral == TRUE){
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
}

#### Sortie des résultats ####
liste <- list()
if(exists("ValInstantanees") == TRUE){
  liste <- c(liste, ValInstantanees = list(ValInstantanees))
}
if(exists("ValJours") == TRUE){
  liste <- c(liste, ValJours = list(ValJours))
}
if(exists("ValMois") == TRUE){
  liste <- c(liste, ValMois = list(ValMois))
}
if(exists("ValAnneeBiol") == TRUE){
  liste <- c(liste, ValAnneeBiol = list(ValAnneeBiol))
}
if(exists("ValComplet") == TRUE){
  liste <- c(liste, ValComplet = list(ValComplet))
}

## Dataframe vers R
if(export == FALSE){
  return(liste)
}

## Export vers xlsx ##
if(export == TRUE){
  openxlsx::write.xlsx(liste, file = paste0("./",projet, "/Sorties/Données/Agrégations_diverses/", Contexte$chmes_coderhj, "_données.xlsx"))
}

} # Fin de la fonction
