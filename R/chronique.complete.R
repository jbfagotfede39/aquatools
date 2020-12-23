#' Complément de chroniques
#'
#' Cette fonction permet de compléter les chroniques présentant des valeurs manquantes
#' @name chronique.complete
#' @param data Data.frame contenant a minima une colonne chmes_date, une colonne chmes_heure et une colonne chmes_valeur
#' @keywords chronique
#' @import tidyverse
#' @export
#' @examples
#' chronique.complete(data)

chronique.complete <- function(
  data = data)
{
  
##### -------------- A FAIRE -------------- #####

# -------------- A FAIRE -------------- #
  
##### Mise au format des données #####

if(length(unique(data$chmes_coderhj)) == 1) chmes_coderhj <- unique(data$chmes_coderhj) else stop("Différentes stations dans le data")
if(length(unique(data$chmes_unite)) == 1) chmes_unite <- unique(data$chmes_unite) else stop("Différentes unités dans le data")
if(length(unique(data$chmes_typemesure)) == 1) chmes_typemesure <- unique(data$chmes_typemesure) else stop("Différents types de mesure dans le data")
if(length(unique(data$chmes_validation)) == 1) chmes_validation <- unique(data$chmes_validation) else warning("Différents statuts de validation dans le data")

data <-
  data %>% 
  mutate(Time = ymd_hms(paste(chmes_date, chmes_heure, sep = "_")))

min <- min(data$Time)
max <- max(data$Time)

ValeurBrutNA <- seq(as.Date(format(min, format="%Y-%m-%d")), as.Date(format(max, format="%Y-%m-%d")), "day")
ValeurBrutNA <- as.data.frame(ValeurBrutNA)
colnames(ValeurBrutNA) <- "chmes_date"
ValeurBrutNA$id <- as.numeric(NA)
ValeurBrutNA$chmes_coderhj <- as.character(NA)
ValeurBrutNA$chmes_capteur <- as.character(NA)
ValeurBrutNA$chmes_date <- as.character(ValeurBrutNA$chmes_date)
ValeurBrutNA$chmes_heure <- as.character(NA)
ValeurBrutNA$chmes_valeur <- as.numeric(NA)
ValeurBrutNA$chmes_unite <- as.character(NA)
ValeurBrutNA$chmes_typemesure <- as.character(NA)
ValeurBrutNA$chmes_validation <- "Validé"
ValeurBrutNA$chmes_modeacquisition <- as.character(NA)
ValeurBrutNA <- 
  ValeurBrutNA %>% 
  select(id, chmes_coderhj, chmes_capteur, chmes_date, chmes_heure, chmes_valeur, chmes_unite, chmes_typemesure, chmes_validation, chmes_modeacquisition)

data <-
  data %>% 
  select(-Time)

ValeurBrutNA$chmes_date <- ymd(ValeurBrutNA$chmes_date)

data <-
  bind_rows(ValeurBrutNA, data)

data <-
  data %>% 
  complete(chmes_date, chmes_heure) 

data$chmes_coderhj <- chmes_coderhj
data$chmes_unite <- chmes_unite
data$chmes_typemesure <- chmes_typemesure
#data$chmes_validation <- chmes_validation
data$chmes_validation[is.na(data$chmes_validation)] <- "Validé"

data <-
  data %>% 
  mutate(Time = ymd_hms(paste(chmes_date, chmes_heure, sep = "_"))) %>% 
  filter(Time >= min, Time <= max) %>% 
  select(id, chmes_coderhj, chmes_capteur, chmes_date, chmes_heure, chmes_valeur, chmes_unite, chmes_typemesure, chmes_validation, chmes_modeacquisition)

return(data)

}