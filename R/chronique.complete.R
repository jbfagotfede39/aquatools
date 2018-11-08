#' Complément de chroniques
#'
#' Cette fonction permet de compléter les chroniques présentant des valeurs manquantes
#' @name chronique.complete
#' @param data Data.frame contenant a minima une colonne Date, une colonne Heure et une colonne Valeur
#' @keywords chronique
#' @import lubridate dplyr
#' @export
#' @examples
#' chronique.complete(data)

chronique.complete <- function(
  data = data)
{
  
##### -------------- A FAIRE -------------- #####

# -------------- A FAIRE -------------- #
  
##### Mise au format des données #####

if(length(unique(data$CodeRDT)) == 1) CodeRDT <- unique(data$CodeRDT) else stop("Différentes stations dans le data")
if(length(unique(data$Unite)) == 1) Unite <- unique(data$Unite) else stop("Différentes unités dans le data")
if(length(unique(data$TypeMesure)) == 1) TypeMesure <- unique(data$TypeMesure) else stop("Différents types de mesure dans le data")
if(length(unique(data$Validation)) == 1) Validation <- unique(data$Validation) else warning("Différents statuts de validation dans le data")

data <-
  data %>% 
  mutate(Time = ymd_hms(paste(Date, Heure, sep = "_")))

min <- min(data$Time)
max <- max(data$Time)

ValeurBrutNA <- seq(as.Date(format(min, format="%Y-%m-%d")), as.Date(format(max, format="%Y-%m-%d")), "day")
ValeurBrutNA <- as.data.frame(ValeurBrutNA)
colnames(ValeurBrutNA) <- "Date"
ValeurBrutNA$MesureID <- as.numeric(NA)
ValeurBrutNA$CodeRDT <- as.character(NA)
ValeurBrutNA$Capteur <- as.character(NA)
ValeurBrutNA$Date <- as.character(ValeurBrutNA$Date)
ValeurBrutNA$Heure <- as.character(NA)
ValeurBrutNA$Valeur <- as.numeric(NA)
ValeurBrutNA$Unite <- as.character(NA)
ValeurBrutNA$TypeMesure <- as.character(NA)
ValeurBrutNA$Validation <- "Validé"
ValeurBrutNA$ModeAcquisition <- as.character(NA)
ValeurBrutNA <- 
  ValeurBrutNA %>% 
  select(MesureID, CodeRDT, Capteur, Date, Heure, Valeur, Unite, TypeMesure, Validation, ModeAcquisition)

data <-
  data %>% 
  select(-Time)

ValeurBrutNA$Date <- ymd(ValeurBrutNA$Date)

data <-
  bind_rows(ValeurBrutNA, data)

data <-
  data %>% 
  complete(Date, Heure) 

data$CodeRDT <- CodeRDT
data$Unite <- Unite
data$TypeMesure <- TypeMesure
#data$Validation <- Validation
data$Validation[is.na(data$Validation)] <- "Validé"

data <-
  data %>% 
  mutate(Time = ymd_hms(paste(Date, Heure, sep = "_"))) %>% 
  filter(Time >= min, Time <= max) %>% 
  select(MesureID, CodeRDT, Capteur, Date, Heure, Valeur, Unite, TypeMesure, Validation, ModeAcquisition)

return(data)

}