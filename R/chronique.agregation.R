#' Agrégation de chroniques
#'
#' Cette fonction permet d'agréger des chroniques de mesures (température, niveaux, etc.)
#' @name chronique.agregation
#' @param data Data.frame contenant a minima une colonne chmes_date, une colonne chmes_heure et une colonne chmes_valeur
#' @keywords chronique
#' @import openxlsx
#' @import lubridate
#' @import tidyverse
#' @export
#' @examples
#' DataTravail <- chronique.agregation(data)
#' DataTravail[[1]];DataTravail[[2]];DataTravail[[3]];DataTravail[[4]]
#' chronique.agregation(data, export= T) # Export sous forme de fichier excel avec 4 onglets

chronique.agregation <- function(
  data = data,
  export = FALSE
)
{

##### -------------- A FAIRE -------------- #####
# Ajout d'un paramètre saison ? Il semble nécessaire de réaliser le calcul à la main pour établir la saison, pas inclus dans lubridate
# Ajout d'un paramètre année civile ?
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
  distinct(CodeRDT)
if(length(Contexte) != 1) stop("Différentes stations dans la chronique à analyser")

Contexte <- 
  data %>% 
  summarise(
    Annee = median(year(chmes_date))
  ) %>% 
  bind_cols(Contexte)

#### Pas d'agrégation ####
ValInstantanees <-
  data

#### Agrégation par jour ####
ValJours <- 
  data %>% 
  group_by(chmes_date) %>% 
  summarise(
    VMinJ = min(chmes_valeur),
    VMedJ = median(chmes_valeur),
    VMoyJ = mean(chmes_valeur),
    VMaxJ = max(chmes_valeur),
    VAmpliJ = VMaxJ-VMinJ,
    VarJ = var(chmes_valeur),
    NMesuresJ = n()
  )

#### Agrégation par mois ####
ValMois <- 
  data %>% 
  mutate(Mois = paste0(year(chmes_date), "-", month(chmes_date))) %>% 
  group_by(Mois) %>% 
  summarise(
    VMinM = min(chmes_valeur),
    VMedM = median(chmes_valeur),
    VMoyM = mean(chmes_valeur),
    VMaxM = max(chmes_valeur),
    VAmpliM = VMaxM-VMinM,
    VarM = var(chmes_valeur),
    NMesuresM = n()
  )

#### Agrégation par année biologique ####
ValAnneeBiol <-
  data %>% 
  formatage.annee.biologique() %>% 
  group_by(AnneeBiol) %>% 
  summarise(
    VMinAB = min(chmes_valeur),
    VMedAB = median(chmes_valeur),
    VMoyAB = mean(chmes_valeur),
    VMaxAB = max(chmes_valeur),
    VAmpliAB = VMaxAB-VMinAB,
    VarAB = var(chmes_valeur),
    NMesuresAB = n()
  )

#### Sortie des résultats ####
## Dataframe vers R


if(export == FALSE){
  return(list(ValInstantanees, ValJours, ValMois, ValAnneeBiol))
}

## Export vers xlsx ##
if(export == TRUE){
  l <- list(ValInstantanees = ValInstantanees, ValJours = ValJours, ValMois = ValMois, ValAnneeBiol = ValAnneeBiol)
  openxlsx::write.xlsx(l, file = paste0("./Sorties/Données/",Contexte$CodeRDT, "_données.xlsx"))
}

} # Fin de la fonction
