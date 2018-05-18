#' Agrégation de chroniques
#'
#' Cette fonction permet d'agréger des chroniques de mesures (température, niveaux, etc.)
#' 
#' @param data Data.frame contenant a minima une colonne Date, une colonne Heure et une colonne Valeur
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
  mutate(Date = ymd(Date)) %>% 
  mutate(Time = ymd_hms(paste(Date, Heure, sep = "_"))) %>% 
  filter(is.na(Valeur) == F) %>% 
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
    Annee = median(year(Date))
  ) %>% 
  bind_cols(Contexte)

#### Pas d'agrégation ####
ValInstantanees <-
  data

#### Agrégation par jour ####
ValJours <- 
  data %>% 
  group_by(Date) %>% 
  summarise(
    VMinJ = min(Valeur),
    VMedJ = median(Valeur),
    VMoyJ = mean(Valeur),
    VMaxJ = max(Valeur),
    VAmpliJ = VMaxJ-VMinJ,
    VarJ = var(Valeur),
    NMesuresJ = n()
  )

#### Agrégation par mois ####
ValMois <- 
  data %>% 
  mutate(Mois = paste0(year(Date), "-", month(Date))) %>% 
  group_by(Mois) %>% 
  summarise(
    VMinM = min(Valeur),
    VMedM = median(Valeur),
    VMoyM = mean(Valeur),
    VMaxM = max(Valeur),
    VAmpliM = VMaxM-VMinM,
    VarM = var(Valeur),
    NMesuresM = n()
  )

#### Agrégation par année biologique ####
ValAnneeBiol <-
  data %>% 
  formatage.annee.biologique() %>% 
  group_by(AnneeBiol) %>% 
  summarise(
    VMinAB = min(Valeur),
    VMedAB = median(Valeur),
    VMoyAB = mean(Valeur),
    VMaxAB = max(Valeur),
    VAmpliAB = VMaxAB-VMinAB,
    VarAB = var(Valeur),
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
