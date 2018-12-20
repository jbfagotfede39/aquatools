#' Ouverture de fichiers de chroniques
#'
#' Cette fonction permet d'ouvrir de manière semi-automatisée des fichiers de chroniques
#' @name chronique.ouverture
#' @param localisation Localisation relative du fichier (à partir de /NAS-DATA/)
#' @keywords chronique
#' @import tidyverse
#' @export
#' @examples
#' chronique.ouverture("/NAS-DATA/Chroniques/Saisie_JB/2018-12-17_Suivi_Ain_amont_FJPPMA_été_2018/10880567.txt")

chronique.ouverture <- function(
  localisation = as.character(NA)
)
{
  
##### -------------- A FAIRE -------------- #####
# 
# -------------- A FAIRE -------------- #
  
#### Chargement des données ####
if(testit::has_warning(read_delim(adresse.switch(ouverture), skip = 2, col_names = c("Date","Heure", "Valeur","b"), delim=";")) == FALSE) data <- read_delim(adresse.switch(ouverture), skip = 2, col_names = c("Date","Heure", "Valeur","b"), delim=";")
if(all(exists("data") & testit::has_warning(read_delim(adresse.switch(ouverture), skip = 2, col_names = c("Date","Heure", "Valeur","b", "c"), delim=";"))) == FALSE) data <- read_delim(adresse.switch(ouverture), skip = 2, col_names = c("Date","Heure", "Valeur","b", "c"), delim=";")
if(all(exists("data") & testit::has_warning(read_delim(adresse.switch(ouverture), skip = 2, col_names = c("Date","Heure", "Valeur","b", "c", "d"), delim=";"))) == FALSE) data <- read_delim(adresse.switch(ouverture), skip = 2, col_names = c("Date","Heure", "Valeur","b", "c", "d"), delim=";")
if(all(exists("data") & testit::has_warning(read_delim(adresse.switch(ouverture), skip = 2, col_names = c("Date","Heure", "Valeur","b", "c", "d", "e"), delim=";"))) == FALSE) data <- read_delim(adresse.switch(ouverture), skip = 2, col_names = c("Date","Heure", "Valeur","b", "c", "d", "e"), delim=";")
if(all(exists("data") & testit::has_warning(read_delim(adresse.switch(ouverture), skip = 2, col_names = c("Date","Heure", "Valeur","b", "c", "d", "e", "f"), delim=";"))) == FALSE) data <- read_delim(adresse.switch(ouverture), skip = 2, col_names = c("Date","Heure", "Valeur","b", "c", "d", "e", "f"), delim=";")

if(exists("data") == FALSE) stop("Scénario d'importation à développer")

#### Nettoyage ####
data <-
  data %>% 
  select(Date, Heure, Valeur) %>%
  mutate(Date = ymd(Date)) %>% 
  mutate(Heure = as.character(Heure)) %>% 
  mutate(Valeur = as.numeric(sub(",", ".", Valeur))) %>% 
  mutate(Valeur = round(Valeur,3)) %>% 
  filter(is.na(Valeur) != T)
  
#### Transformation des champs ####
colnames(data) <- 
  data %>% 
  colnames() %>% 
  paste0("chmes_",.) %>% 
  gsub("[[:punct:]]", "_", .) %>% 
  tolower()
  
#### Sortie des résultats ####
return(data)
  
} # Fin de la fonction