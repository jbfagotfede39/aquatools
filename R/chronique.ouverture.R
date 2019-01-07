#' Ouverture de fichiers de chroniques
#'
#' Cette fonction permet d'ouvrir de manière semi-automatisée des fichiers de chroniques
#' @name chronique.ouverture
#' @param localisation Localisation relative du fichier (à partir de /NAS-dataaimporter/)
#' @keywords chronique
#' @import tidyverse
#' @export
#' @examples
#' chronique.ouverture("/NAS-dataaimporter/Chroniques/Saisie_JB/2018-12-17_Suivi_Ain_amont_FJPPMA_été_2018/10880567.txt")

chronique.ouverture <- function(
  localisation = as.character(NA)
)
{
  
##### -------------- A FAIRE -------------- #####
# Essayer de supprimer l'affichage des tests d'exécution.
# 
# -------------- A FAIRE -------------- #
  
#### Chargement des données ####
if(testit::has_warning(read_delim(adresse.switch(ouverture), skip = 2, col_names = c("Date","Heure", "Valeur","b"), delim=";")) == FALSE) dataaimporter <- read_delim(adresse.switch(ouverture), skip = 2, col_names = c("Date","Heure", "Valeur","b"), delim=";")
if(exists("dataaimporter") == FALSE){
  if(testit::has_warning(read_delim(adresse.switch(ouverture), skip = 2, col_names = c("Date","Heure", "Valeur","b", "c"), delim=";")) == FALSE) dataaimporter <- read_delim(adresse.switch(ouverture), skip = 2, col_names = c("Date","Heure", "Valeur","b", "c"), delim=";")}
if(exists("dataaimporter") == FALSE){
  if(testit::has_warning(read_delim(adresse.switch(ouverture), skip = 2, col_names = c("Date","Heure", "Valeur","b", "c", "d"), delim=";")) == FALSE) dataaimporter <- read_delim(adresse.switch(ouverture), skip = 2, col_names = c("Date","Heure", "Valeur","b", "c", "d"), delim=";")}
if(exists("dataaimporter") == FALSE){
  if(testit::has_warning(read_delim(adresse.switch(ouverture), skip = 2, col_names = c("Date","Heure", "Valeur","b", "c", "d", "e"), delim=";")) == FALSE) dataaimporter <- read_delim(adresse.switch(ouverture), skip = 2, col_names = c("Date","Heure", "Valeur","b", "c", "d", "e"), delim=";")}
if(exists("dataaimporter") == FALSE){
  if(testit::has_warning(read_delim(adresse.switch(ouverture), skip = 2, col_names = c("Date","Heure", "Valeur","b", "c", "d", "e", "f"), delim=";")) == FALSE) dataaimporter <- read_delim(adresse.switch(ouverture), skip = 2, col_names = c("Date","Heure", "Valeur","b", "c", "d", "e", "f"), delim=";")}

if(exists("dataaimporter") == FALSE) stop("Scénario d'importation à développer")

#### Nettoyage ####
dataaimporter <-
  dataaimporter %>% 
  select(Date, Heure, Valeur) %>%
  mutate(Date = ymd(Date)) %>% 
  mutate(Heure = as.character(Heure)) %>% 
  mutate(Valeur = as.numeric(sub(",", ".", Valeur))) %>% 
  mutate(Valeur = round(Valeur,3)) %>% 
  filter(is.na(Valeur) != T)
  
#### Transformation des champs ####
colnames(dataaimporter) <- 
  dataaimporter %>% 
  colnames() %>% 
  paste0("chmes_",.) %>% 
  gsub("[[:punct:]]", "_", .) %>% 
  tolower()
  
#### Sortie des résultats ####
return(dataaimporter)
  
} # Fin de la fonction