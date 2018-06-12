#' Remplacement d'abréviations
#'
#' Cette fonction permet d'exporter la définition des acronymes
#' @export
#' @import tidyverse
#' @examples
#' formatage.abreviation()
#' 
#' NomsColonnes <- colnames(Operations)
#' acronymes <- formatage.abreviation()
#' Operations <-
#'   Operations %>%
#'   left_join(acronymes, by = c(Station = "Acronyme")) %>%
#'   select(-Station) %>%
#'   rename(Station = Definition)

#' Operations <-
#'   Operations %>%
#'   select(match(NomsColonnes,names(.)))

##### TODO LIST #####
# Intégration d'un paramètre colonne (contenant les abréviations permettant de retourner directement le dataframe avec la colonne abréviation transformée avec la définition
#####################

formatage.abreviation <- function(){

  #### Import ####
  if(file.exists("/Users/imac27/NAS-JB/Outils/Informatique/Latex/acronymes.tex") == T) acronymes <- read_lines("/Users/imac27/NAS-JB/Outils/Informatique/Latex/acronymes.tex")
  if(file.exists("/Users/jean-baptistefagot_FD39/NAS-JB/Outils/Informatique/Latex/acronymes.tex") == T) acronymes <- read_lines("/Users/jean-baptistefagot_FD39/NAS-JB/Outils/Informatique/Latex/acronymes.tex")
  
  if(file.exists("/Users/imac27/NAS-JB/Outils/Informatique/Latex/acronymesEsp.tex") == T) acronymesEsp <- read_lines("/Users/imac27/NAS-JB/Outils/Informatique/Latex/acronymesEsp.tex")
  if(file.exists("/Users/jean-baptistefagot_FD39/NAS-JB/Outils/Informatique/Latex/acronymesEsp.tex") == T) acronymesEsp <- read_lines("/Users/jean-baptistefagot_FD39/NAS-JB/Outils/Informatique/Latex/acronymesEsp.tex")
  
  if(file.exists("/Users/imac27/NAS-JB/Outils/Informatique/Latex/acronymesCE.tex") == T) acronymesCE <- read_lines("/Users/imac27/NAS-JB/Outils/Informatique/Latex/acronymesCE.tex")
  if(file.exists("/Users/jean-baptistefagot_FD39/NAS-JB/Outils/Informatique/Latex/acronymesCE.tex") == T) acronymesCE <- read_lines("/Users/jean-baptistefagot_FD39/NAS-JB/Outils/Informatique/Latex/acronymesCE.tex")
  
  #### Transformation en dataframe ####
  acronymes <- as.tbl(as.data.frame(acronymes)) %>% mutate(Type = "Autre")
  acronymesEsp <- as.tbl(as.data.frame(acronymesEsp)) %>% mutate(Type = "Espèce")
  acronymesCE <- as.tbl(as.data.frame(acronymesCE)) %>% mutate(Type = "Écosystème")
  
  #### Fusion des deux morceaux ####
  acronymes <- 
    acronymes %>% 
    bind_rows(rename(acronymesCE, acronymes = acronymesCE)) %>% 
    bind_rows(rename(acronymesEsp, acronymes = acronymesEsp))
  
  #### Filtrage des valeurs commentées (%) ####
  acronymes <- 
  acronymes %>% 
    rename(Complet = acronymes) %>% 
    filter(!grepl("^%.*", Complet)) 
  
  #### Mise en deux colonnes ####
  Acronyme <- gsub(".*\\}\\{(.*)\\}\\{.*", "\\1", acronymes$Complet)
  Definition <- gsub(".*\\}\\{(.*)\\}$", "\\1", acronymes$Complet)
  Type <- acronymes$Type
  acronymes <- data.frame(Acronyme, Definition, Type, stringsAsFactors = FALSE)
  
  #### Nettoyage ####
  rm(Acronyme)
  rm(Definition)
  rm(Type)
  
  acronymes <- 
    acronymes %>% 
    filter(Acronyme != "")
  
  #### Retour du tableau complet ####
  return(acronymes)
  
} # Fin de la fonction
