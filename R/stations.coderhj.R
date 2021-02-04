#' Extraire la distance à la source ou le code écosystème du coderhj
#'
#' Cette fonction permet d'extraire la distance à la source ou le code écosystème à partir du coderhj
#' @name stations.coderhj
#' @param data Dataframe contenant au moins la colonne ColonneEntree
#' @param DistSource Si \code{T} (par défaut), extrait la distance à la source - Extrait sinon le code de l'écosystème
#' @param ColonneEntree Champ contenant la donnée d'entrée
#' @param ColonneSortie Champ recevant la donnée de sortie (peut être identique au champs d'entrée si on le souhaite)
#' @keywords stations
#' @import stringr
#' @import tidyverse
#' @export
#' @examples
#' stations.coderhj(data, ColonneEntree = "test", ColonneSortie = "V3")
#' stations.coderhj(data, ColonneEntree = "test", ColonneSortie = "test")
#' stations.coderhj(data, ColonneEntree = "test", ColonneSortie = "V3", DistSource = F)
#' stations.coderhj(data, ColonneEntree = "test", ColonneSortie = "test", DistSource = F)

##### TODO LIST #####
# [A-Z]* doit fonctionner, plus simple que trois lignes...
# Faire que les champs soient rangés après comme au départ, en enregistant préalablement à part le dataframe d'entrée puis select(matches)
# 
#####################

stations.coderhj <- function(
  data,
  DistSource = T,
  ColonneEntree = NA_character_,
  ColonneSortie = NA_character_
  )
{

  #### Test de vérification ####
if(nchar(ColonneEntree) == 0 | is.na(ColonneEntree)) stop("Pas de champs en entrée")
if(nchar(ColonneSortie) == 0 | is.na(ColonneSortie)) stop("Pas de champs de sortie")
if(ColonneEntree %in% names(data) == FALSE) stop(paste0("Le champs ", ColonneEntree, " est absent du dataframe d'entrée"))
  
  #### Renommage provisoire ####
  dataNomColonnes <- data
  data <- data %>% rename(coderhj := !!ColonneEntree)

  #### Extraction de la distance à la source ####
  if(DistSource == T){
    data <-
      data %>%
      mutate(Sortie = str_extract(coderhj, "[0-9][0-9][0-9]-[0-9]")) %>%
      mutate(Sortie = ifelse(is.na(Sortie), str_extract(coderhj, "[0-9][0-9]-[0-9]"), Sortie)) %>%
      mutate(Sortie = ifelse(is.na(Sortie), str_extract(coderhj, "[0-9]-[0-9]"), Sortie)) %>%
      mutate(Sortie = str_replace(Sortie, "-", ".")) %>%
      mutate(Sortie = as.numeric(Sortie))
  }
  
  #### Extraction du code écosystème ####
  if(DistSource == F){
    data <-
      data %>% 
      mutate(temp := str_replace_all(coderhj, "[-]","")) %>% 
      mutate(Sortie = str_extract(temp, "[A-Z][A-Z][A-Z][A-Z]")) %>% 
      mutate(Sortie = ifelse(is.na(Sortie), str_extract(temp, "[A-Z][A-Z][A-Z]"), Sortie)) %>% 
      mutate(Sortie = ifelse(grepl(" A ", coderhj), NA, Sortie)) %>% # Pour négliger les coderhj qui contiennent en réalité le nom DCE de la station
      dplyr::select(-temp) %>% 
      mutate(Sortie = as.character(Sortie))
  }

  #### Renommage final ####
  if(ColonneEntree == ColonneSortie){data <- data %>% dplyr::select(-coderhj) %>% rename(!!ColonneSortie := Sortie)}
  if(ColonneEntree != ColonneSortie){
    if(ColonneSortie %in% colnames(data) == TRUE){data <- data %>% dplyr::select(-ends_with(ColonneSortie))} # Attention l'ordre de ces deux étapes est important, car on fait disparaître ColonneSortie qui est donc ensuite généré car absent - On utilise ends_with au lieu de matches pour le cas de chsta_distancesource qui couvre également chsta_distancesource_confluencedrainprincipal et qui supprime cette dernière sans qu'on le veuille
    if(!(ColonneSortie %in% colnames(data))){data <- data %>% rename(!!ColonneEntree := coderhj, !!ColonneSortie := Sortie)} # Attention l'ordre de ces deux étapes est important
  }
  
  #### Ré-ordonnancement ####
  if(ColonneSortie %in% colnames(dataNomColonnes)){data <- data %>% dplyr::select(match(colnames(dataNomColonnes),names(.)))}
  if(!(ColonneSortie %in% colnames(dataNomColonnes))){data <- data %>% dplyr::select(match(colnames(dataNomColonnes),names(.)), matches(ColonneSortie))}
  

  #### Restitution des données ####
  return(data)
  
} # Fin de la fonction