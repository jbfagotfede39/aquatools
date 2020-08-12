#' Mise en data des écosystèmes ou inversement
#'
#' Cette fonction permet de mettre en data des noms d'écosystèmes ou de transformer des data en noms des écosystèmes
#' @name formatage.ecosysteme
#' @param data Dataframe contenant les données à transformer
#' @param Operation Type de transformation que l'on souhaite réaliser (\code{Simplification} ou \code{Expansion})
#' @param ColonneEntree Champ contenant la donnée d'entrée
#' @param ColonneSortie Champ recevant la donnée de sortie (peut être identique au champs d'entrée si on le souhaite)
#' @import glue
#' @import stringr
#' @import tidyverse
#' @export
#' @examples
#' formatage.ecosysteme(data, Operation = "Expansion", ColonneEntree = "chsta_coderhj", ColonneSortie = "chsta_ecosysteme")
#' formatage.ecosysteme(data, Operation = "Simplification", ColonneEntree = "chsta_ecosysteme", ColonneSortie = "chsta_ecosysteme")

##### TODO LIST #####
# Ne fonctionne pour l'instant pas avec les entrées géographiques
#####################

formatage.ecosysteme <- function(
  data,
  Operation = c("Simplification", "Expansion"),
  ColonneEntree = NA_character_,
  ColonneSortie = NA_character_
  )
  {

  ## Évaluation des choix
  Operation <- match.arg(Operation)
  
  ## Tests ##
if(nchar(ColonneEntree) == 0) stop("Pas de champs en entrée")
if(nchar(ColonneSortie) == 0) stop("Pas de champs de sortie")
if(ColonneEntree %in% names(data) == FALSE) stop(paste0("Le champs ", ColonneEntree, " est absent du dataframe d'entrée"))

  ##### Expansion #####
if(Operation == "Expansion"){

  dataNomColonnes <- data
  
  data <-
    data %>% 
    rename(coderhj := !!ColonneEntree) %>% 
    stations.coderhj(ColonneEntree = "coderhj", ColonneSortie = "coderhj2", DistSource = F) %>% # Permet de supprimer les distances à la source des stations qui en auraient
    rename(!!ColonneEntree := coderhj) %>% 
    rename(coderhj := coderhj2) %>% 
    left_join(formatage.abreviation(thematique = "Écosystème", formatage = "Propre"), by = c("coderhj" = "Abréviation")) %>% 
    rename(Sortie = Définition)

  
  #### Test de complétude ####
  test <- data %>% dplyr::filter(is.na(Sortie))
  if(nrow(test) != 0) warning(paste0("Présence d'écosystème(s) impossible(s) à étendre : "), glue_collapse(unique(test$coderhj), ", ", last = " et "))
  
#### Renommage final ####
if(ColonneEntree == ColonneSortie){data <- data %>% select(-coderhj) %>% select(-matches(ColonneEntree)) %>% rename(!!ColonneSortie := Sortie)}
if(ColonneEntree != ColonneSortie){
  if(ColonneSortie %in% colnames(data) == TRUE){data <- data %>% select(-matches(ColonneSortie))} # Attention l'ordre de ces deux étapes est important, car on fait disparaître ColonneSortie qui est donc ensuite généré car absent
  if(!(ColonneSortie %in% colnames(data))){data <- data %>% rename(!!ColonneSortie := Sortie)} # Attention l'ordre de ces deux étapes est important
}
  
  #### Ré-ordonnancement ####
  if(ColonneSortie %in% colnames(dataNomColonnes)){data <- data %>% select(match(colnames(dataNomColonnes),names(.)))}
  if(!(ColonneSortie %in% colnames(dataNomColonnes))){data <- data %>% select(match(colnames(dataNomColonnes),names(.)), matches(ColonneSortie))}
  
}
  
  ##### Simplification #####
if(Operation == "Simplification"){
  
  dataNomColonnes <- data
  
  #### Mise en minuscule ####
  # Afin de s'affranchir des problèmes de casse
  
  DataRef <- 
    formatage.abreviation(thematique = "Écosystème", formatage = "Propre") %>% 
    mutate(definition = str_to_lower(Définition))
  
  data <-
    data %>% 
    rename(definitionoriginale := !!ColonneEntree) %>% 
    mutate(definitionsale = str_to_lower(definitionoriginale)) %>% 
    left_join(DataRef, by = c("definitionsale" = "definition")) %>% 
    select(-definitionsale)
  
  #### Exceptions manuelles ####
  data <-
    data %>%
    dplyr::filter(is.na(Abréviation)) %>% 
    select(-(Abréviation:Définition)) %>% 
    mutate(definitionoriginale = case_when(
      grepl("Abbaye", definitionoriginale) ~ "Lac de l'Abbaye",
      grepl("Antre", definitionoriginale) ~ "Lac d'Antre",
      grepl("ascencière|assencière", definitionoriginale) ~ "Lac de l'Assencière",
      grepl("Ilay|Motte", definitionoriginale) ~ "Lac d'Ilay",
      grepl("Val", definitionoriginale) & grepl("lac", definitionoriginale) ~ "Lac du Val"
    )
    ) %>% 
    mutate(definitionsale = str_to_lower(definitionoriginale)) %>% 
    left_join(DataRef, by = c("definitionsale" = "definition")) %>% 
    select(-definitionsale) %>% 
    dplyr::union(data %>% dplyr::filter(!is.na(Abréviation))) # Ne fonctionne pour l'instant pas avec les entrées géographiques -> union impossible

  #### Exceptions manuelles ####
# data[str_detect(data, "bellefontaine")] <- "BEL"
# data[str_detect(data, "bez")] <- "BEZ"
# data[str_detect(data, "bonlieu")] <- "BON"
# data[str_detect(data, "brenet")] <- "BRN"
# data[str_detect(data, "chalain")] <- "CHN"
# data[str_detect(data, "chanon")] <- "CAO"
# data[str_detect(data, "clairvaux pet")] <- "PCL"
# data[str_detect(data, "clairvaux grd")] <- "GCL"
# data[str_detect(data, "chambly")] <- "CHY"
# data[str_detect(data, "dame")] <- "DAM"
# data[str_detect(data, "embouteilleux")] <- "EMB"
# data[str_detect(data, "etival")] <- "GET"
# data[str_detect(data, "petit etival")] <- "PET"
# data[str_detect(data, "fauge")] <- "FAU"
# data[str_detect(data, "plasne")] <- "FDP"
# data[str_detect(data, "fioget")] <- "FIO"
# data[str_detect(data, "lamoura")] <- "LAM"
# data[str_detect(data, "lautrey")] <- "LAU"
# data[str_detect(data, "petit maclu")] <- "PMA"
# data[str_detect(data, "grand maclu")] <- "GMA"
# data[str_detect(data, "mortes")] <- "MOR"
# data[str_detect(data, "narlay")] <- "NAR"
# data[str_detect(data, "onoz")] <- "ONO"
# data[str_detect(data, "penne")] <- "PEN"
# data[str_detect(data, "ratay")] <- "RAT"
# data[str_detect(data, "truites")] <- "RGT"
# data[str_detect(data, "rousses")] <- "ROU"
# data[str_detect(data, "rosay")] <- "ROS"
# data[str_detect(data, "vernois")] <- "VER"
# data[str_detect(data, "viremont")] <- "VIR"
# data[str_detect(data, "viry")] <- "VRY"
# data[str_detect(data, "blye")] <- "BLY"
# data[str_detect(data, "bolozon")] <- "CIZ"
# data[str_detect(data, "coiselet")] <- "COI"
# data[str_detect(data, "cuttura")] <- "CUT"
# data[str_detect(data, "etables")] <- "ETA"
# data[str_detect(data, "lavancia")] <- "LAV"
# data[str_detect(data, "ravilloles")] <- "RAV"
# data[str_detect(data, "mortier")] <- "MOT"
# data[str_detect(data, "vouglans")] <- "VOU"

  #### Test de complétude ####
test <- data %>% dplyr::filter(is.na(Abréviation))
if(nrow(test) != 0) warning(paste0("Présence d'écosystème(s) impossible(s) à simplifier : "), glue::glue_collapse(unique(test$definitionoriginale), ", ", last = " et "))

  #### Renommage final ####
  if(ColonneEntree == ColonneSortie){data <- data %>% select(-definitionoriginale) %>% select(-matches(ColonneEntree)) %>% rename(!!ColonneSortie := Abréviation)}
  if(ColonneEntree != ColonneSortie){
    if(ColonneSortie %in% colnames(data) == TRUE){data <- data %>% select(-matches(ColonneSortie))} # Attention l'ordre de ces deux étapes est important, car on fait disparaître ColonneSortie qui est donc ensuite généré car absent
    if(!(ColonneSortie %in% colnames(data))){data <- data %>% rename(!!ColonneEntree := definitionoriginale, !!ColonneSortie := Abréviation)} # Attention l'ordre de ces deux étapes est important
  }
  
  #### Ré-ordonnancement ####
  if(ColonneSortie %in% colnames(dataNomColonnes)){data <- data %>% select(match(colnames(dataNomColonnes),names(.)))}
  if(!(ColonneSortie %in% colnames(dataNomColonnes))){data <- data %>% select(match(colnames(dataNomColonnes),names(.)), matches(ColonneSortie))}

}
  
#### Retour du tableau complet ####
return(data)
  
} # Fin de la fonction