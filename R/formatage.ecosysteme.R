#' Mise en acronymes des écosystèmes ou inversement
#'
#' Cette fonction permet de mettre en acronymes des noms d'écosystèmes ou de transformer des acronymes en noms des écosystèmes
#' @name formatage.ecosysteme
#' @export
#' @import stringr
#' @import tidyverse
#' @examples
#' formatage.ecosysteme(data$Station, Type = "Simplification")
#' PC$pcmes_coderhj <- formatage.ecosysteme(PC$pcmes_coderhj, Type = "Simplification")
#' data %>% stations.CodeRDT(DistSource = F) %>% formatage.ecosysteme(Type = "Expansion")

##### TODO LIST #####
# La mise en acronymes devrait moyennement fonctionner, car se basait précédemment que sur une colonne
#####################

formatage.ecosysteme <- function(
  acronymes = data,
  Type = c("Simplification", "Expansion")
  )
  {

  ## Évaluation des choix
  Type <- match.arg(Type)

  ##### Expansion #####
if(Type == "Expansion"){
acronymes <-
  acronymes %>% 
  left_join(formatage.abreviation() %>% filter(Type == "Écosystème"), by = c(codemilieu = "Acronyme")) %>% 
  select(-Type, -codemilieu) %>% 
  rename(Ecosysteme = Definition)
}

  ##### Simplification #####
if(Type == "Simplification"){
  #### Mise en minuscule ####
  # Afin de s'affranchir des problèmes de casse
  acronymes <-
    acronymes %>% 
    str_to_lower()

  #### Détection et remplacement ####
acronymes[str_detect(acronymes, "abbaye")] <- "ABB"
acronymes[str_detect(acronymes, "antre")] <- "ANT"
acronymes[str_detect(acronymes, "assencière")] <- "ASS"
acronymes[str_detect(acronymes, "bellefontaine")] <- "BEL"
acronymes[str_detect(acronymes, "bez")] <- "BEZ"
acronymes[str_detect(acronymes, "bonlieu")] <- "BON"
acronymes[str_detect(acronymes, "brenet")] <- "BRN"
acronymes[str_detect(acronymes, "chalain")] <- "CHN"
acronymes[str_detect(acronymes, "chanon")] <- "CAO"
acronymes[str_detect(acronymes, "clairvaux pet")] <- "PCL"
acronymes[str_detect(acronymes, "clairvaux grd")] <- "GCL"
acronymes[str_detect(acronymes, "chambly")] <- "CHY"
acronymes[str_detect(acronymes, "dame")] <- "DAM"
acronymes[str_detect(acronymes, "embouteilleux")] <- "EMB"
acronymes[str_detect(acronymes, "etival")] <- "GET"
acronymes[str_detect(acronymes, "petit etival")] <- "PET"
acronymes[str_detect(acronymes, "fauge")] <- "FAU"
acronymes[str_detect(acronymes, "plasne")] <- "FDP"
acronymes[str_detect(acronymes, "fioget")] <- "FIO"
acronymes[str_detect(acronymes, "ilay")] <- "ILA"
acronymes[str_detect(acronymes, "lamoura")] <- "LAM"
acronymes[str_detect(acronymes, "lautrey")] <- "LAU"
acronymes[str_detect(acronymes, "petit maclu")] <- "PMA"
acronymes[str_detect(acronymes, "grand maclu")] <- "GMA"
acronymes[str_detect(acronymes, "mortes")] <- "MOR"
acronymes[str_detect(acronymes, "narlay")] <- "NAR"
acronymes[str_detect(acronymes, "onoz")] <- "ONO"
acronymes[str_detect(acronymes, "penne")] <- "PEN"
acronymes[str_detect(acronymes, "ratay")] <- "RAT"
acronymes[str_detect(acronymes, "truites")] <- "RGT"
acronymes[str_detect(acronymes, "rousses")] <- "ROU"
acronymes[str_detect(acronymes, "rosay")] <- "ROS"
acronymes[str_detect(acronymes, "val")] <- "LVA"
acronymes[str_detect(acronymes, "vernois")] <- "VER"
acronymes[str_detect(acronymes, "viremont")] <- "VIR"
acronymes[str_detect(acronymes, "viry")] <- "VRY"
acronymes[str_detect(acronymes, "blye")] <- "BLY"
acronymes[str_detect(acronymes, "bolozon")] <- "CIZ"
acronymes[str_detect(acronymes, "coiselet")] <- "COI"
acronymes[str_detect(acronymes, "cuttura")] <- "CUT"
acronymes[str_detect(acronymes, "etables")] <- "ETA"
acronymes[str_detect(acronymes, "lavancia")] <- "LAV"
acronymes[str_detect(acronymes, "ravilloles")] <- "RAV"
acronymes[str_detect(acronymes, "mortier")] <- "MOT"
acronymes[str_detect(acronymes, "vouglans")] <- "VOU"

  #### Test de complétude ####
  if(all(grepl("^[[:upper:]]+$", acronymes)) != T) warning(paste0("Certain(s) cas non traité(s) : ",unique(acronymes[!grepl("^[[:upper:]]+$", acronymes)])))
}
  
#### Retour du tableau complet ####
return(acronymes)
  
} # Fin de la fonction