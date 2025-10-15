#' Import des mesures de scalimétrie issues du logiciel Mosaic
#'
#' Cette fonction permet d'importer et de formater les mesures de scalimétrie réalisée à l'aide du logiciel Mosaic de la loupe binoculaire avec caméra
#' @name scalimetrie.import
#' @param file Fichier à lire
#' @keywords scalimétrie poissons
#' @import tidyverse 
#' @export
#' @examples
#' mesures <- fs::dir_ls(data_dir, regexp = glue("\\{extension}$")) %>% map(~ scalimetrie.import(.)) %>% list_rbind()

scalimetrie.import <- function(
    file = NA_character_
)
{
  
  #### Test de cohérence ####
  if(is.na(file)) stop("Un nom de fichier doit être renseigné")
  
  #### Contexte ####
  filename <- basename(file)
  
  #### Collecte des données ####
  data <- 
    read_excel(file, skip = 35) %>%
    mutate(Name = filename) %>% # Ajouter le nom du fichier dans la colonne Name
    mutate(Length_μm = as.numeric(Length_μm))
  
  #### Nettoyage & reformatage ####
  # Retourner les valeurs mal formées si elles existent
  noms_problemes <- 
    data %>%
    filter(str_count(Name, "_") != 2) %>%
    pull(Name)
  
  # si jamais il ya un fichier avec un nom mal formé : 
  if (length(noms_problemes) > 0) {
    message("Noms mal formés dans le fichier : ", filename)
    print(noms_problemes)
  }
  
  # Continuer le traitement uniquement pour les noms bien formés
  data_v2 <- 
    data %>%
    filter(str_count(Name, "_") == 2) %>%
    separate(Name, into = c("id", "id_poissons", "type"), sep = "_", remove = TRUE) %>% 
    mutate(type = str_replace(type, ".xlsx", "")) %>% 
    mutate(type = str_replace(type, ".txt", ""))
  
  #### Sortie ####
  return(data_v2)
  
} # Fin de la fonction