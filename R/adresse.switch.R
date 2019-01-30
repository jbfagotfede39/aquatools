#' Switcher de localisation de fichier
#'
#' Cette fonction permet de retrouver automatique l'adresse d'un fichier entre l'ordi portable et l'ordi fixe
#' 
#' @name adresse.switch
#' @param url Adresse du fichier à ouvrir
#' @export
#' @examples
#' adresse.switch("NAS-JB/Études/2014_Les Rousses/Résultats/Poissons/ResultatsRoussestotal-28janvier2015.xlsx")
#' adresse.switch("NAS-DATA/Poissons/PDPG/Stations PDPG_V1.xlsx")
#' fish <- read_excel(adresse.switch("NAS-JB/Études/2014_Les Rousses/Résultats/Poissons/ResultatsRoussestotal-28janvier2015.xlsx"), sheet = 2)

##### TODO LIST #####
# 
#####################

adresse.switch <- function(
  url="")
  
{
  
  #### Test de la présence d'une URL ####
  #if(nchar(url) == 0) stop("Attention : pas d'url saisie")
  if(nchar(url) == 0) url = readline(prompt = "Saisir une adresse de fichier : ")
  
  #### Transformation du format ####
  if(file.exists(paste0("/Users/imac27/",url))) url <- paste0("/Users/imac27/",url) # Machine fixe JB
  if(file.exists(paste0("/Volumes/Fixe-FD39/",url))) url <- paste0("/Volumes/Fixe-FD39/",url) # Machine portable JB
  if(file.exists(paste0("/Users/adrienlavigne/",url))) url <- paste0("/Users/adrienlavigne/",url) # Machine Adrien
  if(file.exists(paste0("/Users/quentinducreux/",url))) url <- paste0("/Users/quentinducreux/",url) # Machine portable Quentin
  
  #### Retour de l'adresse correcte ####
  return(url)
  
} # Fin de la fonction