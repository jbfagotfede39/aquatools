#' Collecte de la dernière valeur de débit transmise
#'
#' Cette fonction permet d'extraire depuis Hydroréel la dernière valeur mesurée de débit d'une station donnée
#' @name chronique.hydroreel
#' @param station Numéro de la station
#' @keywords chronique
#' @import lubridate
#' @import rvest
#' @import stringr
#' @import dplyr
#' @export
#' @examples
#' chronique.hydroreel(Station)
#' chronique.hydroreel(433)
#' # Extraction des mesures à partir de http://www.rdbrmc.com/hydroreel2/listestation.php?dep=39

##### TODO LIST #####
# Nb de chiffres après la virgule : cas du Hérisson à 7,58 le 10/03/17 à 9h et seulement 7,5 retenu
#####################

chronique.hydroreel <- function(Station)
{
  # Station = 185
  # Station = 435
  # Station = listeStations[1]
  
  # Collecte de la page
  url <- paste0("http://www.rdbrmc.com/hydroreel2/station.php?codestation=",Station)
  hh <- read_html(url)
  
  # Parse HTML
  complet <-
    hh %>%
    html_nodes("div h5") %>%
    `[[`(2) %>% 
    html_text()

  # Test afin de savoir si on a bien des données de débits dans complet (cas problématiques des stations 35 à 39 car il y a un cadre de plus en haut de page)
  if(is.na(strsplit(complet, "(m3/s)")[[1]][2])){
    complet <-
      hh %>% 
      html_nodes("div h5") %>% 
      .[[3]] %>% # Ligne modifiée : on prend le cadre de dessous pour ne pas considérer les hauteurs d'eau
      html_text()
  }
  
  # Date
  chmes_date <- stringr::str_extract(complet, "([0-9]{2}/[0-9]{2}/[0-9]{4})") # Je cherche dans un bloc (parenthèses) un caractère numérique deux fois puis un slash un caractère numérique deux fois etc.
  chmes_date <- dmy(chmes_date)
  
  # Heure
  chmes_heure <- stringr::str_extract(complet, "([0-9]{2}:[0-9]{2})")
  chmes_heure <- paste0(chmes_heure,":00")
  
  # Valeur
  # Extraction du morceau de phrase contenant seulement la valeur
  complet <- strsplit(complet, "(m3/s)")[[1]][2]
  
  #Dans cet ordre, avec test de la longueur du truc à chaque fois ensuite si = NA on passe au suivant
  chmes_valeur <- stringr::str_extract(complet, "([0-9]{4}.[0-9])") # On cherche une valeur de débit à 4 chiffres
  if(is.na(chmes_valeur)) chmes_valeur <- stringr::str_extract(complet, "([0-9]{4})") # Si celle d'avant est vide, on cherche une valeur de débit à 4 chiffres sans virgule
  if(is.na(chmes_valeur)) chmes_valeur <- stringr::str_extract(complet, "([0-9]{3}.[0-9])") # Si celle d'avant est vide, on cherche une valeur de débit à 3 chiffres
  if(is.na(chmes_valeur)) chmes_valeur <- stringr::str_extract(complet, "([0-9]{2}.[0-9]{3})") # Si celle d'avant est vide, on cherche une valeur de débit à 2 chiffres + 3 //// Dernier
  if(is.na(chmes_valeur)) chmes_valeur <- stringr::str_extract(complet, "([0-9]{1}.[0-9]{3})") # Si celle d'avant est vide, on cherche une valeur de débit à 1 chiffres + 3 //// Dernier
  if(is.na(chmes_valeur)) chmes_valeur <- stringr::str_extract(complet, "([0-9]{3})") # Si celle d'avant est vide, on cherche une valeur de débit à 3 chiffres sans virgule
  if(is.na(chmes_valeur)) chmes_valeur <- stringr::str_extract(complet, "([0-9]{2}.[0-9]{2})") # Si celle d'avant est vide, on cherche une valeur de débit à 2 chiffres + 2
  if(is.na(chmes_valeur)) chmes_valeur <- stringr::str_extract(complet, "([0-9]{2}.[0-9]{1})") # Si celle d'avant est vide, on cherche une valeur de débit à 2 chiffres + 1
  #if(is.na(chmes_valeur)) chmes_valeur <- stringr::str_extract(complet, "([0-9]{2}.[0-9]{0})") # Si celle d'avant est vide, on cherche une valeur de débit à 2 chiffres sans virgule
  if(is.na(chmes_valeur)) chmes_valeur <- stringr::str_extract(complet, "([0-9]{1}.[0-9]{2})") # Si celle d'avant est vide, on cherche une valeur de débit à 1 chiffres + 2  
  if(is.na(chmes_valeur)) chmes_valeur <- stringr::str_extract(complet, "([0-9]{1}.[0-9]{1})") # Si celle d'avant est vide, on cherche une valeur de débit à 1 chiffres + 1 
  #if(is.na(chmes_valeur)) chmes_valeur <- stringr::str_extract(complet, "([0-9]{1}.[0-9]{0})") # Si celle d'avant est vide, on cherche une valeur de débit à 1 chiffres + 0
  
  # Station
  
  # Synthèse
  data <- data.frame(Station,chmes_date,chmes_heure,chmes_valeur) %>% rename(chmes_coderhj = Station)
  data$chmes_valeur <- as.numeric(levels(data$chmes_valeur))[data$chmes_valeur]
  data$chmes_date <- as.character(data$chmes_date)
  data$chmes_heure <- as.character(data$chmes_heure)
  data$chmes_coderhj <- as.character(data$chmes_coderhj)
  
  # Retour
  return(data)
  
} # Fin de la fonction