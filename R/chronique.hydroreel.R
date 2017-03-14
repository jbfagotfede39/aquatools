#' Collecte de la dernière valeur de débit transmise
#'
#' Cette fonction permet d'extraire depuis Hydroréel la dernière valeur mesurée de débit d'une station donnée
#' 
#' @param station Numéro de la station
#' @keywords chronique
#' @import lubridate
#' @import rvest
#' @import stringr
#' @import tidyverse
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
    html_nodes(".cadre") %>%
    `[[`(2) %>% 
    html_text()

  # Test afin de savoir si on a bien des données de débits dans complet (cas problématiques des stations 35 à 39 car il y a un cadre de plus en haut de page)
  if(is.na(strsplit(complet, "(m3/s)")[[1]][2])){
    complet <-
      hh %>% 
      html_nodes(".cadre") %>% 
      .[[3]] %>% # Ligne modifiée : on prend le cadre de dessous pour ne pas considérer les hauteurs d'eau
      html_text()
  }
  
  # Date
  Date <- stringr::str_extract(complet, "([0-9]{2}/[0-9]{2}/[0-9]{4})") # Je cherche dans un bloc (parenthèses) un caractère numérique deux fois puis un slash un caractère numérique deux fois etc.
  Date <- dmy(Date)
  
  # Heure
  Heure <- stringr::str_extract(complet, "([0-9]{2}:[0-9]{2})")
  Heure <- paste0(Heure,":00")
  
  # Valeur
  # Extraction du morceau de phrase contenant seulement la valeur
  complet <- strsplit(complet, "(m3/s)")[[1]][2]
  
  #Dans cet ordre, avec test de la longueur du truc à chaque fois ensuite si = NA on passe au suivant
  Valeur <- stringr::str_extract(complet, "([0-9]{4}.[0-9])") # On cherche une valeur de débit à 4 chiffres
  if(is.na(Valeur)) Valeur <- stringr::str_extract(complet, "([0-9]{4})") # Si celle d'avant est vide, on cherche une valeur de débit à 4 chiffres sans virgule
  if(is.na(Valeur)) Valeur <- stringr::str_extract(complet, "([0-9]{3}.[0-9])") # Si celle d'avant est vide, on cherche une valeur de débit à 3 chiffres
  if(is.na(Valeur)) Valeur <- stringr::str_extract(complet, "([0-9]{2}.[0-9]{3})") # Si celle d'avant est vide, on cherche une valeur de débit à 2 chiffres + 3 //// Dernier
  if(is.na(Valeur)) Valeur <- stringr::str_extract(complet, "([0-9]{1}.[0-9]{3})") # Si celle d'avant est vide, on cherche une valeur de débit à 1 chiffres + 3 //// Dernier
  if(is.na(Valeur)) Valeur <- stringr::str_extract(complet, "([0-9]{3})") # Si celle d'avant est vide, on cherche une valeur de débit à 3 chiffres sans virgule
  if(is.na(Valeur)) Valeur <- stringr::str_extract(complet, "([0-9]{2}.[0-9]{2})") # Si celle d'avant est vide, on cherche une valeur de débit à 2 chiffres + 2
  if(is.na(Valeur)) Valeur <- stringr::str_extract(complet, "([0-9]{2}.[0-9]{1})") # Si celle d'avant est vide, on cherche une valeur de débit à 2 chiffres + 1
  #if(is.na(Valeur)) Valeur <- stringr::str_extract(complet, "([0-9]{2}.[0-9]{0})") # Si celle d'avant est vide, on cherche une valeur de débit à 2 chiffres sans virgule
  if(is.na(Valeur)) Valeur <- stringr::str_extract(complet, "([0-9]{1}.[0-9]{2})") # Si celle d'avant est vide, on cherche une valeur de débit à 1 chiffres + 2  
  if(is.na(Valeur)) Valeur <- stringr::str_extract(complet, "([0-9]{1}.[0-9]{1})") # Si celle d'avant est vide, on cherche une valeur de débit à 1 chiffres + 1 
  #if(is.na(Valeur)) Valeur <- stringr::str_extract(complet, "([0-9]{1}.[0-9]{0})") # Si celle d'avant est vide, on cherche une valeur de débit à 1 chiffres + 0
  
  # Station
  
  # Synthèse
  data <- data.frame(Station,Date,Heure,Valeur)
  data$Valeur <- as.numeric(levels(data$Valeur))[data$Valeur]
  data$Date <- as.character(data$Date)
  data$Heure <- as.character(data$Heure)
  data$Station <- as.character(data$Station)
  
  # Retour
  return(data)
  
} # Fin de la fonction