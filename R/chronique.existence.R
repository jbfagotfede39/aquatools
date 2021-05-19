#' Vérification de l'existence de stations/capteurs
#'
#' Cette fonction permet de vérifier l'existence de références de stations/capteurs dans les tables de références
#' @name chronique.existence
#' @param Stations Data.frame contenant la liste de référence des stations
#' @param Capteurs Data.frame contenant la liste de référence des capteurs
#' @param Station Station recherchée
#' @param Capteur Capteur recherché
#' @keywords chronique
#' @import cli
#' @import glue
#' @import tidyverse
#' @export
#' @examples
#' chronique.existence(Stations, Capteurs, "VAL6-0", "U4129")
#' chronique.existence(Stations, Capteurs, Station, Capteur)

chronique.existence <- function(
  Stations = NA_character_,
  Capteurs = NA_character_,
  Station = NA_character_,
  Capteur = NA_character_)
{
  
##### -------------- A FAIRE -------------- #####

# -------------- A FAIRE -------------- #
  
  ##### Vérification #####
  if(any(is.na(c(Stations, Capteurs, Station, Capteur)))){stop("Une variable d'entrée est vide")}
  
  #### Retour de la réponse ####
  if(dim(Stations %>% filter(chsta_coderhj == Station))[1] == 1) messageStation <- glue("Station {col_green('présente')} dans la base de référence")
  if(dim(Stations %>% filter(chsta_coderhj == Station))[1] == 0) messageStation <- glue("Station {col_red('absente')} de la base de référence")
  if(dim(Stations %>% filter(chsta_coderhj == Station))[1] > 1) messageStation <- glue("Station {col_yellow('présente deux fois')} dans la base de référence")
  
  if(dim(Capteurs %>% filter(chcap_numerocapteur == Capteur))[1] == 1) messageCapteur <- glue("Capteur {col_green('présent')} dans la base de référence")
  if(dim(Capteurs %>% filter(chcap_numerocapteur == Capteur))[1] == 0) messageCapteur <- glue("Capteur {col_red('absent')} de la base de référence")
  if(dim(Capteurs %>% filter(chcap_numerocapteur == Capteur))[1] > 1) messageCapteur <- glue("Capteur {col_yellow('présent deux fois')} dans la base de référence")
  
  #### Affichage ####
  cli_li(
    c(messageStation, messageCapteur)
    )
  
}