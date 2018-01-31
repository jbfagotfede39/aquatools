#' Sauvegarde des bases de données
#'
#' Cette fonction permet de sauvegarder les bases de données de la fédération
#' 
#' @param Type Type de base de données
#' @import lubridate
#' @export
#' @examples
#' BDD.sauvegarde(Type = "Chroniques")

##### TODO LIST #####
# 
#####################

BDD.sauvegarde <- function(
                          Type = c("Chroniques", "Physico-chimie", "Macroinvertébrés", "Temps de travail"))
{
  
  ## Évaluation des choix
  Type <- match.arg(Type)
  
  ## Poissons ##

  ## Chroniques ##
  if(Type == "Chroniques" & file.exists("/Users/imac27/NAS-DATA/Chroniques/BDD_Chroniques_FD39.sqlite") == T) file.copy("/Users/imac27/NAS-DATA/Chroniques/BDD_Chroniques_FD39.sqlite", paste0("/Users/imac27/NAS-DATA/Archives/Archives_BDD_Chroniques_FD39_/BDD_Chroniques_FD39_", format(now(), format="%Y-%m-%d_%H-%M-%S"),".sqlite"))
  if(Type == "Chroniques" & file.exists("/Volumes/Fixe-FD39/NAS-DATA/Chroniques/BDD_Chroniques_FD39.sqlite") == T) file.copy("/Volumes/Fixe-FD39/NAS-DATA/Chroniques/BDD_Chroniques_FD39.sqlite", paste0("/Volumes/Fixe-FD39/NAS-DATA/Archives_à_ranger/BDD_Chroniques_FD39_", format(now(), format="%Y-%m-%d_%H-%M-%S"),".sqlite"))
  
  ## Physico-chimie ##
  if(Type == "Physico-chimie" & file.exists("/Users/imac27/NAS-DATA/Physico-chimie/BDD_Physico-chimie_FD39.sqlite") == T) file.copy("/Users/imac27/NAS-DATA/Physico-chimie/BDD_Physico-chimie_FD39.sqlite", paste0("/Users/imac27/NAS-DATA/Archives/Archives_BDD_Physico-chimie_FD39_/BDD_Physico-chimie_FD39_", format(now(), format="%Y-%m-%d_%H-%M-%S"),".sqlite"))
  if(Type == "Physico-chimie" & file.exists("/Volumes/Fixe-FD39/NAS-DATA/Physico-chimie/BDD_Physico-chimie_FD39.sqlite") == T) file.copy("/Volumes/Fixe-FD39/NAS-DATA/Physico-chimie/BDD_Physico-chimie_FD39.sqlite", paste0("/Volumes/Fixe-FD39/NAS-DATA/Archives_à_ranger/BDD_Physico-chimie_FD39_", format(now(), format="%Y-%m-%d_%H-%M-%S"),".sqlite"))
  
  ## Macroinvertébrés ##
  if(Type == "Macroinvertébrés" & file.exists("/Users/imac27/NAS-DATA/Macroinvertébrés/BDD_MI_FD39.sqlite") == T) file.copy("/Users/imac27/NAS-DATA/Macroinvertébrés/BDD_MI_FD39.sqlite", paste0("/Users/imac27/NAS-DATA/Archives/Archives_BDD_Macroinvertébrés_FD39/BDD_MI_FD39_", format(now(), format="%Y-%m-%d_%H-%M-%S"),".sqlite"))
  if(Type == "Macroinvertébrés" & file.exists("/Volumes/Fixe-FD39/NAS-DATA/Macroinvertébrés/BDD_MI_FD39.sqlite") == T) file.copy("/Volumes/Fixe-FD39/NAS-DATA/Macroinvertébrés/BDD_MI_FD39.sqlite", paste0("/Volumes/Fixe-FD39/NAS-DATA/Archives_à_ranger/BDD_Macroinvertébrés_FD39_", format(now(), format="%Y-%m-%d_%H-%M-%S"),".sqlite"))
  
  ## Temps de travail ##
  if(Type == "Temps de travail" & file.exists("/Users/imac27/NAS-FD/FD39/Activité/Temps de travail/BDD_Tps_travail_FD39.sqlite") == T) file.copy("/Users/imac27/NAS-FD/FD39/Activité/Temps de travail/BDD_Tps_travail_FD39.sqlite", paste0("/Users/imac27/NAS-FD/Archives/Archives_BDD_Tps_travail_FD39_/BDD_Tps_travail_FD39_", format(now(), format="%Y-%m-%d_%H-%M-%S"),".sqlite"))
  if(Type == "Temps de travail" & file.exists("/Volumes/Fixe-FD39/NAS-FD/FD39/Activité/Temps de travail/BDD_Tps_travail_FD39.sqlite") == T) file.copy("/Volumes/Fixe-FD39/NAS-FD/FD39/Activité/Temps de travail/BDD_Tps_travail_FD39.sqlite", paste0("/Volumes/Fixe-FD39/NAS-FD/Archives/Archives_BDD_Tps_travail_FD39_/BDD_Tps_travail_FD39_", format(now(), format="%Y-%m-%d_%H-%M-%S"),".sqlite"))
  
} # Fin de la fonction
