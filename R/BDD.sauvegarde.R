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
  if(Type == "Chroniques" & file.exists("/Users/imac27/NAS-DATA/Chroniques/BDD_Chroniques_FD39.sqlite") == T) file.copy("/Users/imac27/NAS-DATA/Chroniques/BDD_Chroniques_FD39.sqlite", paste0("/Users/imac27/NAS-DATA/Chroniques/Archives_BDD_Chroniques/BDD_Chroniques_FD39-",now(),".sqlite"))
  if(Type == "Chroniques" & file.exists("/Volumes/Fixe-FD39/NAS-DATA/Chroniques/BDD_Chroniques_FD39.sqlite") == T) file.copy("/Volumes/Fixe-FD39/NAS-DATA/Chroniques/BDD_Chroniques_FD39.sqlite", paste0("/Volumes/Fixe-FD39/NAS-DATA/Chroniques/Archives_BDD_Chroniques/BDD_Chroniques_FD39-",now(),".sqlite"))
  
  ## Physico-chimie ##
  if(Type == "Physico-chimie" & file.exists("/Users/imac27/NAS-DATA/Physico-chimie/BDD_Physico-chimie_FD39.sqlite") == T) file.copy("/Users/imac27/NAS-DATA/Physico-chimie/BDD_Physico-chimie_FD39.sqlite", paste0("/Users/imac27/NAS-DATA/Physico-chimie/Archives_BDD_FD39/BDD_Physico-chimie_FD39-",now(),".sqlite"))
  if(Type == "Physico-chimie" & file.exists("/Volumes/Fixe-FD39/NAS-DATA/Physico-chimie/BDD_Physico-chimie_FD39.sqlite") == T) file.copy("/Volumes/Fixe-FD39/NAS-DATA/Physico-chimie/BDD_Physico-chimie_FD39.sqlite", paste0("/Volumes/Fixe-FD39/NAS-DATA/Physico-chimie/Archives_BDD_FD39/BDD_Physico-chimie_FD39-",now(),".sqlite"))
  
  ## Macroinvertébrés ##
  if(Type == "Macroinvertébrés" & file.exists("/Users/imac27/NAS-DATA/Macroinvertébrés/BDD_MI_FD39.sqlite") == T) file.copy("/Users/imac27/NAS-DATA/Macroinvertébrés/BDD_MI_FD39.sqlite", paste0("/Users/imac27/NAS-DATA/Macroinvertébrés/Archives_BDD_MI_FD39/BDD_MI_FD39-",now(),".sqlite"))
  if(Type == "Macroinvertébrés" & file.exists("/Volumes/Fixe-FD39/NAS-DATA/Macroinvertébrés/BDD_MI_FD39.sqlite") == T) file.copy("/Volumes/Fixe-FD39/NAS-DATA/Macroinvertébrés/BDD_MI_FD39.sqlite", paste0("/Volumes/Fixe-FD39/NAS-DATA/Macroinvertébrés/Archives_BDD_MI_FD39/BDD_Macroinvertébrés_FD39-",now(),".sqlite"))
  
  ## Temps de travail ##
  if(Type == "Temps de travail" & file.exists("/Users/imac27/NAS-FD/FD39/Activité/Temps de travail/BDD_Tps_travail_FD39.sqlite") == T) file.copy("/Users/imac27/NAS-FD/FD39/Activité/Temps de travail/BDD_Tps_travail_FD39.sqlite", paste0("/Users/imac27/NAS-FD/FD39/Activité/Temps de travail/Archives_BDD_FD39/BDD_Tps_travail_FD39-",now(),".sqlite"))
  if(Type == "Temps de travail" & file.exists("/Volumes/Fixe-FD39/NAS-FD/FD39/Activité/Temps de travail/BDD_Tps_travail_FD39.sqlite") == T) file.copy("/Volumes/Fixe-FD39/NAS-FD/FD39/Activité/Temps de travail/BDD_Tps_travail_FD39.sqlite", paste0("/Volumes/Fixe-FD39/NAS-FD/FD39/Activité/Temps de travail/Archives_BDD_FD39/BDD_Tps_travail_FD39-",now(),".sqlite"))
  
} # Fin de la fonction
