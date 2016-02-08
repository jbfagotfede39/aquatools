#' Sauvegarde des bases de données
#'
#' Cette fonction permet de sauvegarder les bases de données de la fédération
#' 
#' @param Type Type de base de données
#' @keywords 
#' @import lubridate
#' @export
#' @examples
#' BDD.sauvegarde(Type = "Thermie")

##### TODO LIST #####
# 
#####################

BDD.sauvegarde <- function(x, 
                          Type = c("Chroniques", "Thermie", "Physico-chimie", "Hydrologie", "Piézométrie", "Temps de travail"))
{
  
  ## Évaluation des choix
  Type <- match.arg(Type)
  
  ## Poissons ##

  ## Chroniques ##
  if(Type == "Thermie" & file.exists("/Users/imac27/hubiC/Données/Chroniques/BDD_Chroniques_FD39.sqlite") == T) file.copy("/Users/imac27/hubiC/Données/Chroniques/BDD_Chroniques_FD39.sqlite", paste0("/Users/imac27/hubiC/Données/Chroniques/Archives_BDD_Chroniques/BDD_Chroniques-",now(),".sqlite"))
  if(Type == "Thermie" & file.exists("/Users/jean-baptistefagot_FD39/hubiC/Données/Chroniques/BDD_Chroniques_FD39.sqlite") == T) file.copy("/Users/jean-baptistefagot_FD39/hubiC/Données/Chroniques/BDD_Chroniques_FD39.sqlite", paste0("/Users/jean-baptistefagot_FD39/hubiC/Données/Chroniques/Archives_BDD_Chroniques/BDD_Chroniques-",now(),".sqlite"))
  
  ## Thermie ##
  if(Type == "Thermie" & file.exists("/Users/imac27/hubiC/Données/Thermie/Exploitation_données_thermie/BDD_Thermie_FD39.sqlite") == T) file.copy("/Users/imac27/hubiC/Données/Thermie/Exploitation_données_thermie/BDD_Thermie_FD39.sqlite", paste0("/Users/imac27/hubiC/Données/Thermie/Exploitation_données_thermie/Archives_BDD_Thermie_FD39/BDD_Thermie_FD39-",now(),".sqlite"))
  if(Type == "Thermie" & file.exists("/Users/jean-baptistefagot_FD39/hubiC/Données/Thermie/Exploitation_données_thermie/BDD_Thermie_FD39.sqlite") == T) file.copy("/Users/jean-baptistefagot_FD39/hubiC/Données/Thermie/Exploitation_données_thermie/BDD_Thermie_FD39.sqlite", paste0("/Users/jean-baptistefagot_FD39/hubiC/Données/Thermie/Exploitation_données_thermie/Archives_BDD_Thermie_FD39/BDD_Thermie_FD39-",now(),".sqlite"))
  
  ## Physico-chimie ##
  if(Type == "Physico-chimie" & file.exists("/Users/imac27/hubiC/Données/Physico-chimie/BDD_Physico-chimie_FD39.sqlite") == T) file.copy("/Users/imac27/hubiC/Données/Physico-chimie/BDD_Physico-chimie_FD39.sqlite", paste0("/Users/imac27/hubiC/Données/Physico-chimie/Archives_BDD_FD39/BDD_Physico-chimie_FD39-",now(),".sqlite"))
  if(Type == "Physico-chimie" & file.exists("/Users/jean-baptistefagot_FD39/hubiC/Données/Physico-chimie/BDD_Physico-chimie_FD39.sqlite") == T) file.copy("/Users/jean-baptistefagot_FD39/hubiC/Données/Physico-chimie/BDD_Physico-chimie_FD39.sqlite", paste0("/Users/jean-baptistefagot_FD39/hubiC/Données/Physico-chimie/Archives_BDD_FD39/BDD_Physico-chimie_FD39-",now(),".sqlite"))
  
  ## Piézométrie ##
  if(Type == "Piézométrie" & file.exists("/Users/imac27/hubiC/Données/Piézométrie/BDD_Piézométrie_FD39.sqlite") == T) file.copy("/Users/imac27/hubiC/Données/Piézométrie/BDD_Piézométrie_FD39.sqlite", paste0("/Users/imac27/hubiC/Données/Piézométrie/Archives_BDD_piézo_FD39/BDD_Piézométrie_FD39-",now(),".sqlite"))
  if(Type == "Piézométrie" & file.exists("/Users/jean-baptistefagot_FD39/hubiC/Données/Piézométrie/BDD_Piézométrie_FD39.sqlite") == T) file.copy("/Users/jean-baptistefagot_FD39/hubiC/Données/Piézométrie/BDD_Piézométrie_FD39.sqlite", paste0("/Users/jean-baptistefagot_FD39/hubiC/Données/Piézométrie/Archives_BDD_piézo_FD39/BDD_Piézométrie_FD39-",now(),".sqlite"))
  
  ## Hydrologie ##
  if(Type == "Hydrologie" & file.exists("/Users/imac27/hubiC/Données/Hydrologie/BDD_Hydrologie_FD39.sqlite") == T) file.copy("/Users/imac27/hubiC/Données/Hydrologie/BDD_Hydrologie_FD39.sqlite", paste0("/Users/imac27/hubiC/Données/Hydrologie/Archives_BDD_Hydrologie_FD39/BDD_Hydrologie_FD39-",now(),".sqlite"))
  if(Type == "Hydrologie" & file.exists("/Users/jean-baptistefagot_FD39/hubiC/Données/Hydrologie/BDD_Hydrologie_FD39.sqlite") == T) file.copy("/Users/jean-baptistefagot_FD39/hubiC/Données/Hydrologie/BDD_Hydrologie_FD39.sqlite", paste0("/Users/jean-baptistefagot_FD39/hubiC/Données/Hydrologie/Archives_BDD_Hydrologie_FD39/BDD_Hydrologie_FD39-",now(),".sqlite"))
  
  ## Temps de travail ##
  if(Type == "Temps de travail" & file.exists("/Users/imac27/hubiC/FD39/Activité/Temps de travail/BDD_Tps_travail_FD39.sqlite") == T) file.copy("/Users/imac27/hubiC/FD39/Activité/Temps de travail/BDD_Tps_travail_FD39.sqlite", paste0("/Users/imac27/hubiC/FD39/Activité/Temps de travail/Archives_BDD_FD39/BDD_Tps_travail_FD39-",now(),".sqlite"))
  if(Type == "Temps de travail" & file.exists("/Users/jean-baptistefagot_FD39/hubiC/FD39/Activité/Temps de travail/BDD_Tps_travail_FD39.sqlite") == T) file.copy("/Users/jean-baptistefagot_FD39/hubiC/FD39/Activité/Temps de travail/BDD_Tps_travail_FD39.sqlite", paste0("/Users/jean-baptistefagot_FD39/hubiC/FD39/Activité/Temps de travail/Archives_BDD_FD39/BDD_Tps_travail_FD39-",now(),".sqlite"))
  
} # Fin de la fonction
