#' Chargement des bases de données
#'
#' Cette fonction permet de charger les bases de données de la fédération
#' 
#' @param Type Type de base de données. Chroniques par défaut
#' @import dplyr
#' @export
#' @examples
#' BDD.ouverture()
#' BDD.ouverture(Type = "Poissons")
#' db <- BDD.ouverture()
#' db <- BDD.ouverture(Type = "Poissons")
#' head(tbl(db,"SuiviTerrain"), 10) %>% collect()

##### TODO LIST #####
#
#####################

BDD.ouverture <- function(
                           Type = c("Chroniques", "Poissons", "Macroinvertébrés", "Physico-chimie", "Temps de travail"))
{
  
  ## Évaluation des choix
  Type <- match.arg(Type)
  
## Connexion à la BDD ##
drv <- dbDriver("SQLite")

## Chroniques ##
if(Type == "Chroniques" & file.exists("/Users/imac27/NAS-DATA/Chroniques/BDD_Chroniques_FD39.sqlite") == T) db <- src_sqlite("/Users/imac27/NAS-DATA/Chroniques/BDD_Chroniques_FD39.sqlite")
if(Type == "Chroniques" & file.exists("/Volumes/Fixe-FD39/NAS-DATA/Chroniques/BDD_Chroniques_FD39.sqlite") == T) db <- src_sqlite("/Volumes/Fixe-FD39/NAS-DATA/Chroniques/BDD_Chroniques_FD39.sqlite")

## Poissons ##
if(Type == "Poissons" & file.exists("/Users/imac27/NAS-DATA/Poissons/Base poisson FD/MaxiFish_V3/multifish - datas.sqlite") == T) db <- src_sqlite("/Users/imac27/NAS-DATA/Poissons/Base poisson FD/MaxiFish_V3/multifish - datas.sqlite")
if(Type == "Poissons" & file.exists("/Volumes/Fixe-FD39/NAS-DATA/Poissons/Base poisson FD/MaxiFish_V3/multifish - datas.sqlite") == T) db <- src_sqlite("/Volumes/Fixe-FD39/NAS-DATA/Poissons/Base poisson FD/MaxiFish_V3/multifish - datas.sqlite")

## Macroinvertébrés ##
if(Type == "Macroinvertébrés" & file.exists("/Users/imac27/NAS-DATA/Macroinvertébrés/BDD_MI_FD39.sqlite") == T) db <- src_sqlite("/Users/imac27/NAS-DATA/Macroinvertébrés/BDD_MI_FD39.sqlite")
if(Type == "Macroinvertébrés" & file.exists("/Volumes/Fixe-FD39/NAS-DATA/Macroinvertébrés/BDD_MI_FD39.sqlite") == T) db <- src_sqlite("/Volumes/Fixe-FD39/NAS-DATA/Macroinvertébrés/BDD_MI_FD39.sqlite")

## Physico-chimie ##
if(Type == "Physico-chimie" & file.exists("/Users/imac27/NAS-DATA/Physico-chimie/BDD_Physico-chimie_FD39.sqlite") == T) db <- src_sqlite("/Users/imac27/NAS-DATA/Physico-chimie/BDD_Physico-chimie_FD39.sqlite")
if(Type == "Physico-chimie" & file.exists("/Volumes/Fixe-FD39/NAS-DATA/Physico-chimie/BDD_Physico-chimie_FD39.sqlite") == T) db <- src_sqlite("/Volumes/Fixe-FD39/NAS-DATA/Physico-chimie/BDD_Physico-chimie_FD39.sqlite")

## Temps de travail ##
if(Type == "Temps de travail" & file.exists("/Users/imac27/NAS-FD/Activité/Temps de travail/BDD_Tps_travail_FD39.sqlite") == T) db <- src_sqlite("/Users/imac27/NAS-FD/Activité/Temps de travail/BDD_Tps_travail_FD39.sqlite")
if(Type == "Temps de travail" & file.exists("/Volumes/Fixe-FD39/NAS-FD/Activité/Temps de travail/BDD_Tps_travail_FD39.sqlite") == T) db <- src_sqlite("/Volumes/Fixe-FD39/NAS-FD/Activité/Temps de travail/BDD_Tps_travail_FD39.sqlite")

return(db)

} # Fin de la fonction
