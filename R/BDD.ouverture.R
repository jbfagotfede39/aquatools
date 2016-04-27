#' Chargement des bases de données
#'
#' Cette fonction permet de charger les bases de données de la fédération
#' 
#' @param Type Type de base de données
#' @keywords 
#' @import dplyr DBI RSQLite
#' @export
#' @examples
#' BDD.ouverture(Type = "Chroniques")

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
if(Type == "Chroniques" & file.exists("/Users/imac27/hubiC/Données/Chroniques/BDD_Chroniques_FD39.sqlite") == T) db <- dbConnect(drv, dbname="/Users/imac27/hubiC/Données/Chroniques/BDD_Chroniques_FD39.sqlite")
if(Type == "Chroniques" & file.exists("/Users/jean-baptistefagot_FD39/hubiC/Données/Chroniques/BDD_Chroniques_FD39.sqlite") == T) db <- dbConnect(drv, dbname="/Users/jean-baptistefagot_FD39/hubiC/Données/Chroniques/BDD_Chroniques_FD39.sqlite")

## Poissons ##
if(Type == "Poissons" & file.exists("/Users/imac27/hubiC/Données/Poissons/Base poisson FD/MaxiFish_V3/multifish - datas.sqlite") == T) db <- dbConnect(drv, dbname="/Users/imac27/hubiC/Données/Poissons/Base poisson FD/MaxiFish_V3/multifish - fede39.sqlite")
if(Type == "Poissons" & file.exists("/Users/jean-baptistefagot_FD39/hubiC/Données/Poissons/Base poisson FD/MaxiFish_V3/multifish - datas.sqlite") == T) db <- dbConnect(drv, dbname="/Users/jean-baptistefagot_FD39/hubiC/Données/Poissons/Base poisson FD/MaxiFish_V3/multifish - fede39.sqlite")

## Macroinvertébrés ##
if(Type == "Macroinvertébrés" & file.exists("/Users/imac27/hubiC/Données/Macroinvertébrés/BDD_MI_FD39.sqlite") == T) db <- dbConnect(drv, dbname="/Users/imac27/hubiC/Données/Macroinvertébrés/BDD_MI_FD39.sqlite")
if(Type == "Macroinvertébrés" & file.exists("/Users/jean-baptistefagot_FD39/hubiC/Données/Macroinvertébrés/BDD_MI_FD39.sqlite") == T) db <- dbConnect(drv, dbname="/Users/jean-baptistefagot_FD39/hubiC/Données/Macroinvertébrés/BDD_MI_FD39.sqlite")

## Physico-chimie ##
if(Type == "Physico-chimie" & file.exists("/Users/imac27/hubiC/Données/Physico-chimie/BDD_Physico-chimie_FD39.sqlite") == T) db <- dbConnect(drv, dbname="/Users/imac27/hubiC/Données/Physico-chimie/BDD_Physico-chimie_FD39.sqlite")
if(Type == "Physico-chimie" & file.exists("/Users/jean-baptistefagot_FD39/hubiC/Données/Physico-chimie/BDD_Physico-chimie_FD39.sqlite") == T) db <- dbConnect(drv, dbname="/Users/jean-baptistefagot_FD39/hubiC/Données/Physico-chimie/BDD_Physico-chimie_FD39.sqlite")

## Temps de travail ##
if(Type == "Temps de travail" & file.exists("/Users/imac27/hubiC/FD39/Activité/Temps de travail/BDD_Tps_travail_FD39.sqlite") == T) db <- dbConnect(drv, dbname="/Users/imac27/hubiC/FD39/Activité/Temps de travail/BDD_Tps_travail_FD39.sqlite")
if(Type == "Temps de travail" & file.exists("/Users/jean-baptistefagot_FD39/hubiC/FD39/Activité/Temps de travail/BDD_Tps_travail_FD39.sqlite") == T) db <- dbConnect(drv, dbname="/Users/jean-baptistefagot_FD39/hubiC/FD39/Activité/Temps de travail/BDD_Tps_travail_FD39.sqlite")

return(db)

} # Fin de la fonction
