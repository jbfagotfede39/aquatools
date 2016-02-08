#' Chargement des bases de données
#'
#' Cette fonction permet de charger les bases de données de la fédération
#' 
#' @param Type Type de base de données
#' @keywords 
#' @import dplyr DBI RSQLite
#' @export
#' @examples
#' BDD.ouverture(Type = "Thermie")

##### TODO LIST #####
# 
#####################

BDD.ouverture <- function(x, 
    Type = c("Chroniques", "Poissons", "Thermie", "Physico-chimie", "Piézométrie", "Hydrologie", "Temps de travail"))
{

## Évaluation des choix
Type <- match.arg(Type)

## Connexion à la BDD ##
drv <- dbDriver("SQLite")

## Chroniques ##
if(Type == "Chroniques" & file.exists("/Users/imac27/hubiC/Données/Chroniques/BDD_Chroniques_FD39.sqlite") == T) db <- dbConnect(drv, dbname="/Users/imac27/hubiC/Données/Chroniques/BDD_Chroniques_FD39.sqlite")
if(Type == "Chroniques" & file.exists("/Users/jean-baptistefagot_FD39/hubiC/Données/Chroniques/BDD_Chroniques_FD39.sqlite") == T) db <- dbConnect(drv, dbname="/Users/jean-baptistefagot_FD39/hubiC/Données/Chroniques/BDD_Chroniques_FD39.sqlite")

## Poissons ##
if(Type == "Poissons" & file.exists("/Users/imac27/hubiC/Données/Poissons/Base poisson FD/MaxiFish_V3/multifish - fede39.sqlite") == T) db <- dbConnect(drv, dbname="/Users/imac27/hubiC/Données/Poissons/Base poisson FD/MaxiFish_V3/multifish - fede39.sqlite")
if(Type == "Poissons" & file.exists("/Users/jean-baptistefagot_FD39/hubiC/Données/Poissons/Base poisson FD/MaxiFish_V3/multifish - fede39.sqlite") == T) db <- dbConnect(drv, dbname="/Users/jean-baptistefagot_FD39/hubiC/Données/Poissons/Base poisson FD/MaxiFish_V3/multifish - fede39.sqlite")

## Thermie ##
if(Type == "Thermie" & file.exists("/Users/imac27/hubiC/Données/Thermie/Exploitation_données_thermie/BDD_Thermie_FD39.sqlite") == T) db <- dbConnect(drv, dbname="/Users/imac27/hubiC/Données/Thermie/Exploitation_données_thermie/BDD_Thermie_FD39.sqlite")
if(Type == "Thermie" & file.exists("/Users/jean-baptistefagot_FD39/hubiC/Données/Thermie/Exploitation_données_thermie/BDD_Thermie_FD39.sqlite") == T) db <- dbConnect(drv, dbname="/Users/jean-baptistefagot_FD39/hubiC/Données/Thermie/Exploitation_données_thermie/BDD_Thermie_FD39.sqlite")

## Physico-chimie ##
if(Type == "Physico-chimie" & file.exists("/Users/imac27/hubiC/Données/Physico-chimie/BDD_Physico-chimie_FD39.sqlite") == T) db <- dbConnect(drv, dbname="/Users/imac27/hubiC/Données/Physico-chimie/BDD_Physico-chimie_FD39.sqlite")
if(Type == "Physico-chimie" & file.exists("/Users/jean-baptistefagot_FD39/hubiC/Données/Physico-chimie/BDD_Physico-chimie_FD39.sqlite") == T) db <- dbConnect(drv, dbname="/Users/jean-baptistefagot_FD39/hubiC/Données/Physico-chimie/BDD_Physico-chimie_FD39.sqlite")

## Piézométrie ##
if(Type == "Piézométrie" & file.exists("/Users/imac27/hubiC/Données/Piézométrie/BDD_Piézométrie_FD39.sqlite") == T) db <- dbConnect(drv, dbname="/Users/imac27/hubiC/Données/Piézométrie/BDD_Piézométrie_FD39.sqlite")
if(Type == "Piézométrie" & file.exists("/Users/jean-baptistefagot_FD39/hubiC/Données/Piézométrie/BDD_Piézométrie_FD39.sqlite") == T) db <- dbConnect(drv, dbname="/Users/jean-baptistefagot_FD39/hubiC/Données/Piézométrie/BDD_Piézométrie_FD39.sqlite")

## Hydrologie ##
if(Type == "Hydrologie" & file.exists("/Users/imac27/hubiC/Données/Hydrologie/BDD_Hydrologie_FD39.sqlite") == T) db <- dbConnect(drv, dbname="/Users/imac27/hubiC/Données/Hydrologie/BDD_Hydrologie_FD39.sqlite")
if(Type == "Hydrologie" & file.exists("/Users/jean-baptistefagot_FD39/hubiC/Données/Piézométrie/BDD_Hydrologie_FD39.sqlite") == T) db <- dbConnect(drv, dbname="/Users/jean-baptistefagot_FD39/hubiC/Données/Hydrologie/BDD_Hydrologie_FD39.sqlite")

## Temps de travail ##
if(Type == "Temps de travail" & file.exists("/Users/imac27/hubiC/FD39/Activité/Temps de travail/BDD_Tps_travail_FD39.sqlite") == T) db <- dbConnect(drv, dbname="/Users/imac27/hubiC/FD39/Activité/Temps de travail/BDD_Tps_travail_FD39.sqlite")
if(Type == "Temps de travail" & file.exists("/Users/jean-baptistefagot_FD39/hubiC/FD39/Activité/Temps de travail/BDD_Tps_travail_FD39.sqlite") == T) db <- dbConnect(drv, dbname="/Users/jean-baptistefagot_FD39/hubiC/FD39/Activité/Temps de travail/BDD_Tps_travail_FD39.sqlite")

return(db)

} # Fin de la fonction