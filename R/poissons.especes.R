#' Extraction des données de référence des espèces
#'
#' Récupère l'ensemble des données de référence des espèces de Multifish
#' @keywords donnees
#' @import DBI dplyr RSQLite
#' @export
#' @examples
#' poissons.especes()

poissons.especes <- function(){
  
  #library("RSQLite");library("dplyr")
  
  ## Ouverture de la BDD ##
  db <- BDD.ouverture(Type = "Poissons")
  
  ##### Récupération des données #####
  #Resultats <- dbReadTable(db, "Resultats")
  #Operations <- dbReadTable(db, "Operations")
  #Inventaires <- dbReadTable(db, "Inventaires")
  #Stations <- dbReadTable(db, "Stations")
  #Ecosystemes <- dbReadTable(db, "Ecosystemes")
  #Communes <- dbReadTable(db, "Communes")
  Especes <- dbReadTable(db, "Especes")
  
  return(Especes)
} # Fin de la fonction

# Resultats <- poissons.resultats() pour avoir les données en utilisant la fonction