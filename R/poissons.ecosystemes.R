#' Listing des cours d'eau de la BDD
#'
#' Cette fonction permet de lister les différents cours d'eau de la base de données
#' 
#' @keywords poissons
#' @import dplyr RSQLite DBI lubridate
#' @export
#' @examples
#' listeCE <- poissons.ecosystemes()

##### TODO LIST #####
# 
#####################

poissons.ecosystemes <- function()
{
  
  
  ## Ouverture de la BDD ##
  db <- BDD.ouverture(Type = "Poissons")
  
  ## Récupération des données ##
  #Captures <- dbReadTable(db, "Captures")
  #Operations <- dbReadTable(db, "Operations")
  #Inventaires <- dbReadTable(db, "Inventaires")
  #Stations <- dbReadTable(db, "Stations")
  Ecosystemes <- dbReadTable(db, "Ecosystemes")
  
  return(Ecosystemes)
  
} # Fin de la fonction