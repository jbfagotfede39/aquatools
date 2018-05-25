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
  Especes <- tbl(db,"especes") %>% collect(n = Inf)
  
  return(Especes)
} # Fin de la fonction

# Resultats <- poissons.resultats() pour avoir les données en utilisant la fonction