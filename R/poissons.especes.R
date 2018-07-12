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
  dbP <- BDD.ouverture(Type = "Poissons")
  
  ##### Récupération des données #####
  Especes <- tbl(dbP,"especes") %>% collect(n = Inf)
  
  return(Especes)
  DBI::dbDisconnect(dbP)
} # Fin de la fonction

# Resultats <- poissons.resultats() pour avoir les données en utilisant la fonction