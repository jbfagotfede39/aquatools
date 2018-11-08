#' Extraction des données de référence des espèces
#'
#' Récupère l'ensemble des données de référence des espèces de Multifish
#' @name poissons.especes
#' @keywords donnees
#' @import DBI dplyr RSQLite
#' @export
#' @examples
#' poissons.especes()

poissons.especes <- function(){
  
  ## Ouverture de la BDD ##
  dbP <- BDD.ouverture(Type = "Poissons")
  
  ##### Récupération des données #####
  Especes <- tbl(dbP,"especes") %>% collect(n = Inf)
  
  ## Fermeture de la BDD ##
  DBI::dbDisconnect(dbP)
  
  return(Especes)
} # Fin de la fonction