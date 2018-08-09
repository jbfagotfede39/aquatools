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
  if(exists("dbP") == FALSE){
    dbP <- BDD.ouverture(Type = "Poissons")
    assign("dbP", dbP, envir = .GlobalEnv)
  }
  
  ##### Récupération des données #####
  Especes <- tbl(dbP,"especes") %>% collect(n = Inf)
  
  return(Especes)
} # Fin de la fonction

# Resultats <- poissons.resultats() pour avoir les données en utilisant la fonction