#' Extraire les résultats de pêches d'inventaire
#'
#' Permet d'extraire les résultats de pêches d'inventaire
#' @name poissons.exportation
#' @keywords poissons
#' @param station Code RHJ de la station
#' @param date Date de la pêche
#' @param commentaires \code{FALSE} par défault. Permet d'extraire le commentaire associé à la pêche
#' @import dplyr
#' @import lubridate 
#' @import sf
#' @import stringr
#' @import tidyverse
#' @export
#' @examples
#' poissons.exportation("SOR10-2", "2015-05-19")
#' poissons.exportation("SOR10-2", "2015-05-19", commentaires = T)

poissons.exportation <- function(  
  station = "AIN18-4",
  date = "2011-09-07",
  commentaires = FALSE
  )
{

##### -------------- A FAIRE -------------- #####
# 
# -------------- A FAIRE -------------- #  

#### Évaluation des choix ####


#### Vérification des répertoires ####

if(file.exists(paste0("./",station, "/")) == FALSE){
  dir.create(paste0("./",station, "/"), showWarnings = FALSE, recursive = FALSE)
}

#### Déplacement dans le bon répertoire ####
setwd(paste0("./",station))

#### Exportation propre ####
if(commentaires == FALSE) poissons.fiche(station, date, commentaires = F) # fichier pdf
if(commentaires == TRUE) poissons.fiche(station, date, commentaires = T) # fichier pdf

poissons.brut(station, date) # fichier excel

} # Fin de la fonction
