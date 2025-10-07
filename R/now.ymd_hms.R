#' Affichage de la date/heure actuelle
#'
#' Cette fonction permet d'afficher la date/heure actuelle au format \code{ymd_hms}
#' @name now.ymd_hms
#' @keywords chronique
#' @import lubridate
#' @export
#' @examples
#' now.ymd_hm()

now.ymd_hms <- function()
{
  
  #### Calcul ####
  data_sortie <- format(now(), format="%Y-%m-%d %H:%M:%S")
  
  #### Sortie ####
  return(data_sortie)
}