#' Affichage de la date/heure actuelle
#'
#' Cette fonction permet d'afficher la date/heure actuelle au format \code{ymd_hm}
#' @name now.ymd_hm
#' @keywords chronique
#' @import lubridate
#' @export
#' @examples
#' now.ymd_hm()

now.ymd_hm <- function()
{
  
  #### Calcul ####
  data_sortie <- format(now(), format="%Y-%m-%d %H:%M")
  
  #### Sortie ####
  return(data_sortie)
}