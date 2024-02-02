#' Reformatage des heures issues d'excel
#'
#' Cette fonction permet de mettre de reformater les heures d'excel importées au format numérique
#' @name formatage.heure.excel
#' @param data Heure à corriger (\code{0.57} par exemple)
#' @import lubridate
#' @import tidyverse
#' @export
#' @examples
#' formatage.heure.excel(0.57)

formatage.heure.excel <- function(
  data = NA
  )
  {
  
#### Calcul ####
  data_v2 <-
    format(parse_date_time(as.character(seconds_to_period(data*86400)), 'HMS'), "%H:%M:%S")
  
#### Sortie ####
return(data_v2)
  
} # Fin de la fonction