#' Filtrage d'une chronique
#'
#' Cette fonction permet de filtrer la période d'intérêt. La valeur de début est l'heure sélectionnée +1, la valeur de fin est strictement inférieure à celle indiquée
#' 
#' @param Début Date de début (2015-02-01 14:00:00)
#' @param Fin Date de fin (2015-02-04 14:00:00)
#' @keywords chronique
#' @import dplyr lubridate
#' @export
#' @examples
#' chronique.periode(Data, "2015-02-04 11:00:00", )
#' chronique.periode(Data, ,"2015-02-04 14:00:00")
#' chronique.periode(Data, "2015-02-04 11:00:00", "2015-02-04 14:00:00")

##### TODO LIST #####

#####################

chronique.periode <- function(x = Data, deb = "2011-07-13 14:00:00", fin = "2014-07-13 14:00:00")

{

  if (nchar(deb) == 0){
  fin <- ymd_hms(fin)
  x <-
    x %>% 
    mutate(Time = ymd_hms(paste(Date, Heure, sep = "_"))) %>% 
    filter(Time < fin) %>% #Time >= deb, 
    select(-Time)
}
  
if (nchar(fin) == 0){
  deb <- ymd_hms(deb)
  x <-
    x %>% 
    mutate(Time = ymd_hms(paste(Date, Heure, sep = "_"))) %>% 
    filter(Time > deb + hours(1)) %>% #, Time <= fin
    select(-Time)
}

if (nchar(deb) != 0 & nchar(fin) != 0){
  deb <- ymd_hms(deb)
  fin <- ymd_hms(fin)
  x <-
    x %>% 
    mutate(Time = ymd_hms(paste(Date, Heure, sep = "_"))) %>% 
    filter(Time > deb + hours(1), Time < fin) %>% 
    select(-Time)
}
  
  #deb = ""; fin = "2014-07-13 14:00:00"
  #deb = "2014-07-13 14:00:00"; fin = ""
  #deb = "2011-07-13 14:00:00"; fin = "2014-07-13 14:00:00"
  
  #x <- Mesures
  ## Affichage des résultats ##
  return(x)
}