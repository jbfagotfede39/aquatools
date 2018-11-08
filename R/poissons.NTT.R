#' Calcul des NTT
#'
#' Cette fonction permet de calculer les Niveaux Typologiques Théoriques
#' @name poissons.NTT
#' @param Données Dataframe issu de poissons.stations()
#' @keywords poissons
#' @export
#' @import dplyr
#' @examples
#' poissons.NTT(station)

##### -------------- A FAIRE -------------- #####

##### -------------- A FAIRE -------------- #####

poissons.NTT <- function(station){
  station <- 
    station %>% 
    mutate(T1 = 0.55*station$temperaturemax-4.34) %>% 
    mutate(T2 = 1.17*log(station$distancesource*station$durete*0.01)+1.5) %>% 
    mutate(T3 = 1.75*log(100*station$sectionmouillee/(station$pente*station$largeurlitmineur*station$largeurlitmineur))+3.92) %>% 
    mutate(NTT = round(0.45*T1+0.3*T2+0.25*T3,2)) %>% 
    mutate(NTTarrondi = round(NTT * 2) / 2) %>% 
    arrange(nom)
  
return(station)
  
}