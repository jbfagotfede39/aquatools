#' Calcul des NTT
#'
#' Cette fonction permet de calculer les Niveaux Typologiques Théoriques à partir d'un dataframe issu de poissons.stations()
#' @param Données
#' @keywords poissons
#' @export
#' @import dplyr
#' @examples
#' poissons.NTT(station)

##### -------------- A FAIRE -------------- #####

##### -------------- A FAIRE -------------- #####

poissons.NTT <- function(station){
  T1<-0.55*station$TemperatureMax-4.34
  T2<-1.17*log(station$DistanceSource*station$Durete*0.01)+1.5
  T3<-1.75*log(100*station$SectionMouillee/(station$Pente*station$LargeurLitMineur*station$LargeurLitMineur))+3.92
  NTT<-0.45*T1+0.3*T2+0.25*T3
  NTT<-round(NTT,2)
  station<-mutate(station,NTT) # Afin de rajouter le NTT au bout de la base de données initiale
  station<-arrange(station,Nom)
}