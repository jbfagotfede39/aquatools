#' Calcul des NTT
#'
#' Cette fonction permet de calculer les Niveaux Typologiques Théoriques
#' @param Données
#' @keywords poissons
#' @export
#' @import dplyr
#' @examples
#' ntt(donnee)

##### -------------- A FAIRE -------------- #####

# Stocker PC dans un dataframe et pouvoir le réutiliser dans l'environnement hors de la fonction

##### -------------- A FAIRE -------------- #####

ntt<-function(PC){
  T1<-0.55*PC$Tmm30j-4.34
  T2<-1.17*log(PC$DistSource*PC$Durete*0.01)+1.5
  T3<-1.75*log(100*PC$Sectionmou/(PC$Pente*PC$Largeurlit*PC$Largeurlit))+3.92
  NTT<-0.45*T1+0.3*T2+0.25*T3
  NTT<-round(NTT,2)
  PC<-mutate(PC,NTT) # Afin de rajouter le NTT au bout de la base de données initiale
  PC<-arrange(PC,Identifian)
  #print(PC) #print(PC) : était commenté dans la version précédente pour utilisation ultérieure dans latex, mais semble ok si on fait test <- ntt(basededonnees) dans le script
}