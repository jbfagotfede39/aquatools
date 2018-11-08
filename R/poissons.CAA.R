#' Affichage de graphes piscicoles
#'
#' Cette fonction permet de créer des comparaisons entre les espèces attendues et observées. Le jeu de données doit contenir un champ ESPECE, un champ CA et un champ Date.
#' @name poissons.CAA
#' @keywords poissons
#' @export
#' @import ggplot2
#' @examples
#' poissons.CAA(poissonsvue)

# Il faudra affiner la présentation
# Il faudra rajouter un paramètre save=T, en sous option if dans chaque bloc

poissons.CAA <- function(poissonsvue){
  
  ggplot(poissonsvue, aes(x=ESPECE, y = CA,fill=Date)) +
    geom_bar(stat="identity") +
    facet_grid( Date ~ .) +
    labs(x = "Espèce", y = "Cote d'abondance (/5)") + 
    scale_fill_manual(values=c("#8A9B0F","#F8CA00","#E97F02", "#BD1550", "#490A3D")) +
    theme(legend.position="top") + # Pour mettre la légende en haut
    ylim(0,5)
  #theme(axis.text=element_text(size=12), # Taille des unités des graduations
  #axis.title=element_text(size=14,face="bold"), # Taille et gras pour les titres des axes
  #legend.title=element_text(size=14,face="bold"), # Taille et gras pour titre de la légende
  #legend.text=element_text(size=12)) # Taille des légendes
  
} # Fin de la fonction