#' Affichage de graphes piscicoles
#'
#' Cette fonction permet de créer des comparaisons entre les espèces attendues et observées. Le jeu de données doit contenir un champ codeespece, un champ CA et un champ date
#' @name poissons.CAAvue
#' @keywords poissons
#' @export
#' @import ggplot2
#' @examples
#' poissons.CAAvue(data)
#' poissons.CAAvue(data, save=T)
#' poissons.CAA("MAD6-2", 5) %>% poissons.CAAvue()
#' poissons.CAA("MAD6-2", 5) %>% poissons.CAAvue(save = T)

##### -------------- A FAIRE -------------- #####
# Intégrer dans une chaîne cette fonction et poissons.CAA au sein de poissons.exportation
# -------------- A FAIRE -------------- #

poissons.CAAvue <- function(
  data = data,
  save=F,
  format=".png")
  {
  
  ## Extraction d'informations ##
  station <- data %>% distinct(station) %>% as.character()
  
  ## Vérifications ##
  if(length(station) == 0) stop("Aucune station au sein du jeu de données")
  if(length(station) != 1) stop("Plusieurs stations au sein du jeu de données")
  
  ## Transformation des données ##
  data(listeSp) # Pour charger la liste complète des espèces
  data$codeespece <- factor(data$codeespece,listeSp) # Pour modifier l'ordre des espèce
  
  ## Représentation graphique ##
  gg1 <- ggplot(data, aes(x=factor(codeespece,listeSp), y = CA, fill=as.factor(date)))
  gg1 <- gg1 + geom_bar(stat="identity")
  gg1 <- gg1 + facet_grid(date ~ .)
  gg1 <- gg1 + labs(x = "Espèce", y = "Cote d'abondance (/5)", fill= "Date", title = station)
  #gg1 <- gg1 + scale_fill_manual(values=c("#8A9B0F","#F8CA00","#E97F02", "#BD1550", "#490A3D")) +
  gg1 <- gg1 + scale_fill_manual(values=c("#0000FF","#FF0000","#8A9B0F","#F8CA00","#E97F02", "#BD1550", "#490A3D","#490A3D","#490A3D","#490A3D"))
  gg1 <- gg1 + theme(legend.position = 'none', panel.grid.minor = element_blank()) # Pour mettre la légende en haut + enlever les lignes blanches horizontales des 0,5
  gg1 <- gg1 + ylim(0,5)
  gg1
  if(save==T){ggsave(file=paste("Vue_CAA_CAR_", station, format,sep=""))}
  if(save==F){return(gg1)}
  
} # Fin de la fonction