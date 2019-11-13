#' Affichage de graphes piscicoles
#'
#' Cette fonction permet de créer des comparaisons entre les espèces attendues et observées. Le jeu de données doit en entrée doit être issu de poissons.CAA
#' @name poissons.CAAvue
#' @keywords poissons
#' @export
#' @import ggplot2
#' @examples
#' poissons.CAAvue(data)
#' poissons.CAAvue(data, export=T)
#' poissons.CAA("MAD6-2") %>% poissons.CAAvue()
#' poissons.CAA("MAD6-2") %>% poissons.CAAvue(export = T)

##### -------------- A FAIRE -------------- #####
# 
# -------------- A FAIRE -------------- #

poissons.CAAvue <- function(
  data = data,
  export = FALSE,
  format = ".png"
  )
  {
  
  ##### Palettes ####
  data(PalettePoissons)
  data(PaletteAnnees)
  
  #### Contexte ####
  Contexte <-
    data %>% select(Station) %>% distinct() %>% ungroup()
  
  #### Transformation des données ####
  data$Espece <- factor(data$Espece,listeSp) # Pour modifier l'ordre des espèce
  
  #### Représentation ####
  ggNTT<- ggplot(data, aes(x=factor(Espece,listeSp), y = CA, fill=as.factor(Annee)))
  ggNTT <- ggNTT + geom_bar(stat="identity")
  ggNTT <- ggNTT + facet_grid(Date ~ .)
  ggNTT <- ggNTT + labs(x = "Espèce", y = "Cote d'abondance (/5)", fill= "Date", title = Contexte$Station)
  ggNTT <- ggNTT + theme_linedraw()
  ggNTT <- ggNTT + scale_fill_manual(values = PaletteAnnees)
  ggNTT <- ggNTT + theme(legend.position = 'none', panel.grid.minor = element_blank()) # Pour mettre la légende en haut + enlever les lignes blanches horizontales des 0,5
  ggNTT <- ggNTT + ylim(0,5)
  ggNTT
  
  #### Sortie ####
  if(export == TRUE){ggsave(file=paste("./",Contexte$Station,"/PoissonsCA/CA_",Contexte$Station, format, sep=""), width = 15, height = 15, units = ("cm"))}
  if(export == FALSE){return(ggNTT)}
  
} # Fin de la fonction