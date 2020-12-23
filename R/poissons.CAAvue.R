#' Affichage de graphes piscicoles
#'
#' Cette fonction permet de créer des comparaisons entre les espèces attendues et observées. Le jeu de données doit en entrée doit être issu de poissons.CAA
#' @name poissons.CAAvue
#' @param data Jeu de données en entrée (cotes d'abondance issues de poissons.CAA)
#' @param export \code{FALSE} par défault. Permet d'exporter les données
#' @param format Défini le format d'enregistrement (par défaut .png)
#' @keywords poissons
#' @export
#' @import tidyverse
#' @examples
#' poissons.CAAvue(data)
#' poissons.CAAvue(data, export = T)
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
  #Contexte <- data %>% select(Station) %>% distinct() %>% ungroup()
  if(n_distinct(data$Station) > 1){stop("Plusieurs stations dans le jeu de données CA")}
  Contexte <- data %>% distinct(Station)
  Station <- Contexte$Station
  
  #### Transformation des données ####
  data$Espece <- factor(data$Espece,listeSp) # Pour modifier l'ordre des espèce
  
  #### Représentation ####
  ggNTT<- ggplot(data, aes(x=factor(Espece,listeSp), y = CA, fill=as.factor(Annee)))
  ggNTT <- ggNTT + geom_bar(stat="identity")
  ggNTT <- ggNTT + facet_grid(Date ~ .)
  ggNTT <- ggNTT + labs(x = "Espèce", y = "Cote d'abondance (/5)", fill= "Date", title = Station)
  ggNTT <- ggNTT + theme_linedraw()
  ggNTT <- ggNTT + scale_fill_manual(values = PaletteAnnees)
  ggNTT <- ggNTT + theme(legend.position = 'none', panel.grid.minor = element_blank()) # Pour mettre la légende en haut + enlever les lignes blanches horizontales des 0,5
  ggNTT <- ggNTT + ylim(0,5)
  ggNTT
  
  #### Sortie ####
  if(export == TRUE){ggsave(file=paste("./",Station,"/PoissonsCA/CA_",Station, format, sep=""), width = 15, height = 15, units = ("cm"))}
  if(export == FALSE){return(ggNTT)}
  
} # Fin de la fonction