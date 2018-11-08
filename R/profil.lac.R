#' Création de profils verticaux lacustres
#'
#' Cette fonction permet de créer des profils graphiques de paramètres physico-chimiques
#' @name profil.lac
#' @param O2mg Pour réaliser un profil de concentration en oxygène dissous
#' @param O2pourc Pour réaliser un profil de saturation en oxygène dissous
#' @param Cond Pour réaliser un profil de conductivité
#' @param ph Pour réaliser un profil de pH
#' @param temp Pour réaliser un profil de température
#' @export
#' @import ggplot2
#' @examples
#' profil.lac(PC,param="O2mg")
#' profil.lac(PC,param="O2pourc")
#' profil.lac(PC,param="Cond")
#' profil.lac(PC,param="ph")
#' #' profil.lac(PC,param="temp")

# Il faudra affiner la présentation
# Il faudra rajouter un paramètre save=T, en sous option if dans chaque bloc

profil.lac<-function(PC,param="O2mg"){
  
  if(param=="O2mg"){
  ggO2 <- ggplot(subset(PC, (ParametreSANDRE == 1311)), aes(-1*Profondeur, Valeur, colour=as.character(Date)))
  ggO2 <- ggO2 + geom_point(stat="identity") 
  ggO2 <- ggO2 + geom_line(linetype="dashed")
  if(subset(PC, (ParametreSANDRE == 1311)) %>% distinct(Date) %>% nrow() <= 8) gg <- gg + scale_color_manual(
    values = c("#D55E00", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#CC79A7", "#999999", "#0072B2"))
  ggO2 <- ggO2 + labs(x = "Profondeur (m)", y = expression(Oxygene~dissous~(mg~O[2]/L)), color = "Date") # Pour changer le titre
  ggO2 <- ggO2 + coord_flip() # pour inverser l'affichage des X et des Y
  ggO2 <- ggO2 + theme_bw()
  ggO2
  }

  else if(param=="O2pourc"){
  gg <- ggplot(subset(PC, (ParametreSANDRE == 1312)), aes(-1*Profondeur, Valeur, colour=as.character(Date)))
  gg <- gg + geom_point(stat="identity")
  gg <- gg + geom_line(linetype="dashed")
  if(subset(PC, (ParametreSANDRE == 1312)) %>% distinct(Date) %>% nrow() <= 8) gg <- gg + scale_color_manual(
    values = c("#D55E00", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#CC79A7", "#999999", "#0072B2"))
  gg <- gg + labs(x = "Profondeur (m)", y = expression(Oxygene~dissous~("%"~saturation~O[2]/L)), color = "Date") # Pour changer le titre
  gg <- gg + coord_flip() # pour inverser l'affichage des X et des Y
  gg <- gg + theme_bw()
  gg
  }
  
  else if(param=="Cond"){
  gg <- ggplot(subset(PC, (ParametreSANDRE == 1303)), aes(-1*Profondeur, Valeur, colour=as.character(Date)))
  gg <- gg + geom_point(stat="identity")
  gg <- gg + geom_line(linetype="dashed")
  if(subset(PC, (ParametreSANDRE == 1303)) %>% distinct(Date) %>% nrow() <= 8) gg <- gg + scale_color_manual(
    values = c("#D55E00", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#CC79A7", "#999999", "#0072B2"))
  gg <- gg + labs(x = "Profondeur (m)", y = expression(Conductivite~corrigee~à~25~degree*C~(paste(mu,S))), color = "Date") # Pour changer le titre
  gg <- gg + coord_flip() # pour inverser l'affichage des X et des Y
  gg <- gg + theme_bw()
  gg
  }
  
  else if(param=="ph"){
  gg <- ggplot(subset(PC, (ParametreSANDRE == 1302)), aes(-1*Profondeur, Valeur, colour=as.character(Date)))
  gg <- gg + geom_point(stat="identity")
  gg <- gg + geom_line(linetype="dashed")
  if(subset(PC, (ParametreSANDRE == 1302)) %>% distinct(Date) %>% nrow() <= 8) gg <- gg + scale_color_manual(
    values = c("#D55E00", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#CC79A7", "#999999", "#0072B2"))
  gg <- gg + labs(x = "Profondeur (m)", y = expression(pH~(unite~pH)), color = "Date") # Pour changer le titre
  gg <- gg + coord_flip() # pour inverser l'affichage des X et des Y
  gg <- gg + theme_bw()
  gg
  }
  
  else if(param=="temp"){
  gg <- ggplot(subset(PC, (ParametreSANDRE == 1301)), aes(-1*Profondeur, Valeur, colour=as.character(Date)))
  gg <- gg + geom_point(stat="identity") 
  gg <- gg + geom_line(linetype="dashed")
  if(subset(PC, (ParametreSANDRE == 1301)) %>% distinct(Date) %>% nrow() <= 8) gg <- gg + scale_color_manual(
    values = c("#D55E00", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#CC79A7", "#999999", "#0072B2"))
  gg <- gg + labs(x = "Profondeur (m)", y = expression(Temperature~(degree*C)), color = "Date") # Pour changer le titre
  gg <- gg + coord_flip() # pour inverser l'affichage des X et des Y
  gg <- gg + theme_bw()
  gg
  }

} # Fin de la fonction