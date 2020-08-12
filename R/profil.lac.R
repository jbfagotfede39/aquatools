#' Création de profils verticaux lacustres
#'
#' Cette fonction permet de créer des profils graphiques de paramètres physico-chimiques
#' @name profil.lac
#' @param param Paramètre physico-chimique à représenter (O2mg, O2pourc, cond, ph, temp, redox, chlorophylles, phycocyanines, secchi)
#' @export
#' @import ggplot2
#' @examples
#' profil.lac(PC, param = "O2mg")
#' PC %>% profil.lac(param = "redox")

##### -------------- A FAIRE -------------- #####
# Il faudra affiner la présentation
# Ré-écrire en ajoutant une paramétrisation des paramètres PC au début du traitement
# Supprimer le filtrage ?
# Il faudra rajouter un paramètre save=T, en sous option if dans chaque bloc
##### -------------- A FAIRE -------------- #####

profil.lac <- function(
  PC,
  param = c("O2mg", "O2pourc", "cond", "ph", "temp", "redox", "chlorophylles", "phycocyanines", "secchi")
  )
  {
  
  #### Évaluation des choix ####
  param <- match.arg(param)
  
  #### Traitement ####
  if(param=="O2mg"){
  gg <- ggplot(subset(PC, (pcmes_parametresandre == 1311)), aes(-1*pcmes_profondeurlacustre, pcmes_valeur, colour = as.character(pcmes_date)))
  gg <- gg + geom_point(stat="identity") 
  gg <- gg + geom_line(linetype="dashed")
  if(subset(PC, (pcmes_parametresandre == 1311)) %>% distinct(pcmes_date) %>% nrow() <= 8) gg <- gg + scale_color_manual(
    values = c("#D55E00", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#CC79A7", "#999999", "#0072B2"))
  gg <- gg + labs(x = "Profondeur (m)", y = expression(Oxygène~dissous~(mg~O[2]/L)), color = "Date") # Pour changer le titre
  gg <- gg + coord_flip() # pour inverser l'affichage des X et des Y
  gg <- gg + theme_bw()
  gg
  }

  else if(param=="O2pourc"){
  gg <- ggplot(subset(PC, (pcmes_parametresandre == 1312)), aes(-1*pcmes_profondeurlacustre, pcmes_valeur, colour = as.character(pcmes_date)))
  gg <- gg + geom_point(stat="identity")
  gg <- gg + geom_line(linetype="dashed")
  if(subset(PC, (pcmes_parametresandre == 1312)) %>% distinct(pcmes_date) %>% nrow() <= 8) gg <- gg + scale_color_manual(
    values = c("#D55E00", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#CC79A7", "#999999", "#0072B2"))
  gg <- gg + labs(x = "Profondeur (m)", y = expression(Oxygène~dissous~("%"~saturation~O[2]/L)), color = "Date") # Pour changer le titre
  gg <- gg + coord_flip() # pour inverser l'affichage des X et des Y
  gg <- gg + theme_bw()
  gg
  }
  
  else if(param=="cond"){
  gg <- ggplot(subset(PC, (pcmes_parametresandre == 1303)), aes(-1*pcmes_profondeurlacustre, pcmes_valeur, colour = as.character(pcmes_date)))
  gg <- gg + geom_point(stat="identity")
  gg <- gg + geom_line(linetype="dashed")
  if(subset(PC, (pcmes_parametresandre == 1303)) %>% distinct(pcmes_date) %>% nrow() <= 8) gg <- gg + scale_color_manual(
    values = c("#D55E00", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#CC79A7", "#999999", "#0072B2"))
  gg <- gg + labs(x = "Profondeur (m)", y = expression(Conductivite~corrigee~à~25~degree*C~(paste(mu,S))), color = "Date") # Pour changer le titre
  gg <- gg + coord_flip() # pour inverser l'affichage des X et des Y
  gg <- gg + theme_bw()
  gg
  }
  
  else if(param=="ph"){
  gg <- ggplot(subset(PC, (pcmes_parametresandre == 1302)), aes(-1*pcmes_profondeurlacustre, pcmes_valeur, colour = as.character(pcmes_date)))
  gg <- gg + geom_point(stat="identity")
  gg <- gg + geom_line(linetype="dashed")
  if(subset(PC, (pcmes_parametresandre == 1302)) %>% distinct(pcmes_date) %>% nrow() <= 8) gg <- gg + scale_color_manual(
    values = c("#D55E00", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#CC79A7", "#999999", "#0072B2"))
  gg <- gg + labs(x = "Profondeur (m)", y = expression(pH~(unite~pH)), color = "Date") # Pour changer le titre
  gg <- gg + coord_flip() # pour inverser l'affichage des X et des Y
  gg <- gg + theme_bw()
  gg
  }
  
  else if(param=="temp"){
  gg <- ggplot(subset(PC, (pcmes_parametresandre == 1301)), aes(-1*pcmes_profondeurlacustre, pcmes_valeur, colour = as.character(pcmes_date)))
  gg <- gg + geom_point(stat="identity") 
  gg <- gg + geom_line(linetype="dashed")
  if(subset(PC, (pcmes_parametresandre == 1301)) %>% distinct(pcmes_date) %>% nrow() <= 8) gg <- gg + scale_color_manual(
    values = c("#D55E00", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#CC79A7", "#999999", "#0072B2"))
  gg <- gg + labs(x = "Profondeur (m)", y = expression(Temperature~(degree*C)), color = "Date") # Pour changer le titre
  gg <- gg + coord_flip() # pour inverser l'affichage des X et des Y
  gg <- gg + theme_bw()
  gg
  }
  
  else if(param=="redox"){
    gg <- ggplot(subset(PC, (pcmes_parametresandre == 1330)), aes(-1*pcmes_profondeurlacustre, pcmes_valeur, colour = as.character(pcmes_date)))
    gg <- gg + geom_point(stat="identity") 
    gg <- gg + geom_line(linetype="dashed")
    if(subset(PC, (pcmes_parametresandre == 1330)) %>% distinct(pcmes_date) %>% nrow() <= 8) gg <- gg + scale_color_manual(
      values = c("#D55E00", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#CC79A7", "#999999", "#0072B2"))
    gg <- gg + labs(x = "Profondeur (m)", y = expression(paste("Potentiel d'oxydo-réduction (mV)")), color = "Date") # Pour changer le titre
    gg <- gg + coord_flip() # pour inverser l'affichage des X et des Y
    gg <- gg + theme_bw()
    gg
  }  
  
  else if(param=="chlorophylles"){
    gg <- ggplot(subset(PC, (pcmes_parametrenom == "Chlorophylle")), aes(-1*pcmes_profondeurlacustre, pcmes_valeur, colour = as.character(pcmes_date)))
    gg <- gg + geom_point(stat="identity") 
    gg <- gg + geom_line(linetype="dashed")
    if(subset(PC, (pcmes_parametrenom == "Chlorophylle")) %>% distinct(pcmes_date) %>% nrow() <= 8) gg <- gg + scale_color_manual(
      values = c("#D55E00", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#CC79A7", "#999999", "#0072B2"))
    gg <- gg + labs(x = "Profondeur (m)", y = expression(Chlorophylles~(paste(mu,g/L))), color = "Date") # Pour changer le titre
    gg <- gg + coord_flip() # pour inverser l'affichage des X et des Y
    gg <- gg + theme_bw()
    gg
  }
  
  else if(param=="phycocyanines"){
    gg <- ggplot(subset(PC, (pcmes_parametresandre == 7844)), aes(-1*pcmes_profondeurlacustre, pcmes_valeur, colour = as.character(pcmes_date)))
    gg <- gg + geom_point(stat="identity") 
    gg <- gg + geom_line(linetype="dashed")
    if(subset(PC, (pcmes_parametresandre == 7844)) %>% distinct(pcmes_date) %>% nrow() <= 8) gg <- gg + scale_color_manual(
      values = c("#D55E00", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#CC79A7", "#999999", "#0072B2"))
    gg <- gg + labs(x = "Profondeur (m)", y = expression(Phycocyanines~(paste(mu,g/L))), color = "Date") # Pour changer le titre
    gg <- gg + coord_flip() # pour inverser l'affichage des X et des Y
    gg <- gg + theme_bw()
    gg
  }
  
  else if(param=="secchi"){
    gg <- ggplot(subset(PC, (pcmes_parametresandre == 1332)), aes(-1*pcmes_valeur, as.character(pcmes_date)))
    gg <- gg + geom_point(stat="identity") 
    gg <- gg + labs(x = "Profondeur du disque de Secchi (m)", y = "Date") # Pour changer le titre
    gg <- gg + coord_flip() # pour inverser l'affichage des X et des Y
    gg <- gg + xlim(-15, 0)
    gg <- gg + theme_bw()
    gg <- gg + theme(axis.text.x  = element_text(angle=315))
    gg
  }

} # Fin de la fonction