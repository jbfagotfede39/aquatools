#' Création de profils verticaux lacustres
#'
#' Cette fonction permet de créer des profils graphiques de paramètres physico-chimiques
#' @param O2mg 
#' @param O2pourc
#' @param Cond 
#' @param ph
#' @keywords cats
#' @export
#' @import ggplot2
#' @examples
#' profil.lac(PC,param="O2mg")
#' profil.lac(PC,param="O2pourc")
#' profil.lac(PC,param="Cond")
#' profil.lac(PC,param="ph")

# Il faudra affiner la présentation
# Il faudra rajouter un paramètre save=T, en sous option if dans chaque bloc

profil.lac<-function(PC,param="O2mg"){
  
  if(param=="O2mg"){
  ggplot(PC, aes(Profondeurcor)) +
  geom_point(aes(y = O2mg, colour = "O2 (mg/L)"),size = 5) +
  geom_point(aes(y = T, colour = "Température (°C)"),size = 5) +
  geom_line(aes(y = O2mg, colour = "O2 (mg/L)")) +
  geom_line(aes(y = T, colour = "Température (°C)")) +
  labs(color = "Légende",x = "Profondeur (m)", y = "",title=paste(levels(PC$Lac)[1],levels(PC$Date)[1],sep=" le ")) + # Pour changer le titre de la légende
  coord_flip() # pour inverser l'affichage des X et des Y
  }

  else if(param=="O2pourc"){
  ggplot(PC, aes(Profondeurcor)) +
    geom_point(aes(y = O2pourc, colour = "O2 (% saturation)"),size = 5) +
    geom_point(aes(y = T, colour = "Température (°C)"),size = 5) +
    geom_line(aes(y = O2pourc, colour = "O2 (% saturation)")) +
    geom_line(aes(y = T, colour = "Température (°C)")) +
    labs(color = "Légende",x = "Profondeur (m)", y = "",title=paste(levels(PC$Lac)[1],levels(PC$Date)[1],sep=" le ")) + # Pour changer le titre de la légende
    coord_flip() # pour inverser l'affichage des X et des Y
  }
  
  else if(param=="Cond"){
    ggplot(PC, aes(Profondeurcor)) +
      geom_point(aes(y = Cond, colour = "Conductivité (uS)"),size = 5) +
      geom_point(aes(y = T, colour = "Température (°C)"),size = 5) +
      geom_line(aes(y = Cond, colour = "Conductivité (uS)")) +
      geom_line(aes(y = T, colour = "Température (°C)")) +
      labs(color = "Légende",x = "Profondeur (m)", y = "",title=paste(levels(PC$Lac)[1],levels(PC$Date)[1],sep=" le ")) + # Pour changer le titre de la légende
      coord_flip() # pour inverser l'affichage des X et des Y
  }
  
  else if(param=="ph"){
    ggplot(PC, aes(Profondeurcor)) +
      geom_point(aes(y = ph, colour = "pH"),size = 5) +
      geom_point(aes(y = T, colour = "Température (°C)"),size = 5) +
      geom_line(aes(y = ph, colour = "pH")) +
      geom_line(aes(y = T, colour = "Température (°C)")) +
      labs(color = "Légende",x = "Profondeur (m)", y = "",title=paste(levels(PC$Lac)[1],levels(PC$Date)[1],sep=" le ")) + # Pour changer le titre de la légende
      coord_flip() # pour inverser l'affichage des X et des Y
  }

} # Fin de la fonction