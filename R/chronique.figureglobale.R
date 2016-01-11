#' Représentation de chroniques
#'
#' Cette fonction permet de représenter des chroniques de mesures (température, niveaux, etc.)
#' 
#' @param thermie Data.frame contenant a minima une colonne Date et une colonne Temp
#' @param st Titre du graphique (vide par défaut)
#' @param legendeY Défini le texte de la légende de l'axe Y (Température (C))
#' @param save Si \code{FALSE} (par défault), n'enregistre pas les
#'    figures.  Si \code{TRUE}, les enregistre.
#' @param format Défini le format d'enregistrement (par défaut .png)
#' @keywords temperature
#' @import ggplot2 dplyr
#' @export
#' @examples
#' chronique.figureglobale(thermie)
#' chronique.figureglobale(thermie = mesdonnees, legendeY = "Température (°C)")
#' chronique.figureglobale(thermie = tableaudonnee, st=nom, legendeY = "Température (°C)", save=T, format=".png")

chronique.figureglobale <- function(
  thermie = tab,
  st="",
  legendeY = "Température (°C)",
  save=F,
  format=".png")
{
  
  
  ##### -------------- A FAIRE -------------- #####
  
  # il faudra rajouter l'ajout optionnel de lignes horizontales, avec tempmin, tempmax et tempmaxextreme
  # Il faudra mettre des interrupteurs pour fixer ou non les limites des axes X (dates) et Y (temp)
  
  ##### -------------- A FAIRE -------------- #####
  
  ## Chargement des données pour travaux ##
  #thermie <- read.delim2("./Données-test/ANG0-7_10316505_ANG1-a2014.csv",header=TRUE,sep=";")
  #thermie$Temp <- as.numeric( sub(",", ".", thermie$Temp))
  #library(ggplot2);library(dplyr);library(aquatools)
  
  #st="saineamontconflemme2011";legendeY = "Température (°C)";save=T;format=".png"
  #str(thermie)
  #resume(thermie)
  #thermie <- Mesures
  
  ##### Mise au format des données #####
  
  ## Transformation du format des dates
  thermie$Date <- as.Date(thermie$Date,format="%Y-%m-%d")
  
  ## Extraction des valeurs remarquables journalières ##
  
  syntjour <-
    thermie %>%
    group_by(Date) %>%
    summarise(
      TMinJ=min(Temperature),
      TMoyJ=mean(Temperature),
      TMaxJ=max(Temperature)
    )
  
  ###T Moymax 30 J
  cumuleTMaxJ <- numeric(length(syntjour$TMaxJ)-30)
  for (i in 1:length(syntjour$TMaxJ)){
    if (i+29<=length(syntjour$TMaxJ)) cumuleTMaxJ[i]<-sum(syntjour$TMaxJ[i:(i+29)])}  
  TMaxMoy30J <- round(max(cumuleTMaxJ)/30,1)
  
  DateDebutTMaxMoy30J <- syntjour$Date[which(cumuleTMaxJ==max(cumuleTMaxJ))]
  DateFinTMaxMoy30J <- syntjour$Date[which(cumuleTMaxJ==max(cumuleTMaxJ))+29]
  
  # Pour avoir un affichage propre de l'étiquette de Tmm30j et du trait de Tmm
  data.label <- data.frame(
    xdeb = DateDebutTMaxMoy30J,
    xfin = DateFinTMaxMoy30J,
    xtext = DateDebutTMaxMoy30J,
    ytmm = TMaxMoy30J,
    ytext = TMaxMoy30J+2,
    label = "Tmm30j"
  )
  
  #resume(syntjour)
  #head(syntjour)
  #tail(syntjour)
  
  ##### Plot temps relatif sur l'échantillon de données #####
  plotrelatif <-
    ggplot(syntjour, aes(Date)) +
    geom_line(aes(y = TMinJ, colour = "Min/J")) +
    geom_line(aes(y = TMoyJ, colour = "Moy/J")) +
    geom_line(aes(y = TMaxJ, colour = "Max/J")) +
    geom_text(data = data.label, aes(x = xtext , y = ytext , label = label ), size = 4, color = "red", fontface="bold") +
    geom_segment(data = data.label, aes(x = xdeb, y = ytmm, xend = xfin, yend = ytmm), color = "red", size = 2) +
    scale_x_date(date_breaks = "1 month") +
    #ylim(0,25) +
    labs(x = "", y = legendeY, title=st, color = "Températures :") # Pour changer le titre
  plotrelatif
  if(save==T){ggsave(file=paste("Sorties/Vues/relatif_",st,format,sep=""))}
  
  
  ##### Plot temps absolu sur une année ####
  if(length(unique(format(syntjour$Date,"%Y"))) == 1){
    ggplot(syntjour, aes(Date)) +
      geom_line(aes(y = TMinJ, colour = "Min/J")) +
      geom_line(aes(y = TMoyJ, colour = "Moy/J")) +
      geom_line(aes(y = TMaxJ, colour = "Max/J")) +
      geom_text(data = data.label, aes(x = xtext , y = ytext , label = label ), size = 4, color = "red", fontface="bold") +
      geom_segment(data = data.label, aes(x = xdeb, y = ytmm, xend = xfin, yend = ytmm), color = "red", size = 2) +
      scale_x_date(#breaks = "2 month", 
        #labels = date_format("%m-%Y"), # Ne fonctionne pas
        limits = as.Date(c(paste(as.numeric(format(syntjour$Date[1],"%Y"))-1,"-10-01",sep=""),paste(format(syntjour$Date[length(syntjour$Date)],"%Y"),"-09-30",sep="")))) +
      
      #scale_x_date(breaks = "2 month", #### Save qui fonctionne avant le test automatique plus haut
      #labels = date_format("%m-%Y"), # Ne fonctionne pas
      #limits = as.Date(c('2013-10-01','2014-09-30'))) +
      ylim(0,25) +
      labs(x = "", y = legendeY, title=st, color = "Températures :") # Pour changer le titre
    if(save==T){ggsave(file=paste("Sorties/Vues/absolu_",st,format,sep=""))}
  }
  
  if(length(unique(format(syntjour$Date,"%Y"))) == 2){
    ggplot(syntjour, aes(Date)) +
      geom_line(aes(y = TMinJ, colour = "Min/J")) +
      geom_line(aes(y = TMoyJ, colour = "Moy/J")) +
      geom_line(aes(y = TMaxJ, colour = "Max/J")) +
      geom_text(data = data.label, aes(x = xtext , y = ytext , label = label ), size = 4, color = "red", fontface="bold") +
      geom_segment(data = data.label, aes(x = xdeb, y = ytmm, xend = xfin, yend = ytmm), color = "red", size = 2) +
      scale_x_date(#breaks = "2 month", 
        #labels = date_format("%m-%Y"), # Ne fonctionne pas
        limits = as.Date(c(paste(format(syntjour$Date[1],"%Y"),"-10-01",sep=""),paste(format(syntjour$Date[length(syntjour$Date)],"%Y"),"-09-30",sep="")))) +
      
      #scale_x_date(breaks = "2 month", #### Save qui fonctionne avant le test automatique plus haut
      #labels = date_format("%m-%Y"), # Ne fonctionne pas
      #limits = as.Date(c('2013-10-01','2014-09-30'))) +
      ylim(0,25) +
      labs(x = "", y = legendeY, title=st, color = "Températures :") # Pour changer le titre
    if(save==T){ggsave(file=paste("Sorties/Vues/absolu_",st,format,sep=""))}
  }
  
  return(plotrelatif)
  
} # Fin de la fonction