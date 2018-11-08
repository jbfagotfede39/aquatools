#' Représentation de chroniques
#'
#' Cette fonction permet de représenter des chroniques de mesures (température, niveaux, etc.)
#' @name chronique.figure
#' @param thermie Data.frame contenant a minima une colonne Date et une colonne Valeur
#' @param Titre Titre du graphique (vide par défaut)
#' @param legendeY Défini le texte de la légende de l'axe Y (Température (C))
#' @param duree Si \code{Complet} (par défault), affichage de l'année complète.  Si \code{Relatif}, affichage uniquement de la période concernée.
#' @param Vmm30j Si \code{FALSE} (par défault), n'affiche pas les
#'    Vmm30j.  Si \code{TRUE}, les affiche.
#' @param Vminmax Si \code{TRUE} (par défault), affiche pas les
#'    valeurs journalières minimales et maximales.  Si \code{FALSE}, ne les affiche pas.
#' @param Ymin Valeur minimale de l'axe des Y (0 par défaut)
#' @param Ymax Valeur maximale de l'axe des Y (aucune par défaut)
#' @param save Si \code{FALSE} (par défault), n'enregistre pas les
#'    figures.  Si \code{TRUE}, les enregistre.
#' @param format Défini le format d'enregistrement (par défaut .png)
#' @keywords chronique
#' @import ggplot2 dplyr
#' @export
#' @examples
#' chronique.figure(data)
#' chronique.figure(data = mesdonnees, legendeY = "Température (°C)", duree = "Complet")
#' chronique.figure(data = tableaudonnee, Titre=nom, legendeY = "Température (°C)", save=T, format=".png")

chronique.figure <- function(
    data = data,
    Titre="",
    legendeY = "Température (°C)",
    duree = "Complet",
    Vmm30j=F,
    Vminmax=T,
    Ymin=0,
    Ymax=NA,
    save=F,
    format=".png")
  {
  
##### -------------- A FAIRE -------------- #####
  # il faudra rajouter l'ajout optionnel de lignes horizontales, avec tempmin, tempmax et tempmaxextreme
  # Il faudra mettre des interrupteurs pour fixer ou non les limites des axes X (dates)
  # Évaluation du paramètre Relatif ou complet
  # Choix de l'année d'affichage de la chronique, plus pertinent que complet car pas le choix de l'année (même si détection automatique : seule la fin de la chronique pluriannuelle est conservée)
  # Rajouter une détection de durée minimale de 30 jours
  # Changer ordre max/min/moy dans légence par Max/Moy/Min
  # Faire un test pour savoir si on a un jeu de données de thermie, de hauteurs d'eau, d'hydrologie ou d'oxygénation (mg/L et %) et changer les légendes en fonction
# -------------- A FAIRE -------------- #
  #data <- DataToAdd
  
##### Mise au format des données #####

## Transformation du format des dates
data$Date <- as.Date(data$Date,format="%Y-%m-%d")

## Extraction des valeurs remarquables journalières ##

syntjour <-
  data %>%
  group_by(Date) %>%
  summarise(
    VMinJ=min(Valeur),
    VMoyJ=mean(Valeur),
    VMaxJ=max(Valeur)
    )

if(Vmm30j == T){
###T Moymax 30 J
cumuleVMaxJ <- numeric(length(syntjour$VMaxJ)-30)
for (i in 1:length(syntjour$VMaxJ)){
  if (i+29<=length(syntjour$VMaxJ)) cumuleVMaxJ[i]<-sum(syntjour$VMaxJ[i:(i+29)])}  
TMaxMoy30J <- round(max(cumuleVMaxJ)/30,1)

DateDebutTMaxMoy30J <- syntjour$Date[which(cumuleVMaxJ==max(cumuleVMaxJ))]
DateFinTMaxMoy30J <- syntjour$Date[which(cumuleVMaxJ==max(cumuleVMaxJ))+29]
  
# Pour avoir un affichage propre de l'étiquette de Tmm30j et du trait de Tmm
data.label <- data.frame(
  xdeb = DateDebutTMaxMoy30J,
  xfin = DateFinTMaxMoy30J,
  xtext = DateDebutTMaxMoy30J,
  ytmm = TMaxMoy30J,
  ytext = TMaxMoy30J+2,
  label = "Tmm30j"
)
}
#resume(syntjour)
#head(syntjour)
#tail(syntjour)
#str(syntjour)

##### Plot temps relatif sur l'échantillon de données #####

  plotrelatif <- ggplot(syntjour, aes(Date))
  plotrelatif <- plotrelatif + geom_line(aes(y = VMoyJ, colour = "Moy/J"))
  if(Vminmax == T) plotrelatif <- plotrelatif + geom_line(aes(y = VMinJ, colour = "Min/J"))
  if(Vminmax == T) plotrelatif <- plotrelatif + geom_line(aes(y = VMaxJ, colour = "Max/J"))
  if(Vmm30j == T){plotrelatif <- plotrelatif + geom_text(data = data.label, aes(x = xtext , y = ytext , label = label ), size = 4, color = "red", fontface="bold")
  plotrelatif <- plotrelatif + geom_segment(data = data.label, aes(x = xdeb, y = ytmm, xend = xfin, yend = ytmm), color = "red", size = 2)}
  #plotrelatif <- plotrelatif + scale_x_date(date_breaks = "1 month")
  plotrelatif <- plotrelatif + scale_x_date(date_labels = "%m/%Y")
  if(is.na(Ymax) == FALSE & is.na(Ymin) == TRUE) plotrelatif <- plotrelatif + ylim(0,as.numeric(Ymax))
  if(is.na(Ymax) == FALSE & is.na(Ymin) == FALSE) plotrelatif <- plotrelatif + ylim(as.numeric(Ymin),as.numeric(Ymax))
  plotrelatif <- plotrelatif + labs(x = "", y = legendeY, title=Titre, color = "Températures :") # Pour changer le titre

if(duree == "Relatif"){
  plotrelatif
  if(save==T){ggsave(file=paste("Sorties/Vues/relatif_",Titre,format,sep=""))}
  if(save==F){return(plotrelatif)}
}

##### Plot temps absolu sur une année ####
if(length(unique(format(syntjour$Date,"%Y"))) == 1){
  plotabsolu <-
    plotabsolu <- ggplot(syntjour, aes(Date))
    plotabsolu <- plotabsolu + geom_line(aes(y = VMoyJ, colour = "Moy/J"))
    if(Vminmax == T) plotabsolu <- plotabsolu + geom_line(aes(y = VMinJ, colour = "Min/J"))
    if(Vminmax == T) plotabsolu <- plotabsolu + geom_line(aes(y = VMaxJ, colour = "Max/J"))
    if(Vmm30j == T){plotabsolu <- plotabsolu + geom_text(data = data.label, aes(x = xtext , y = ytext , label = label ), size = 4, color = "red", fontface="bold")
    plotabsolu <- plotabsolu + geom_segment(data = data.label, aes(x = xdeb, y = ytmm, xend = xfin, yend = ytmm), color = "red", size = 2)}
    plotabsolu <- plotabsolu + scale_x_date(#breaks = "2 month", 
      #labels = date_format("%m-%Y"), # Ne fonctionne pas
      limits = as.Date(c(paste(as.numeric(format(syntjour$Date[1],"%Y"))-1,"-10-01",sep=""),paste(format(syntjour$Date[length(syntjour$Date)],"%Y"),"-09-30",sep=""))))
    
    #scale_x_date(breaks = "2 month", #### Save qui fonctionne avant le test automatique plus haut
    #labels = date_format("%m-%Y"), # Ne fonctionne pas
    #limits = as.Date(c('2013-10-01','2014-09-30'))) +
    if(is.na(Ymax) == FALSE & is.na(Ymin) == TRUE) plotabsolu <- plotabsolu + ylim(0,as.numeric(Ymax))
    if(is.na(Ymax) == FALSE & is.na(Ymin) == FALSE) plotabsolu <- plotabsolu + ylim(as.numeric(Ymin),as.numeric(Ymax))
    plotabsolu <- plotabsolu + labs(x = "", y = legendeY, title=Titre, color = "Températures :") # Pour changer le titre
  #if(save==T){ggsave(file=paste("Sorties/Vues/absolu_",Titre,format,sep=""))}
}

if(length(unique(format(syntjour$Date,"%Y"))) == 2){
  
  plotabsolu <- ggplot(syntjour, aes(Date))
  plotabsolu <- plotabsolu + geom_line(aes(y = VMoyJ, colour = "Moy/J"))
  if(Vminmax == T) plotabsolu <- plotabsolu + geom_line(aes(y = VMinJ, colour = "Min/J"))
  if(Vminmax == T) plotabsolu <- plotabsolu + geom_line(aes(y = VMaxJ, colour = "Max/J"))
  if(Vmm30j == T){plotabsolu <- plotabsolu + geom_text(data = data.label, aes(x = xtext , y = ytext , label = label ), size = 4, color = "red", fontface="bold")
  plotabsolu <- plotabsolu + geom_segment(data = data.label, aes(x = xdeb, y = ytmm, xend = xfin, yend = ytmm), color = "red", size = 2)}
  plotabsolu <- plotabsolu + scale_x_date(#breaks = "2 month", 
      #labels = date_format("%m-%Y"), # Ne fonctionne pas
      limits = as.Date(c(paste(format(syntjour$Date[1],"%Y"),"-10-01",sep=""),paste(format(syntjour$Date[length(syntjour$Date)],"%Y"),"-09-30",sep=""))))
    
    #scale_x_date(breaks = "2 month", #### Save qui fonctionne avant le test automatique plus haut
    #labels = date_format("%m-%Y"), # Ne fonctionne pas
    #limits = as.Date(c('2013-10-01','2014-09-30'))) +
  if(is.na(Ymax) == FALSE & is.na(Ymin) == TRUE) plotabsolu <- plotabsolu + ylim(0,as.numeric(Ymax))
  if(is.na(Ymax) == FALSE & is.na(Ymin) == FALSE) plotabsolu <- plotabsolu + ylim(as.numeric(Ymin),as.numeric(Ymax))
  plotabsolu <- plotabsolu + labs(x = "", y = legendeY, title=Titre, color = "Températures :") # Pour changer le titre
 # if(save==T){ggsave(file=paste("Sorties/Vues/absolu_",Titre,format,sep=""))}
}

if(duree == "Complet"){
  plotabsolu
  if(save==T){ggsave(file=paste("Sorties/Vues/absolu_",Titre,format,sep=""))}
  if(save==F){return(plotabsolu)}
}

} # Fin de la fonction