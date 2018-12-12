#' Représentation de chroniques
#'
#' Cette fonction permet de représenter des chroniques de mesures (température, niveaux, etc.)
#' @name chronique.figure
#' @param data Data.frame contenant a minima une colonne chmes_date et une colonne chmes_valeur
#' @param Titre Titre du graphique (vide par défaut)
#' @param typemesure Défini le type de données et modifie les légendes en fonction. Ignoré si le champ chmes_typemesure est présent dans data
#' @param duree Si \code{Complet} (par défault), affichage de l'année complète.  Si \code{Relatif}, affichage uniquement de la période concernée.
#' @param Vmm30j Si \code{FALSE} (par défault), n'affiche pas les
#'    Vmm30j.  Si \code{TRUE}, les affiche.
#' @param Vminmax Si \code{TRUE} (par défault), affiche pas les
#'    valeurs journalières minimales et maximales.  Si \code{FALSE}, ne les affiche pas.
#' @param Ymin Valeur minimale de l'axe des Y (0 par défaut)
#' @param Ymax Valeur maximale de l'axe des Y (aucune par défaut)
#' @param save Si \code{FALSE} (par défault), n'enregistre pas les
#'    figures.  Si \code{TRUE}, les enregistre.
#' @param projet Nom du projet
#' @param format Défini le format d'enregistrement (par défaut .png)
#' @keywords chronique
#' @import ggplot2 dplyr
#' @export
#' @examples
#' chronique.figure(data)
#' chronique.figure(data = mesdonnees, typemesure = "Thermie", duree = "Complet")
#' chronique.figure(data = tableaudonnee, Titre=nom, typemesure = "Barométrie", save=T, format=".png")

chronique.figure <- function(
    data = data,
    Titre="",
    typemesure = c("Thermie", "Thermie barométrique", "Thermie piézométrique", "Barométrie", "Piézométrie", "Piézométrie brute", "Piézométrie compensée", "Oxygénation", "Hydrologie", "Pluviométrie"),
    duree = c("Complet", "Relatif"),
    Vmm30j=F,
    Vminmax=T,
    Ymin=0,
    Ymax=NA,
    save=F,
    projet = as.character(NA),
    format=".png")
  {

  ## Évaluation des choix
  typemesure <- match.arg(typemesure)
  duree <- match.arg(duree)

##### -------------- A FAIRE -------------- #####
  # il faudra rajouter l'ajout optionnel de lignes horizontales, avec tempmin, tempmax et tempmaxextreme
  # Il faudra mettre des interrupteurs pour fixer ou non les limites des axes X (dates)
  # Changer ordre max/min/moy dans légende par Max/Moy/Min
  # Il faudra faire une fonction commune (entre chronique.figure, chronique.agregation et chronique.analyse) pour créer un contexte propre de chronique
# -------------- A FAIRE -------------- #
  
##### Mise au format des données #####
## Transformation du format des dates
data$chmes_date <- as.Date(data$chmes_date,format="%Y-%m-%d")

##### Contexte de la chronique #####
# Calcul du nombre de stations ##
if("chmes_coderhj" %in% colnames(data)) Contexte <- tibble(nStations = n_distinct(data$chmes_coderhj))
if("chmes_coderhj" %in% colnames(data) == FALSE){
  Contexte$nStations <- 1
  data <- data %>% mutate(chmes_coderhj = NA)
}

if(Contexte$nStations == 0) stop("Aucune donnée dans la chronique à analyser")
if(Contexte$nStations > 1) stop("Différentes stations dans la chronique à analyser")
  
# chmes_typemesure
  if(testit::has_error(data %>% 
                       distinct(chmes_typemesure) %>% 
                       bind_cols(Contexte)) == TRUE) stop("Plusieurs chmes_typemesure au sein du jeu de données")
  Contexte <- 
    data %>% 
    distinct(chmes_typemesure) %>% 
    bind_cols(Contexte)
  typemesure <- Contexte$chmes_typemesure

# nJours
  Contexte$nJours <- n_distinct(syntjour$chmes_date)
  
#### Valeurs remarquables journalières ####
DataTravail <- chronique.agregation(data)
syntjour <- DataTravail[[2]]

## Calcul de la Vmm30j ##
if(Contexte$nJours < 30){
  Vmm30j == F
  warning("Durée inférieure à 30 jours : pas d'affichage de la Vmm30j")
}
if(Vmm30j == T & Contexte$nStations != 1){
###T Moymax 30 J
cumuleVMaxJ <- numeric(length(syntjour$VMaxJ)-30)
for (i in 1:length(syntjour$VMaxJ)){
  if (i+29<=length(syntjour$VMaxJ)) cumuleVMaxJ[i]<-sum(syntjour$VMaxJ[i:(i+29)])}  
VMaxMoy30J <- round(max(cumuleVMaxJ)/30,1)

DateDebutVMaxMoy30J <- syntjour$chmes_date[which(cumuleVMaxJ==max(cumuleVMaxJ))]
DateFinVMaxMoy30J <- syntjour$chmes_date[which(cumuleVMaxJ==max(cumuleVMaxJ))+29]
  
# Pour avoir un affichage propre de l'étiquette de Tmm30j et du trait de Tmm
data.label <- data.frame(
  xdeb = DateDebutVMaxMoy30J,
  xfin = DateFinVMaxMoy30J,
  xtext = DateDebutVMaxMoy30J,
  ytmm = VMaxMoy30J,
  ytext = VMaxMoy30J+2,
  label = "Tmm30j"
)
}
#resume(syntjour)
#head(syntjour)
#tail(syntjour)
#str(syntjour)

#### Ajustement des paramètres en fonction du typemesure ####
if(typemesure == "Thermie" | typemesure == "Thermie barométrique" | typemesure == "Thermie piézométrique"){
  legendeY = "Température (°C)"
  legendeTitre = "Températures :"
  typemesureTitreSortie = "_thermie_"
  }
if(typemesure == "Barométrie"){
  legendeY = "Pression atmosphérique (kPa)"
  legendeTitre = "Barométrie :"
  typemesureTitreSortie = "_barométrie_"
  }
if(typemesure == "Piézométrie" | typemesure == "Piézométrie brute" | typemesure == "Piézométrie compensée"){
  legendeY = "Hauteur d'eau (cm)"
  legendeTitre = "Piézométrie :"
  typemesureTitreSortie = "_piézométrie_"
  }
if(typemesure == "Oxygénation"){
  legendeY = expression(Oxygene~dissous~(mg~O[2]/L))
  legendeTitre = "Oxygénation :"
  typemesureTitreSortie = "_oxygénation_"
  }
if(typemesure == "Hydrologie"){
  legendeY = expression(Débit~(m^3/s))
  legendeTitre = "Hydrologie :"
  typemesureTitreSortie = "_hydrologie_"
}
if(typemesure == "Pluviométrie"){
  legendeY = expression(Précipitations~(L/m^2))
  legendeTitre = "Pluviométrie :"
  typemesureTitreSortie = "_pluviométrie_"
}

##### Plot temps relatif sur l'échantillon de données #####

plotrelatif <- ggplot(syntjour, aes(chmes_date))
if(Contexte$nStations == 1) plotrelatif <- plotrelatif + geom_line(aes(y = VMoyJ, colour = "Moy/J"))
if(Contexte$nStations != 1) plotrelatif <- plotrelatif + geom_line(aes(y = VMoyJ, colour = chmes_coderhj))
if(Vminmax == T & Contexte$nStations == 1) plotrelatif <- plotrelatif + geom_line(aes(y = VMinJ, colour = "Min/J"))
if(Vminmax == T & Contexte$nStations == 1) plotrelatif <- plotrelatif + geom_line(aes(y = VMaxJ, colour = "Max/J"))
if(Vmm30j == T & Contexte$nStations == 1){plotrelatif <- plotrelatif + geom_text(data = data.label, aes(x = xtext , y = ytext , label = label ), size = 4, color = "red", fontface="bold")
plotrelatif <- plotrelatif + geom_segment(data = data.label, aes(x = xdeb, y = ytmm, xend = xfin, yend = ytmm), color = "red", size = 2)}
plotrelatif <- plotrelatif + scale_x_date(date_labels = "%m/%Y")
if(is.na(Ymax) == FALSE & is.na(Ymin) == TRUE) plotrelatif <- plotrelatif + ylim(0,as.numeric(Ymax))
if(is.na(Ymax) == FALSE & is.na(Ymin) == FALSE) plotrelatif <- plotrelatif + ylim(as.numeric(Ymin),as.numeric(Ymax))
plotrelatif <- plotrelatif + labs(x = "", y = legendeY, title=Titre, color = legendeTitre) # Pour changer le titre

if(duree == "Relatif"){
  plotrelatif
  if(save==T){ggsave(file=paste(projet,"/Sorties/Vues/relatif",typemesureTitreSortie, Titre,format,sep=""))}
  if(save==F){return(plotrelatif)}
}

##### Plot temps absolu sur une année ####
if(length(unique(format(syntjour$chmes_date,"%Y"))) == 1){
    plotabsolu <- ggplot(syntjour, aes(chmes_date))
    if(Contexte$nStations == 1) plotabsolu <- plotabsolu + geom_line(aes(y = VMoyJ, colour = "Moy/J"))
    if(Contexte$nStations != 1) plotabsolu <- plotabsolu + geom_line(aes(y = VMoyJ, colour = chmes_coderhj))
    if(Vminmax == T & Contexte$nStations == 1) plotabsolu <- plotabsolu + geom_line(aes(y = VMinJ, colour = "Min/J"))
    if(Vminmax == T & Contexte$nStations == 1) plotabsolu <- plotabsolu + geom_line(aes(y = VMaxJ, colour = "Max/J"))
    if(Vmm30j == T & Contexte$nStations == 1){plotabsolu <- plotabsolu + geom_text(data = data.label, aes(x = xtext , y = ytext , label = label ), size = 4, color = "red", fontface="bold")
    plotabsolu <- plotabsolu + geom_segment(data = data.label, aes(x = xdeb, y = ytmm, xend = xfin, yend = ytmm), color = "red", size = 2)}
    plotabsolu <- plotabsolu + scale_x_date(#breaks = "2 month", 
      #labels = date_format("%m-%Y"), # Ne fonctionne pas
      limits = as.Date(c(paste(as.numeric(format(syntjour$chmes_date[1],"%Y"))-1,"-10-01",sep=""),paste(format(syntjour$chmes_date[length(syntjour$chmes_date)],"%Y"),"-09-30",sep=""))))
    
    #scale_x_date(breaks = "2 month", #### Save qui fonctionne avant le test automatique plus haut
    #labels = date_format("%m-%Y"), # Ne fonctionne pas
    #limits = as.Date(c('2013-10-01','2014-09-30'))) +
    if(is.na(Ymax) == FALSE & is.na(Ymin) == TRUE) plotabsolu <- plotabsolu + ylim(0,as.numeric(Ymax))
    if(is.na(Ymax) == FALSE & is.na(Ymin) == FALSE) plotabsolu <- plotabsolu + ylim(as.numeric(Ymin),as.numeric(Ymax))
    plotabsolu <- plotabsolu + labs(x = "", y = legendeY, title=Titre, color = legendeTitre) # Pour changer le titre
  #if(save==T){ggsave(file=paste(projet,"/Sorties/Vues/absolu_",Titre,format,sep=""))}
}

if(length(unique(format(syntjour$chmes_date,"%Y"))) >= 2){ # modifié le 14/11/2018, c'était == avant
  
  plotabsolu <- ggplot(syntjour, aes(chmes_date))
  if(Contexte$nStations == 1) plotabsolu <- plotabsolu + geom_line(aes(y = VMoyJ, colour = "Moy/J"))
  if(Contexte$nStations != 1) plotabsolu <- plotabsolu + geom_line(aes(y = VMoyJ, colour = chmes_coderhj))
  if(Vminmax == T & Contexte$nStations == 1) plotabsolu <- plotabsolu + geom_line(aes(y = VMinJ, colour = "Min/J"))
  if(Vminmax == T & Contexte$nStations == 1) plotabsolu <- plotabsolu + geom_line(aes(y = VMaxJ, colour = "Max/J"))
  if(Vmm30j == T & Contexte$nStations == 1){plotabsolu <- plotabsolu + geom_text(data = data.label, aes(x = xtext , y = ytext , label = label ), size = 4, color = "red", fontface="bold")
  plotabsolu <- plotabsolu + geom_segment(data = data.label, aes(x = xdeb, y = ytmm, xend = xfin, yend = ytmm), color = "red", size = 2)}
  plotabsolu <- plotabsolu + scale_x_date(#breaks = "2 month", 
      #labels = date_format("%m-%Y"), # Ne fonctionne pas
      limits = as.Date(c(paste(format(syntjour$chmes_date[1],"%Y"),"-10-01",sep=""),paste(format(syntjour$chmes_date[length(syntjour$chmes_date)],"%Y"),"-09-30",sep=""))))
    
    #scale_x_date(breaks = "2 month", #### Save qui fonctionne avant le test automatique plus haut
    #labels = date_format("%m-%Y"), # Ne fonctionne pas
    #limits = as.Date(c('2013-10-01','2014-09-30'))) +
  if(is.na(Ymax) == FALSE & is.na(Ymin) == TRUE) plotabsolu <- plotabsolu + ylim(0,as.numeric(Ymax))
  if(is.na(Ymax) == FALSE & is.na(Ymin) == FALSE) plotabsolu <- plotabsolu + ylim(as.numeric(Ymin),as.numeric(Ymax))
  plotabsolu <- plotabsolu + labs(x = "", y = legendeY, title=Titre, color = legendeTitre) # Pour changer le titre
 # if(save==T){ggsave(file=paste(projet,"/Sorties/Vues/absolu_",Titre,format,sep=""))}
}

if(duree == "Complet"){
  plotabsolu
  if(save==T){ggsave(file=paste(projet,"/Sorties/Vues/absolu",typemesureTitreSortie,Titre,format,sep=""))}
  if(save==F){return(plotabsolu)}
}

} # Fin de la fonction