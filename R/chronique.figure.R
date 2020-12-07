#' Représentation de chroniques
#'
#' Cette fonction permet de représenter des chroniques de mesures (température, niveaux, etc.)
#' @name chronique.figure
#' @param data Data.frame contenant a minima une colonne chmes_date et une colonne chmes_valeur
#' @param Titre Titre du graphique (vide par défaut)
#' @param typemesure Ignoré si le champ chmes_typemesure est présent dans data. Défini le type de données et modifie les légendes en fonction (\code{Thermie}, \code{Thermie barométrique}, \code{Thermie piézométrique}, \code{Barométrie}, \code{Piézométrie}, \code{Piézométrie brute}, \code{Piézométrie compensée}, \code{Piézométrie NGF}, \code{Oxygénation}, \code{Hydrologie}, \code{Pluviométrie}.
#' @param duree Si \code{Complet} (par défault), affichage de l'année complète.  Si \code{Relatif}, affichage uniquement de la période concernée.
#' @param complement Si \code{TRUE}, complément de la chronique avec les données manquantes (\code{FALSE} par défaut)
#' @param Vmm30j Si \code{FALSE} (par défault), n'affiche pas les
#'    Vmm30j.  Si \code{TRUE}, les affiche.
#' @param Vminmax Si \code{TRUE} (par défault), affiche pas les
#'    valeurs journalières minimales et maximales.  Si \code{FALSE}, ne les affiche pas.
#' @param Ymin Valeur minimale de l'axe des Y (-1 par défaut)
#' @param Ymax Valeur maximale de l'axe des Y (aucune par défaut)
#' @param save Si \code{FALSE} (par défault), n'enregistre pas les
#'    figures.  Si \code{TRUE}, les enregistre.
#' @param projet Nom du projet
#' @param format Défini le format d'enregistrement (par défaut .png)
#' @keywords chronique
#' @import ggplot2 
#' @import tidyverse
#' @export
#' @examples
#' chronique.figure(data)
#' chronique.figure(data = mesdonnees, typemesure = "Thermie", duree = "Complet")
#' chronique.figure(data = tableaudonnee, Titre=nom, typemesure = "Barométrie", save=T, format=".png")

chronique.figure <- function(
    data = data,
    Titre="",
    typemesure = c("Thermie", "Thermie barométrique", "Thermie piézométrique", "Barométrie", "Piézométrie", "Piézométrie brute", "Piézométrie compensée", "Piézométrie calée", "Piézométrie NGF", "Oxygénation", "Hydrologie", "Pluviométrie"),
    duree = c("Complet", "Relatif"),
    complement = FALSE,
    Vmm30j=F,
    Vminmax=T,
    Ymin=-1,
    Ymax=NA,
    save=F,
    projet = as.character(NA),
    format=".png")
  {

  
  ##### -------------- A FAIRE -------------- #####
  # Remplacer l'appel de data_frame() par tibble()
  # il faudra rajouter l'ajout optionnel de lignes horizontales, avec tempmin, tempmax et tempmaxextreme
  # Il faudra mettre des interrupteurs pour fixer ou non les limites des axes X (dates)
  # Changer ordre max/min/moy dans légende par Max/Moy/Min
  # Il faudra faire une fonction commune (entre chronique.figure, chronique.figure.cumul, chronique.agregation et chronique.analyse) pour créer un contexte propre de chronique
  # -------------- A FAIRE -------------- #
  
  ## Évaluation des choix
  typemesure <- match.arg(typemesure)
  duree <- match.arg(duree)

##### Mise au format des données #####
## Transformation du format des dates
if(class(data$chmes_date) != "Date"){#data$chmes_date <- as.Date(data$chmes_date,format="%Y-%m-%d") # ancien format du 08/04/19
data$chmes_date <- ymd(data$chmes_date)}
  
#### Test de cohérence ####
if("chmes_unite" %in% colnames(data)){if(data %>% dplyr::filter(chmes_unite == "kPa") %>% count() %>% pull() != 0) warning("Attention pour les piézo, des données en kPa seront représentées avec une échelle en cm d'eau, alors que 1 kPa = 10,1972 cm d'H2O")}
  
##### Contexte de la chronique #####
# Calcul du nombre de stations ##
if("chmes_coderhj" %in% colnames(data)) Contexte <- tibble(nStations = n_distinct(data$chmes_coderhj))
if("chmes_coderhj" %in% colnames(data) == FALSE){
  Contexte <- data_frame(nStations = 1)
  data <- data %>% mutate(chmes_coderhj = NA)
}
if("chmes_typemesure" %in% colnames(data) == FALSE){
  data <- data %>% mutate(chmes_typemesure = typemesure)
}

if(Contexte$nStations == 0) stop("Aucune donnée dans la chronique à analyser")
if(Contexte$nStations > 1) warning("Différentes stations dans la chronique à analyser - Cas en test à partir du rapport N2000 Vogna et de 2019-05-15_Calcul_résultats_chroniques_Vouglans.R")
  
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
  Contexte$nJours <- n_distinct(data$chmes_date)
  
#### Valeurs remarquables journalières ####
  if(Contexte$nStations == 1){
    if(complement == FALSE) DataTravail <- chronique.agregation(data)
    if(complement == TRUE) DataTravail <- chronique.agregation(data, complement = T)
    syntjour <- DataTravail %>% purrr::pluck(2)
  }
  
  if(Contexte$nStations > 1){
    listeStations <- distinct(data, chmes_coderhj) %>% dplyr::pull()
    for(i in 1:Contexte$nStations){
      DataTravail <- data %>% dplyr::filter(chmes_coderhj == listeStations %>% purrr::pluck(i))
      if(complement == FALSE){DataTravail <- chronique.agregation(DataTravail)}
      if(complement == TRUE){DataTravail <- chronique.agregation(DataTravail, complement = T)}
      if(i == 1){syntjour <- DataTravail %>% purrr::pluck(2)}
      if(i != 1){syntjour <- syntjour %>% dplyr::union(DataTravail %>% purrr::pluck(2))}
    }
  }

## Calcul de la Vmm30j ##
if(Contexte$nJours < 30 & Vmm30j == T){
  Vmm30j <- F
  warning("Durée inférieure à 30 jours : pas d'affichage de la Vmm30j")
}

if(Vmm30j == T & Contexte$nStations == 1){
###T Moymax 30 J
syntjourSansAgregation <- syntjour %>% dplyr::filter(!is.na(VMaxJ)) # Si on fait un complément du jeu de données
cumuleVMaxJ <- numeric(length(syntjourSansAgregation$VMaxJ)-30)
for (i in 1:length(syntjourSansAgregation$VMaxJ)){
  if (i+29<=length(syntjourSansAgregation$VMaxJ)) cumuleVMaxJ[i]<-sum(syntjourSansAgregation$VMaxJ[i:(i+29)])}  
VMaxMoy30J <- round(max(cumuleVMaxJ)/30,1)

DateDebutVMaxMoy30J <- syntjourSansAgregation$chmes_date[which(cumuleVMaxJ==max(cumuleVMaxJ))]
DateFinVMaxMoy30J <- syntjourSansAgregation$chmes_date[which(cumuleVMaxJ==max(cumuleVMaxJ))+29]
  
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
if(typemesure == "Piézométrie NGF"){
  legendeY = "Hauteur d'eau (NGF)"
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
  
#### Palette ####
data(PaletteSite)

##### Plot temps relatif sur l'échantillon de données #####
## Version grisée avec enveloppe sur fond clair (min/max) ##
plotrelatif <- ggplot(syntjour, aes(chmes_date))
if(Contexte$nStations == 1) plotrelatif <- plotrelatif + geom_ribbon(aes(ymin = VMinJ, ymax = VMaxJ), alpha=0.2)
if(Contexte$nStations != 1) plotrelatif <- plotrelatif + geom_line(aes(y = VMoyJ, colour = chmes_coderhj))
if(Contexte$nStations != 1) plotrelatif <- plotrelatif + scale_colour_manual(values = PaletteSite)
if(Vmm30j == T & Contexte$nStations == 1){
  plotrelatif <- plotrelatif + geom_text(data = data.label, aes(x = xtext , y = ytext , label = label ), size = 4, color = "red", fontface="bold")
  plotrelatif <- plotrelatif + geom_segment(data = data.label, aes(x = xdeb, y = ytmm, xend = xfin, yend = ytmm), color = "red", size = 2)
  }
if(length(unique(format(syntjour$chmes_date,"%m"))) < 9) plotrelatif <- plotrelatif + scale_x_date(date_labels = "%b %Y")
if(length(unique(format(syntjour$chmes_date,"%m"))) >= 9) plotrelatif <- plotrelatif + scale_x_date(date_labels = "%b %Y", date_minor_breaks = "1 month")
if(is.na(Ymax) == FALSE & is.na(Ymin) == TRUE) plotrelatif <- plotrelatif + ylim(0,as.numeric(Ymax))
if(is.na(Ymax) == FALSE & is.na(Ymin) == FALSE) plotrelatif <- plotrelatif + ylim(as.numeric(Ymin),as.numeric(Ymax))
plotrelatif <- plotrelatif + labs(x = "", y = legendeY, title=Titre, color = legendeTitre) # Pour changer le titre
plotrelatif <- plotrelatif + theme_bw()

if(duree == "Relatif"){
  plotrelatif
  if(save==T){
    if(is.na(Ymax) == TRUE & is.na(Ymin) == TRUE & Vmm30j == F) ggsave(file=paste(projet,"/Sorties/Vues/Annuelles_relatif-libre/relatif-libre",typemesureTitreSortie,Titre,format,sep=""))
    if(is.na(Ymax) == TRUE & is.na(Ymin) == TRUE & Vmm30j == T) ggsave(file=paste(projet,"/Sorties/Vues/Annuelles_relatif-libre/relatif-libre-vmm30j",typemesureTitreSortie,Titre,format,sep=""))
    if(is.na(Ymax) == FALSE & is.na(Ymin) == FALSE & Vmm30j == F) ggsave(file=paste(projet,"/Sorties/Vues/Annuelles_relatif-fixe/relatif-fixe",typemesureTitreSortie,Titre,format,sep=""))
    if(is.na(Ymax) == FALSE & is.na(Ymin) == FALSE & Vmm30j == T) ggsave(file=paste(projet,"/Sorties/Vues/Annuelles_relatif-fixe/relatif-fixe-vmm30j",typemesureTitreSortie,Titre,format,sep=""))
    if(is.na(Ymax) == FALSE & is.na(Ymin) == TRUE) warning("cas d'export de la figure plotrelatif avec Ymax fixe et Ymin libre non programmé")
    if(is.na(Ymax) == TRUE & is.na(Ymin) == FALSE) warning("cas d'export de la figure plotrelatif avec Ymax libre et Ymin fixe non programmé")
    }
  if(save==F){return(plotrelatif)}
}

##### Plot temps absolu sur une année ####
## Version grisée avec enveloppe sur fond clair (min/max) ##
# if(length(unique(format(syntjour$chmes_date,"%Y"))) == 1){
  plotabsolu <- ggplot(syntjour, aes(chmes_date))
  if(Contexte$nStations == 1) plotabsolu <- plotabsolu + geom_ribbon(aes(ymin = VMinJ, ymax = VMaxJ), alpha=0.2)
  if(Contexte$nStations != 1) plotabsolu <- plotabsolu + geom_line(aes(y = VMoyJ, colour = chmes_coderhj))
  if(Contexte$nStations != 1) plotabsolu <- plotabsolu + scale_colour_manual(values = PaletteSite)
  if(Vmm30j == T & Contexte$nStations == 1){
    plotabsolu <- plotabsolu + geom_text(data = data.label, aes(x = xtext , y = ytext , label = label ), size = 4, color = "red", fontface="bold")
    plotabsolu <- plotabsolu + geom_segment(data = data.label, aes(x = xdeb, y = ytmm, xend = xfin, yend = ytmm), color = "red", size = 2)
    }
  if(is.na(Ymax) == FALSE & is.na(Ymin) == TRUE) plotabsolu <- plotabsolu + ylim(-1,as.numeric(Ymax))
  if(is.na(Ymax) == FALSE & is.na(Ymin) == FALSE) plotabsolu <- plotabsolu + ylim(as.numeric(Ymin),as.numeric(Ymax))
  if(length(unique(format(syntjour$chmes_date,"%Y"))) < 2) plotabsolu <- plotabsolu + scale_x_date(date_minor_breaks = "1 month", limits = as.Date(c(paste(as.numeric(format(syntjour$chmes_date[1],"%Y"))-1,"-10-01",sep=""),paste(format(syntjour$chmes_date[length(syntjour$chmes_date)],"%Y"),"-09-30",sep=""))))
  if(length(unique(format(syntjour$chmes_date,"%Y"))) >= 2) plotabsolu <- plotabsolu + scale_x_date(date_minor_breaks = "1 month", limits = as.Date(c(paste(format(syntjour$chmes_date[1],"%Y"),"-10-01",sep=""),paste(format(syntjour$chmes_date[length(syntjour$chmes_date)],"%Y"),"-09-30",sep=""))))
  plotabsolu <- plotabsolu + labs(x = "", y = legendeY, title=Titre, color = legendeTitre) # Pour changer le titre
  plotabsolu <- plotabsolu + theme_bw()
# }

if(duree == "Complet"){
  plotabsolu
  if(save==T){
    if(is.na(Ymax) == TRUE & is.na(Ymin) == TRUE & Vmm30j == F) ggsave(file=paste(projet,"/Sorties/Vues/Annuelles_absolu-libre/absolu-libre",typemesureTitreSortie,Titre,format,sep=""))
    if(is.na(Ymax) == TRUE & is.na(Ymin) == TRUE & Vmm30j == T) ggsave(file=paste(projet,"/Sorties/Vues/Annuelles_absolu-libre/absolu-libre-vmm30j",typemesureTitreSortie,Titre,format,sep=""))
    if(is.na(Ymax) == FALSE & is.na(Ymin) == FALSE & Vmm30j == F) ggsave(file=paste(projet,"/Sorties/Vues/Annuelles_absolu-fixe/absolu-fixe",typemesureTitreSortie,Titre,format,sep=""))
    if(is.na(Ymax) == FALSE & is.na(Ymin) == FALSE & Vmm30j == T) ggsave(file=paste(projet,"/Sorties/Vues/Annuelles_absolu-fixe/absolu-fixe-vmm30j",typemesureTitreSortie,Titre,format,sep=""))
    if(is.na(Ymax) == FALSE & is.na(Ymin) == TRUE) warning("cas d'export de la figure plotabsolu avec Ymax fixe et Ymin libre non programmé")
    if(is.na(Ymax) == TRUE & is.na(Ymin) == FALSE) warning("cas d'export de la figure plotabsolu avec Ymax libre et Ymin fixe non programmé")
    }
  if(save==F){return(plotabsolu)}
}

} # Fin de la fonction