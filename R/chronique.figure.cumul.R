#' Représentation de chroniques cumulées
#'
#' Cette fonction permet de représenter des cumuls de chroniques (température, débits, pluviométrie, etc.)
#' @name chronique.figure.cumul
#' @param data Data.frame de données de mesures brutes, issues de chronique.mesures()
#' @param Titre Titre du graphique (vide par défaut)
#' @param typemesure Défini le type de données et modifie les légendes en fonction. Ignoré si le champ chmes_typemesure est présent dans data
#' @param duree Si \code{Relatif} (par défault), affichage uniquement de la période concernée. Si \code{Complet}, affichage de l'année complète.  
#' @param datedebutanneebiol Date de démarrage de l'année biologique : 10-01 (par défaut - 1er octobre)
#' @param legendemax Nombre d'éléments maximum dans la légende (15 par défaut)
#' @param Ymin Valeur minimale de l'axe des Y (-1 par défaut)
#' @param Ymax Valeur maximale de l'axe des Y (aucune par défaut)
#' @param save Si \code{FALSE} (par défault), n'enregistre pas les figures. Si \code{TRUE}, les enregistre.
#' @param projet Nom du projet
#' @param etiquette Affichage des étiquettes de chroniques (\code{FALSE} (par défault))
#' @param format Défini le format d'enregistrement (par défaut .png)
#' @keywords chronique
#' @import tidyverse
#' @export
#' @examples
#' chronique.figure.cumul(data)
#' MesuresTest %>% chronique.figure.cumul()
#' MesuresTest %>% chronique.agregation(datedebutanneebiol = "03-01") %>% purrr::pluck(2) %>% chronique.figure.cumul(datedebutanneebiol = "03-01")
# Pour du multisite voir rapport SMISA 2018 -> il faut faire évoluer chronique.agregation pour du multisite

chronique.figure.cumul <- function(
  data = data,
  Titre="",
  typemesure = c("Thermie", "Thermie barométrique", "Thermie piézométrique", "Oxygénation", "Hydrologie", "Pluviométrie"),
  duree = c("Relatif", "Complet"),
  datedebutanneebiol = "10-01",
  legendemax = 15,
  Ymin=-10,
  Ymax=NA,
  save=F,
  projet = NA_character_,
  etiquette = F,
  format=".png")
{
  
  ##### -------------- A FAIRE -------------- #####
  # Intégration à chronique.traitement ?
  # Possibilité de terminer le graphique pour une somme de degré jours finale, avec la date la plus tardive pour lensemble des chroniques ? (https://drsimonj.svbtle.com/label-line-ends-in-time-series-with-ggplot2)
  # Valeur de degrés jours libre ou à partir dune table de référence par stade par espèce ?
  # Affiner l'affichage des étiquettes avec https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html
  # Il faudrait améliorer le ttt multi-site pour une année et multi-site pour plusieurs années en ouvrant chronique.agregation à ce cas
  # -------------- A FAIRE -------------- #
  
  ## Évaluation des choix
  typemesure <- match.arg(typemesure)
  duree <- match.arg(duree)
  
  ##### Mise au format des données #####
  ## Transformation du format des dates
  if(class(data$chmes_date) != "Date"){
    data$chmes_date <- ymd(data$chmes_date)}
  
  ## Complément et reformatage des données ##
  syntjour <- 
    data %>% 
    ungroup() %>% 
    chronique.agregation() %>% 
    purrr::pluck(2) %>% 
    formatage.annee.biologique(datedebutanneebiol = datedebutanneebiol)
  
  # Recalage sur une année arbritraire commune afin de pouvoir comparer les dates ensembles
  year(syntjour$chmes_date[str_sub(syntjour$chmes_date, 6, 10) >= datedebutanneebiol]) <- 2001 # Année arbitraire afin de pouvoir les projeter toutes ensemble
  year(syntjour$chmes_date[str_sub(syntjour$chmes_date, 6, 10) < datedebutanneebiol]) <- 2002 # Année arbitraire afin de pouvoir les projeter toutes ensemble

  ##### Contexte de la chronique #####
  Contexte <- 
    syntjour %>% 
    chronique.contexte()
  
  #### Paramétrisation ####
  if(Contexte$ntypemesure == 1) typemesure <- as.character(Contexte$typemesure)
  
  if(typemesure == "Pluviométrie" | typemesure == "Hydrologie"){
    stop("Modes de calcul à modifier")
  }
  
  #### Test ####
  if(Contexte$ntypemesure != 1) stop("Plusieurs chmes_typemesure au sein du jeu de données")
  if(Contexte$nstation == 0 | Contexte$nannee == 0) stop("Aucune donnée dans la chronique à analyser")
  if(Contexte$nstation > 1) warning("Différentes stations dans la chronique à analyser")
  
  #### Paramètres de légende ####
  parametres <- Contexte %>% chronique.figure.parametres(typefigure = "cumul")
  legendeY <- parametres$legendeY
  legendeTitre <- parametres$legendeTitre
  typemesureTitreSortie <- parametres$typemesureTitreSortie
  
  if(nchar(Titre) == 0) Titre <- Contexte$station

#### Représentation graphique ####
  if(Contexte$nannee != 1 | Contexte$nstation != 1){
    syntjour <-
      syntjour %>% 
      chronique.cle(formatcle = "SA")
  }
  
  ## Palette de couleurs ##
  if(Contexte$nannee != 1 | Contexte$nstation != 1){
    # data(PaletteAnnees) # Couleurs trop proches pour années successives
    if(Contexte$nannee * Contexte$nstation <= 11){PaletteCouples <- RColorBrewer::brewer.pal(Contexte$nannee * Contexte$nstation, "Spectral")} #Set3
    if(Contexte$nannee * Contexte$nstation > 11){
      colourCount <- length(unique(syntjour$Cle))
      getPalette <- colorRampPalette(RColorBrewer::brewer.pal(9, "Spectral")) #Set3
      PaletteCouples <- getPalette(colourCount)
    }
  }
  
  ## Étiquettes ##
if (etiquette == T) {
  
  ## Afin d'isoler une seule étiquette par Cle + afin d'essayer de répartir longitudinalement les étiquettes
  Ngroupes <- length(unique(syntjour$Cle))

  syntjour_sub <-
    syntjour %>%
    group_by(Cle) %>%
    mutate(percile = ntile(row_number(), Ngroupes)) %>%
    filter(percile == group_indices()) %>%
    filter(VMedJ == max(VMedJ)) %>%
    filter(VMaxJ == max(VMaxJ))
}
  
  ## Complet ##
  if(duree == "Complet") stop("Fonctionnalité de représentation complète à développer")
  
  ## Relatif ##
  if(duree == "Relatif"){
    plotrelatif <- ggplot(syntjour, aes(chmes_date, y = SommeMoyJ))
    if(Contexte$nannee != 1 & Contexte$nstation == 1) plotrelatif <- plotrelatif + geom_line(aes(, colour = as.character(chmes_anneebiol)))
    if(Contexte$nannee == 1 & Contexte$nstation != 1) plotrelatif <- plotrelatif + geom_line(aes(y = SommeMoyJ, colour = chmes_coderhj))
    if(Contexte$nannee != 1 & Contexte$nstation != 1 & etiquette == F) plotrelatif <- plotrelatif + geom_line(aes(y = SommeMoyJ, colour = Cle))
    if(Contexte$nannee != 1 & Contexte$nstation != 1 & etiquette == T) plotrelatif <- plotrelatif + geom_line(aes(y = SommeMoyJ, colour = Cle))
    plotrelatif <- plotrelatif + scale_x_date(date_labels = "%b")
    if(is.na(Ymax) == FALSE & is.na(Ymin) == TRUE) plotrelatif <- plotrelatif + ylim(0,as.numeric(Ymax))
    if(is.na(Ymax) == FALSE & is.na(Ymin) == FALSE) plotrelatif <- plotrelatif + ylim(as.numeric(Ymin),as.numeric(Ymax))
    if(Contexte$nannee != 1 | Contexte$nstation != 1) plotrelatif <- plotrelatif + scale_colour_manual(values = PaletteCouples)
    plotrelatif <- plotrelatif + labs(x = "", y = legendeY, title=Titre, color = legendeTitre) # Pour changer le titre
    plotrelatif <- plotrelatif + theme_bw()
    if(Contexte$nannee * Contexte$nstation > legendemax) plotrelatif <- plotrelatif + theme(legend.position = "none") # | etiquette == T
    if(etiquette == T) plotrelatif <- plotrelatif + geom_label_repel(aes(label = Cle, colour = Cle), data = syntjour_sub, force = 5, size=3, show.legend = F, min.segment.length = unit(0.05,"cm"))
    plotrelatif
    if(save==T){
      if(is.na(Ymax) == TRUE & is.na(Ymin) == TRUE & Vmm30j == F) ggsave(file=paste(projet,"/Sorties/Vues/Interannuelles/Interannuelle_cumul",typemesureTitreSortie,Titre,format,sep=""))
    }
    if(save==F){return(plotrelatif)}
  }
  
}