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
#' @param save Si \code{FALSE} (par défault), n'enregistre pas les figures. Si \code{TRUE}, les enregistre.
#' @param projet Nom du projet
#' @param etiquette Affichage des étiquettes de chroniques (\code{FALSE} (par défault))
#' @param format Défini le format d'enregistrement (par défaut .png)
#' @keywords chronique
#' @import scales
#' @import tidyverse
#' @export
#' @examples
#' chronique.figure.cumul(data)
#' MesuresTest %>% chronique.figure.cumul()
#' MesuresTest %>% chronique.agregation(datedebutanneebiol = "03-01") %>% purrr::pluck(2) %>% chronique.figure.cumul(datedebutanneebiol = "03-01")

chronique.figure.cumul <- function(
  data = data,
  Titre="",
  typemesure = c("Thermie", "Thermie barométrique", "Thermie piézométrique", "Oxygénation", "Hydrologie", "Pluviométrie"),
  duree = c("Relatif", "Complet"),
  datedebutanneebiol = "10-01",
  legendemax = 15,
  save=F,
  projet = NA_character_,
  etiquette = F,
  format=".png")
{
  
  ##### -------------- A FAIRE -------------- #####
  # Possibilité de terminer le graphique pour une somme de degré jours finale, avec la date la plus tardive pour lensemble des chroniques ? (https://drsimonj.svbtle.com/label-line-ends-in-time-series-with-ggplot2)
  # Valeur de degrés jours libre ou à partir dune table de référence par stade par espèce ?
  # Affiner l'affichage des étiquettes avec https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html
  # Il faudrait améliorer le ttt multi-site pour une année et multi-site pour plusieurs années en ouvrant chronique.agregation à ce cas
  # Création d'une fonction chronique.palette(), à mutualiser avec chronique.figure.cumul() et chronique.figure.interannuelle()
  # -------------- A FAIRE -------------- #
  
  ## Évaluation des choix
  typemesure <- match.arg(typemesure)
  duree <- match.arg(duree)
  
  ##### Mise au format des données #####
  ## Transformation du format des dates
  if(class(data$chmes_date) != "Date"){
    data$chmes_date <- ymd(data$chmes_date)}

  ## Agrégation journalière si nécessaire ##
  # Données issues de la table chroniques_mesuresgroupees
  if(!("chmes_heure" %in% names(data)) & "chmesgr_coderhj_id" %in% names(data)){
    syntjour <-
      data %>% 
      arrange(chmes_date) %>% 
      mutate(VMoyJ = chmesgr_valeur) %>% # on part de données journalières déjà agrégées
      formatage.annee.biologique(datedebutanneebiol = datedebutanneebiol) %>% 
      group_by(chmesgr_coderhj_id, chmes_anneebiol) %>% 
      mutate(SommeMoyJ = cumsum(round(VMoyJ,0))) %>% 
      ungroup()
  }
  
  # Données issues de la fonction chronique.agregation()
  if(!("chmes_heure" %in% names(data)) & "chmes_coderhj" %in% names(data)){
    syntjour <-
      data %>% 
      ungroup() %>% 
      arrange(chmes_date) %>% 
      # mutate(VMoyJ = chmesgr_valeur) %>% # on part de données journalières déjà agrégées
      formatage.annee.biologique(datedebutanneebiol = datedebutanneebiol) %>% 
      group_by(chmes_coderhj, chmes_anneebiol) %>% 
      mutate(SommeMoyJ = cumsum(round(VMoyJ,0))) %>% 
      ungroup()
  }
  
  if("chmes_heure" %in% names(data)) {
  syntjour <- 
    data %>% 
    ungroup() %>% 
    chronique.agregation(instantanne = F, mensuel = F, annuel = F, integral = F)
  }
  
  ## Calcul de l'année biologique ##
  syntjour <- 
    syntjour %>% 
    formatage.annee.biologique(datedebutanneebiol = datedebutanneebiol)
  
  # Recalage sur une année arbitraire commune afin de pouvoir comparer/projeter les dates ensembles
  syntjour <- 
    syntjour %>% 
    formatage.annee.neutre(datedebutanneeneutre = datedebutanneebiol) %>% 
    mutate(chmes_date = chmes_date_anneeneutre) %>% 
    select(-chmes_date_anneeneutre)

  ##### Contexte de la chronique #####
  Contexte <- 
    syntjour %>% 
    chronique.contexte()
  
  #### Paramétrisation ####
  if(Contexte$ntypemesure == 1) typemesure <- as.character(Contexte$typemesure)
  
  if(typemesure == "Hydrologie"){
    stop(glue('Mode de calcul à modifier pour typemesure = {typemesure}'))
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
  
  if(nchar(Titre) == 0){
    if(Contexte$nannee != 1 | Contexte$nstation != 1) Titre <- Contexte$station
    if(Contexte$nannee == 1 & Contexte$nstation == 1) Titre <- glue('{Contexte$station}_{Contexte$annee}')
  }
  
  if(nchar(Titre) != 0){
    if(Contexte$nannee == 1 & Contexte$nstation == 1 & Titre == Contexte$station) Titre <- glue('{Contexte$station}_{Contexte$annee}')
  }

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
    mutate(percentile = ntile(row_number(), Ngroupes)) %>%
    filter(percentile == group_indices()) %>%
    filter(VMedJ == max(VMedJ)) %>%
    filter(VMaxJ == max(VMaxJ))
}
  
  ## Complet ##
  if(duree == "Complet") stop("Fonctionnalité de représentation complète à développer")
  
  ## Relatif ##
  if(duree == "Relatif"){
    plotrelatif <- ggplot(syntjour, aes(chmes_date, y = SommeMoyJ))
    if(Contexte$nannee == 1 & Contexte$nstation == 1) plotrelatif <- plotrelatif + geom_line()
    if(Contexte$nannee != 1 & Contexte$nstation == 1) plotrelatif <- plotrelatif + geom_line(aes(, colour = as.character(chmes_anneebiol)))
    if(Contexte$nannee == 1 & Contexte$nstation != 1) plotrelatif <- plotrelatif + geom_line(aes(y = SommeMoyJ, colour = chmes_coderhj))
    if(Contexte$nannee != 1 & Contexte$nstation != 1 & etiquette == F) plotrelatif <- plotrelatif + geom_line(aes(y = SommeMoyJ, colour = Cle))
    if(Contexte$nannee != 1 & Contexte$nstation != 1 & etiquette == T) plotrelatif <- plotrelatif + geom_line(aes(y = SommeMoyJ, colour = Cle))
    plotrelatif <- plotrelatif + scale_x_date(date_labels = "%b")
    if(Contexte$nannee != 1 | Contexte$nstation != 1) plotrelatif <- plotrelatif + scale_colour_manual(values = PaletteCouples)
    plotrelatif <- plotrelatif + labs(x = "", y = legendeY, title=str_replace(Titre, "_", " - "), color = legendeTitre) # Pour changer le titre
    plotrelatif <- plotrelatif + theme_bw()
    if(Contexte$nannee * Contexte$nstation > legendemax) plotrelatif <- plotrelatif + theme(legend.position = "none") # | etiquette == T
    if(etiquette == T) plotrelatif <- plotrelatif + geom_label_repel(aes(label = Cle, colour = Cle), data = syntjour_sub, force = 5, size=3, show.legend = F, min.segment.length = unit(0.05,"cm"))
    plotrelatif
    if(save==T){
      ggsave(file=paste(projet,"/Sorties/Vues/Interannuelles/Interannuelle_cumul",typemesureTitreSortie,Titre,format,sep=""))
    }
    if(save==F){return(plotrelatif)}
  }
  
}
