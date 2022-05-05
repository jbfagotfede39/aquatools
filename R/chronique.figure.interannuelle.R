#' Comparaison interannuelles de chroniques
#'
#' Cette fonction permet de représenter des chroniques de mesures (température, niveaux, etc.) sous forme de comparaison interannuelle
#' @name chronique.figure.interannuelle
#' @param data Data.frame contenant a minima une colonne chmes_date et une colonne chmes_valeur
#' @param Titre Titre du graphique (vide par défaut)
#' @param typemesure Défini le type de données et modifie les légendes en fonction. Ignoré si le champ chmes_typemesure est présent dans data
#' @param Ymin Valeur minimale de l'axe des Y (-1 par défaut)
#' @param Ymax Valeur maximale de l'axe des Y (aucune par défaut)
#' @param affichagevide Si \code{TRUE} (par défault), fait apparaître les années ne contenant pas de résultats. Si \code{FALSE}, ne fait apparaître que les années contenants des résultats
#' @param style En forme de boxplot (par défaut) ou de violon ou de courbes
#' @param datedebutanneebiol Date de démarrage de l'année biologique : 10-01 (par défaut - 1er octobre), pour l'affichage sous forme de courbes
#' @param save Si \code{FALSE} (par défault), n'enregistre pas les
#'    figures. Si \code{TRUE}, les enregistre.
#' @param projet Nom du projet
#' @param format Défini le format d'enregistrement (par défaut .png)
#' @keywords chronique
#' @import tidyverse
#' @export
#' @examples
#' chronique.figure.interannuelle(data)
#' data %>% chronique.resultats.filtrage() %>% chronique.figure.interannuelle()
#' chronique.figure.interannuelle(data = tableaudonnee, Titre=nom, typemesure = "Barométrie", save=T, format=".png")
#' chronique.figure.interannuelle(dataVOG4, affichagevide = F, style = "courbes")

chronique.figure.interannuelle <- function(
  data = data,
  Titre = "",
  typemesure = c("Thermie", "Thermie barométrique", "Thermie piézométrique", "Barométrie", "Piézométrie", "Piézométrie brute", "Piézométrie compensée", "Piézométrie calée", "Piézométrie NGF", "Oxygénation", "Hydrologie", "Pluviométrie"),
  Ymin = -1,
  Ymax = 30,
  affichagevide = TRUE,
  style = c("boxplot", "violon", "courbes"),
  datedebutanneebiol = "10-01",
  save = F,
  projet = NA_character_,
  format = ".png")
{
  
  ## Évaluation des choix
  typemesure <- match.arg(typemesure)
  style <- match.arg(style)
  
##### -------------- A FAIRE -------------- #####
# Implantation de chronique.contexte()
# Implantation de chronique.figure.parametres()
# Création d'une fonction chronique.palette(), à mutualiser avec chronique.figure.cumul() et chronique.figure.interannuelle()
# Vérifier qu'il y a bien un filtre sur les dix dernières années ?
##### ##### ##### ##### ##### ##### ##### ##### 

  ##### Contexte de la chronique #####
  ## Nouvelle version en cours d'implémentantion ## (avec minuscule : contexte)
  contexte <- chronique.contexte(data)
  
  ## Ancienne version ## (avec majuscule : Contexte)
  # Calcul du nombre de stations ##
  if("chmes_coderhj" %in% colnames(data)) Contexte <- tibble(nstation = n_distinct(data$chmes_coderhj))
  if("chmes_coderhj" %in% colnames(data) == FALSE){
    Contexte <- tibble(nstation = 1)
    data <- data %>% mutate(chmes_coderhj = NA)
  }
  if("chmes_typemesure" %in% colnames(data) == FALSE){
    data <- data %>% mutate(chmes_typemesure = typemesure)
  }

  if(Contexte$nstation == 0) stop("Aucune donnée dans la chronique à analyser")
  if(Contexte$nstation > 1) stop("Différentes stations dans la chronique à analyser - Cas à développer")

  # Calcul du nombre d'années biologiques ##
  if(!("chmes_anneebiol" %in% names(data))) data <- data %>% formatage.annee.biologique(datedebutanneebiol = datedebutanneebiol)
  Contexte$nannee <- n_distinct(data$chmes_anneebiol)
  
  # chmes_typemesure
  if(testit::has_error(data %>%
                       distinct(chmes_typemesure) %>%
                       bind_cols(Contexte)) == TRUE) stop("Plusieurs chmes_typemesure au sein du jeu de données")
  Contexte <-
    data %>%
    distinct(chmes_typemesure) %>%
    bind_cols(Contexte)
  typemesure <- Contexte$chmes_typemesure
  
  # Stations
  if(testit::has_error(data %>%
                       distinct(chmes_coderhj) %>%
                       bind_cols(Contexte)) == TRUE) stop("Plusieurs chmes_coderhj au sein du jeu de données")
  Contexte <-
    data %>%
    distinct(chmes_coderhj) %>%
    bind_cols(Contexte)
  if(nchar(Titre) == 0) Titre <- Contexte$chmes_coderhj
  
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
  if(typemesure == "Piézométrie" | typemesure == "Piézométrie brute" | typemesure == "Piézométrie compensée" | typemesure == "Piézométrie calée"){
    legendeY = "Hauteur d'eau (cm)"
    legendeTitre = "Piézométrie :"
    typemesureTitreSortie = "_piézométrie_"
    if(Ymin == -1) Ymin <- NA
    if(Ymax == 30) Ymax <- NA
  }
  if(typemesure == "Piézométrie NGF"){
    legendeY = "Hauteur d'eau (NGF)"
    legendeTitre = "Piézométrie :"
    typemesureTitreSortie = "_piézométrie_"
    if(Ymin == -1) Ymin <- NA
    if(Ymax == 30) Ymax <- NA
  }
  if(typemesure == "Oxygénation"){
    if(contexte$unite == "mg/L") legendeY = expression(Oxygene~dissous~(mg~O[2]/L))
    if(contexte$unite == "%") legendeY = expression(Oxygene~dissous~("%"))
    legendeTitre = "Oxygénation :"
    typemesureTitreSortie = "_oxygénation_"
    if(Ymin == -1) Ymin <- NA
    if(Ymax == 30) Ymax <- NA
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
  
  if(grepl("Thermie", typemesure)) ecartvisuel <- 0.25
  if(is.na(Ymin) & grepl("Thermie", typemesure)) positionNbJ <- min(data$chmes_valeur) + ecartvisuel
  if(!is.na(Ymin) & grepl("Thermie", typemesure)) positionNbJ <- Ymin + ecartvisuel
  if(grepl("Piézométrie", typemesure)) ecartvisuel <- 0.1*(max(data$chmes_valeur)-min(data$chmes_valeur))
  if(is.na(Ymin) & grepl("Piézométrie", typemesure)) positionNbJ <- max(data$chmes_valeur) + ecartvisuel
  if(!is.na(Ymin) & grepl("Piézométrie", typemesure)) positionNbJ <- Ymin + ecartvisuel
  if(grepl("Oxygénation", typemesure)) ecartvisuel <- 0.1*(max(data$chmes_valeur)-min(data$chmes_valeur))
  if(is.na(Ymin) & grepl("Oxygénation", typemesure)) positionNbJ <- max(data$chmes_valeur) + ecartvisuel
  if(!is.na(Ymin) & grepl("Oxygénation", typemesure)) positionNbJ <- Ymin + ecartvisuel
  
  #### Cas avec boxplot ou violon ####
  if(style %in% c("boxplot", "violon")){
  ### Calcul des valeurs remarquables ###
  dataanalysees <-
    data %>% 
    chronique.traitement(export = F, filtrage = F, log = "Aucun")
  
  ### Affichage des années vides ###
  data <-
    data %>% 
    {if(affichagevide == TRUE) complete(., chmes_coderhj, chmes_typemesure, chmes_anneebiol = seq(min(chmes_anneebiol), max(chmes_anneebiol)))
      else .}

  ##### Collecte des valeurs remarquables #####
  ValeursRemarquables <- dataanalysees %>% dplyr::select(Typemesure, Coderhj, Annee, NbJ, VMinI, VMaxI, VMoyJMinPer, VMoyJMaxPer, VMaxMoy30J)
  
  ValeursRemarquablesMinI <- 
    ValeursRemarquables %>% 
    filter(VMinI == min(VMinI)) %>% 
    mutate(AnneeEt = Annee) %>% 
    mutate(Annee = "Instant.")
  
  ValeursRemarquablesMaxI <- 
    ValeursRemarquables %>% 
    filter(VMaxI == max(VMaxI)) %>% 
    mutate(AnneeEt = Annee) %>% 
    mutate(Annee = "Instant.")
  
  ValeursRemarquablesVMM <- 
    ValeursRemarquables %>% 
    arrange(desc(Annee)) %>% 
    slice(1:10) %>% # pour ne conserver que les dix années les plus récentes
    group_by(Typemesure, Coderhj) %>% 
    summarise(
      VMinVMM = min(VMaxMoy30J),
      VMaxVMM = max(VMaxMoy30J),
      VMoyVMM = mean(VMaxMoy30J),
      NVMM = n(),
      AnneeVMinVMM = Annee[VMaxMoy30J == min(VMaxMoy30J)][1], # le [1] permet d'afficher la première occurence dans le cas d'occurences multiples,
      AnneeVMaxVMM = Annee[VMaxMoy30J == max(VMaxMoy30J)][1] # le [1] permet d'afficher la première occurence dans le cas d'occurences multiples
    ) %>% 
    mutate(Annee = "Vmm30j")

  ### Représentation graphique ###
ggplot <- ggplot(data, aes(as.character(chmes_anneebiol), chmes_valeur))
if(style == "boxplot"){ggplot <- ggplot + geom_boxplot()}
if(style == "violon"){ggplot <- ggplot + geom_violin()}
if(is.na(Ymax) == FALSE & is.na(Ymin) == TRUE) ggplot <- ggplot + ylim(0,as.numeric(Ymax))
if(is.na(Ymax) == FALSE & is.na(Ymin) == FALSE) ggplot <- ggplot + ylim(as.numeric(Ymin),as.numeric(Ymax))
ggplot <- ggplot + labs(x = "Année", y = legendeY, title=Titre, color = legendeTitre) # Pour changer le titre
ggplot <- ggplot + theme_bw()
# Ajout des valeurs journalières annuelles remarquables #
ggplot <- ggplot + geom_point(data = ValeursRemarquables, aes(as.character(Annee), VMoyJMinPer), colour = "#5f90ff")
ggplot <- ggplot + geom_point(data = ValeursRemarquables, aes(as.character(Annee), VMoyJMaxPer), colour = "red")
ggplot <- ggplot + geom_point(data = ValeursRemarquables, aes(as.character(Annee), VMaxMoy30J), colour = "orange")
ggplot <- ggplot + geom_text(data = ValeursRemarquables, aes(as.character(Annee), positionNbJ, label=paste0(NbJ, " j.")), size = 2.5)
# Ajout des valeurs instantannées pluriannuelles remarquables #
ggplot <- ggplot + geom_point(data = ValeursRemarquablesMinI, aes(as.character(Annee), VMinI), colour = "#5f90ff")
ggplot <- ggplot + geom_text(data = ValeursRemarquablesMinI, aes(as.character(Annee), VMinI-2*ecartvisuel, label=AnneeEt), size = 2.5)
ggplot <- ggplot + geom_point(data = ValeursRemarquablesMaxI, aes(as.character(Annee), VMaxI), colour = "red")
ggplot <- ggplot + geom_text(data = ValeursRemarquablesMaxI, aes(as.character(Annee), VMaxI+2*ecartvisuel, label=AnneeEt), size = 2.5)
# Ajout des Vmm30j pluriannuelles remarquables #
ggplot <- ggplot + geom_point(data = ValeursRemarquablesVMM, aes(as.character(Annee), VMinVMM), colour = "#5f90ff")
ggplot <- ggplot + geom_text(data = ValeursRemarquablesVMM, aes(as.character(Annee), VMinVMM-2*ecartvisuel, label=AnneeVMinVMM), size = 2.5)
ggplot <- ggplot + geom_point(data = ValeursRemarquablesVMM, aes(as.character(Annee), VMaxVMM), colour = "red")
if(Contexte$nannee != 1) ggplot <- ggplot + geom_text(data = ValeursRemarquablesVMM, aes(as.character(Annee), VMaxVMM+2*ecartvisuel, label=AnneeVMaxVMM), size = 2.5)
ggplot <- ggplot + geom_point(data = ValeursRemarquablesVMM, aes(as.character(Annee), VMoyVMM), colour = "orange")
if(Contexte$nannee == 1) ggplot <- ggplot + geom_text(data = ValeursRemarquablesVMM, aes(as.character(Annee), positionNbJ, label= paste0(NVMM, " année")), size = 2.5)
if(Contexte$nannee != 1) ggplot <- ggplot + geom_text(data = ValeursRemarquablesVMM, aes(as.character(Annee), positionNbJ, label= paste0(NVMM, " années")), size = 2.5)
}
  
  #### Cas avec courbes annuelles #####
  if(style %in% c("courbes")){
  # Agrégation des données
    DataTravail <- chronique.agregation(data)
    syntjour <- 
      DataTravail %>% 
      purrr::pluck(2) %>% 
      formatage.annee.biologique(datedebutanneebiol = datedebutanneebiol)
    
    # Recalage sur une année arbritraire commune afin de pouvoir comparer les dates ensembles
    year(syntjour$chmes_date[str_sub(syntjour$chmes_date, 6, 10) >= datedebutanneebiol]) <- 2001 # Année arbitraire afin de pouvoir les projeter toutes ensemble
    year(syntjour$chmes_date[str_sub(syntjour$chmes_date, 6, 10) < datedebutanneebiol]) <- 2002 # Année arbitraire afin de pouvoir les projeter toutes ensemble
    
    ## Palette de couleurs ##
    if(Contexte$nannee != 1 | Contexte$nstation != 1){
      # data(PaletteAnnees) # Couleurs trop proches pour années successives
      if(Contexte$nannee * Contexte$nstation <= 11){PaletteCouples <- RColorBrewer::brewer.pal(Contexte$nannee * Contexte$nstation, "Spectral")} #Set3
      if(Contexte$nannee * Contexte$nstation > 11){
        syntjour <- syntjour %>% chronique.cle()
        colourCount <- length(unique(syntjour$Cle))
        getPalette <- colorRampPalette(RColorBrewer::brewer.pal(9, "Spectral")) #Set3
        PaletteCouples <- getPalette(colourCount)
      }
    }
  
ggplot <- ggplot(syntjour, aes(chmes_date))
ggplot <- ggplot + geom_line(aes(y = VMoyJ, colour = as.character(chmes_anneebiol)))
if(is.na(Ymax) == FALSE & is.na(Ymin) == TRUE){
  if(grepl("Thermie", typemesure)) ggplot <- ggplot + ylim(0,as.numeric(Ymax))
  if(grepl("Piézométrie", typemesure)) ggplot <- ggplot + ylim(min(data$chmes_valeur) - 10, as.numeric(Ymax))
  if(grepl("Oxygénation", typemesure)) ggplot <- ggplot + ylim(min(data$chmes_valeur) - 10, as.numeric(Ymax))
  }
if(is.na(Ymax) == FALSE & is.na(Ymin) == FALSE) ggplot <- ggplot + ylim(as.numeric(Ymin),as.numeric(Ymax))
ggplot <- ggplot + scale_x_date(date_labels = "%b")
if(Contexte$nannee != 1 | Contexte$nstation != 1) ggplot <- ggplot + scale_colour_manual(values = PaletteCouples)
ggplot <- ggplot + labs(x = "", y = legendeY, title=Titre, color = legendeTitre) # Pour changer le titre
ggplot <- ggplot + theme_bw()
}

  # Affichage
ggplot

  # Enregistrement
if(save==T){
  if(style=="boxplot")ggsave(file=paste(projet,"/Sorties/Vues/Interannuelles/Interannuelle",typemesureTitreSortie,Titre,format,sep=""))
  if(style=="violon")ggsave(file=paste(projet,"/Sorties/Vues/Interannuelles/Interannuelle_violon",typemesureTitreSortie,Titre,format,sep=""))
  if(style=="courbes")ggsave(file=paste(projet,"/Sorties/Vues/Interannuelles/Interannuelle_courbes",typemesureTitreSortie,Titre,format,sep=""))
  }

if(save==F){return(ggplot)}

} # Fin de la fonction
