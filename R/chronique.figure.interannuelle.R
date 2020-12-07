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
#' @param style En forme de boxplot (par défaut) ou de violon
#' @param save Si \code{FALSE} (par défault), n'enregistre pas les
#'    figures. Si \code{TRUE}, les enregistre.
#' @param projet Nom du projet
#' @param format Défini le format d'enregistrement (par défaut .png)
#' @keywords chronique
#' @import ggplot2 
#' @import dplyr
#' @export
#' @examples
#' chronique.figure.interannuelle(data)
#' data %>% chronique.resultats.filtrage() %>% chronique.figure.interannuelle()
#' chronique.figure.interannuelle(data = tableaudonnee, Titre=nom, typemesure = "Barométrie", save=T, format=".png")

chronique.figure.interannuelle <- function(
  data = data,
  Titre = "",
  typemesure = c("Thermie", "Thermie barométrique", "Thermie piézométrique", "Barométrie", "Piézométrie", "Piézométrie brute", "Piézométrie compensée", "Piézométrie calée", "Piézométrie NGF", "Oxygénation", "Hydrologie", "Pluviométrie"),
  Ymin = -1,
  Ymax = 30,
  affichagevide = TRUE,
  style = c("boxplot", "violon"),
  save = F,
  projet = NA_character_,
  format = ".png")
{
  
  ## Évaluation des choix
  typemesure <- match.arg(typemesure)
  style <- match.arg(style)
  
##### -------------- A FAIRE -------------- #####
# Remplacer l'appel de data_frame() par tibble()
# Implantation de chronique.contexte()
# Implantation de chronique.figure.parametres()
# Vérifier qu'il y a bien un filtre sur les dix dernières années ?
##### ##### ##### ##### ##### ##### ##### ##### 

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
  if(Contexte$nStations > 1) stop("Différentes stations dans la chronique à analyser - Cas à développer")

  # Calcul du nombre d'années biologiques ##
  Contexte$nAnneebiol <- n_distinct(data$chmes_anneebiol)
  
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
  
  #### Calcul des valeurs remarquables ####
  dataanalysees <-
    data %>% 
    chronique.traitement(export = F, filtrage = F)
  
  #### Affichage des années vides ####
  data <-
    data %>% 
    {if(affichagevide == TRUE) complete(., chmes_coderhj, chmes_typemesure, chmes_anneebiol = seq(min(chmes_anneebiol), max(chmes_anneebiol)))
      else .}

  ##### Collecte des valeurs remarquables #####
  ValeursRemarquables <- dataanalysees %>% select(Typemesure, Coderhj, Annee, NbJ, VMinI, VMaxI, VMoyJMinPer, VMoyJMaxPer, VMaxMoy30J)
  
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

##### Boxplot interannuel #####
if(is.na(Ymin)) positionNbJ <- min(data$chmes_valeur)+0.25
if(!is.na(Ymin)) positionNbJ <- Ymin+0.25

ggboxplot <- ggplot(data, aes(as.character(chmes_anneebiol), chmes_valeur))
if(style == "boxplot"){ggboxplot <- ggboxplot + geom_boxplot()}
if(style == "violon"){ggboxplot <- ggboxplot + geom_violin()}
if(is.na(Ymax) == FALSE & is.na(Ymin) == TRUE) ggboxplot <- ggboxplot + ylim(0,as.numeric(Ymax))
if(is.na(Ymax) == FALSE & is.na(Ymin) == FALSE) ggboxplot <- ggboxplot + ylim(as.numeric(Ymin),as.numeric(Ymax))
ggboxplot <- ggboxplot + labs(x = "Année", y = legendeY, title=Titre, color = legendeTitre) # Pour changer le titre
ggboxplot <- ggboxplot + theme_bw()
# Ajout des valeurs journalières annuelles remarquables #
ggboxplot <- ggboxplot + geom_point(data = ValeursRemarquables, aes(as.character(Annee), VMoyJMinPer), colour = "#5f90ff")
ggboxplot <- ggboxplot + geom_point(data = ValeursRemarquables, aes(as.character(Annee), VMoyJMaxPer), colour = "red")
ggboxplot <- ggboxplot + geom_point(data = ValeursRemarquables, aes(as.character(Annee), VMaxMoy30J), colour = "orange")
ggboxplot <- ggboxplot + geom_text(data = ValeursRemarquables, aes(as.character(Annee), positionNbJ, label=paste0(NbJ, " j.")), size = 2.5)
# Ajout des valeurs instantannées pluriannuelles remarquables #
ggboxplot <- ggboxplot + geom_point(data = ValeursRemarquablesMinI, aes(as.character(Annee), VMinI), colour = "#5f90ff")
ggboxplot <- ggboxplot + geom_text(data = ValeursRemarquablesMinI, aes(as.character(Annee), VMinI-0.5, label=AnneeEt), size = 2.5)
ggboxplot <- ggboxplot + geom_point(data = ValeursRemarquablesMaxI, aes(as.character(Annee), VMaxI), colour = "red")
ggboxplot <- ggboxplot + geom_text(data = ValeursRemarquablesMaxI, aes(as.character(Annee), VMaxI+0.5, label=AnneeEt), size = 2.5)
# Ajout des Vmm30j pluriannuelles remarquables #
ggboxplot <- ggboxplot + geom_point(data = ValeursRemarquablesVMM, aes(as.character(Annee), VMinVMM), colour = "#5f90ff")
ggboxplot <- ggboxplot + geom_text(data = ValeursRemarquablesVMM, aes(as.character(Annee), VMinVMM-0.5, label=AnneeVMinVMM), size = 2.5)
ggboxplot <- ggboxplot + geom_point(data = ValeursRemarquablesVMM, aes(as.character(Annee), VMaxVMM), colour = "red")
if(Contexte$nAnneebiol != 1) ggboxplot <- ggboxplot + geom_text(data = ValeursRemarquablesVMM, aes(as.character(Annee), VMaxVMM+0.5, label=AnneeVMaxVMM), size = 2.5)
ggboxplot <- ggboxplot + geom_point(data = ValeursRemarquablesVMM, aes(as.character(Annee), VMoyVMM), colour = "orange")
if(Contexte$nAnneebiol == 1) ggboxplot <- ggboxplot + geom_text(data = ValeursRemarquablesVMM, aes(as.character(Annee), positionNbJ, label= paste0(NVMM, " année")), size = 2.5)
if(Contexte$nAnneebiol != 1) ggboxplot <- ggboxplot + geom_text(data = ValeursRemarquablesVMM, aes(as.character(Annee), positionNbJ, label= paste0(NVMM, " années")), size = 2.5)
ggboxplot

if(save==T){
  if(style=="boxplot")ggsave(file=paste(projet,"/Sorties/Vues/Interannuelles/Interannuelle",typemesureTitreSortie,Titre,format,sep=""))
  if(style=="violon")ggsave(file=paste(projet,"/Sorties/Vues/Interannuelles/Interannuelle_violon",typemesureTitreSortie,Titre,format,sep=""))
  
}
if(save==F){return(ggboxplot)}
} # Fin de la fonction
