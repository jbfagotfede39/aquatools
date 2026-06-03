#' Vue sous forme d'horizon plot
#'
#' Cette fonction permet de représenter les classes de valeurs des chroniques de mesures (température, niveaux, etc.) sous forme d'un graphique d'horizons
#' @name chronique.figure.horizonplot
#' @param data Data.frame issu de chronique.agregation (données journalières)
#' @param titre Titre du graphique (vide par défaut)
#' @param origine_donnees Éventuelle source des données à afficher sur la figure (vide par défaut)
#' @param typemesure Défini le type de données et modifie les légendes en fonction. Ignoré si le champ chmes_typemesure est présent dans data
#' @param stations Dataframe contenant les stations, avec au minimum \code{chsta_coderhj} et \code{chsta_distancesource}
#' @param affichagevide Si \code{TRUE}, fait apparaître les années/stations ne contenant pas de résultats. Si \code{FALSE} (par défault), ne fait apparaître que les années/stations contenants des résultats
#' @param datedebutanneebiol Date de démarrage de l'année biologique : 10-01 (par défaut - 1er octobre), pour l'affichage sous forme de courbes
#' @param datedebutanneeneutre Date de démarrage de l'année neutre : 10-01 (par défaut - 1er octobre), pour l'affichage sous forme d'années neutres
#' @param save Si \code{FALSE} (par défault), n'enregistre pas les figures. Si \code{TRUE}, les enregistre.
#' @param projet Nom du projet
#' @param format Défini le format d'enregistrement (par défaut .png)
#' @keywords chronique
#' @import ggHoriPlot
#' @import glue
#' @import scales
#' @import sf
#' @import stringr
#' @import tidyverse
#' @export
#' @examples
#' mesures_exemple %>% filter(grepl("DRO11-6", chmes_coderhj)) %>% filter(chmes_validation == "Validé") %>% filter(chmes_typemesure == "Thermie") %>% chronique.agregation(instantanne = FALSE, mensuel = FALSE, annuel = FALSE, integral = FALSE, complement = TRUE) %>% formatage.annee.biologique(datedebutanneebiol = "01-01") %>% formatage.annee.neutre() %>% chronique.figure.horizonplot()

chronique.figure.horizonplot <- function(
  data = data,
  titre = "",
  origine_donnees = "",
  typemesure = c("Thermie", "Thermie barométrique", "Thermie piézométrique", "Barométrie", "Piézométrie", "Piézométrie brute", "Piézométrie compensée", "Piézométrie calée", "Piézométrie NGF", "Oxygénation", "Hydrologie", "Pluviométrie"),
  stations = NA_character_,
  affichagevide = FALSE,
  datedebutanneebiol = "10-01",
  datedebutanneeneutre = "10-01",
  save = FALSE,
  projet = NA_character_,
  format = ".png"
  )
{
  
  ## Évaluation des choix
  typemesure <- match.arg(typemesure)
  Sys.setlocale(locale="fr_FR.UTF-8") # Afin d'avoir les abréviations des mois en français sur les figures
  
  ##### Contexte de la chronique #####
  contexte <- chronique.contexte(data)
  
  # Test du nombre de stations ##
  if(contexte$nstation == 0) stop("Aucune donnée dans la chronique à analyser")

  # Test des typemesure
  if(contexte$ntypemesure > 1) stop("Plusieurs chmes_typemesure au sein du jeu de données")
  
  #### Paramètres ####
  ##### Titre #####
  if(nchar(titre) == 0) titre <- contexte$station
  
  ##### Ajustement des paramètres en fonction du typemesure #####
  parametres <- contexte %>% chronique.figure.parametres()

  unite <- parametres$unite
  legendeY <- parametres$legendeY
  legendeTitre <- parametres$legendeTitre
  typemesureTitreSortie <- glue("{parametres$typemesureTitreSortie}classes_")
  classes <- parametres$classes
  palette <- parametres$palette
  
  ori <- 10
  sca <- c(-5, 0, 5, 15, 20, 25, 30, 35)
  
  #### Préparation des données ####
  ##### Affichage des années vides #####
  # Les sites vides sont traités plus bas via les stations
  # if(affichagevide == TRUE){
  #   if(contexte$nannee != 1) warning("Ajout des années vides non développé, seulement les sites")
  # # attention : un complete seul ne suffit pas forcément (à moins de bien le construire), car ensuite la fonction formatage.annee.neutre supprime les lignes vides
  # }
  
  #### Création de la vue ####
  # if(contexte$nstation == 1) ggplot <- ggplot(data_calculees, aes(chmes_date_anneeneutre, group = voyant_valeur, fill = voyant_valeur))
  ggplot <- ggplot(data, aes(chmes_date_anneeneutre, VMoyJ))
  ggplot <- ggplot + geom_horizon(aes(fill = ..Cutpoints..), origin = ori, horizonscale = sca)
  ggplot <- ggplot + scale_fill_hcl(palette = 'RdBu', reverse = T)
  # ggplot <- ggplot + facet_grid(chmes_anneebiol ~ .)
  # if(contexte$nstation == 1 & contexte$nannee != 1) ggplot <- ggplot + facet_wrap(vars(chmes_anneebiol))
  # if(contexte$nstation != 1 & contexte$nannee == 1) ggplot <- ggplot + facet_wrap(vars(chmes_coderhj))
  # if(contexte$nstation != 1 & contexte$nannee != 1) ggplot <- ggplot + facet_wrap(chmes_anneebiol ~ chmes_coderhj)
  if(contexte$nstation == 1 & contexte$nannee != 1) ggplot <- ggplot + facet_grid(chmes_anneebiol ~ .)
  if(contexte$nstation != 1 & contexte$nannee == 1) ggplot <- ggplot + facet_grid(chmes_coderhj ~ .)
  if(contexte$nstation != 1 & contexte$nannee != 1) ggplot <- ggplot + facet_grid(vars(chmes_anneebiol, chmes_coderhj) ~ .)
  # ggplot <- ggplot + theme_few() # library(ggthemes)
  ggplot <- ggplot + theme_minimal()
  ggplot <- ggplot + theme(
    panel.spacing.y = unit(0, "lines"),
    strip.text.y = element_text(size = 7, angle = 0, hjust = 0),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank()
  )
  ggplot <- ggplot + scale_x_date(expand = c(0,0), 
                                  date_breaks = "1 month", 
                                  date_labels = "%b")
  ggplot <- ggplot + xlab('Date')
  ggplot <- ggplot + labs(fill = "Moyennes \n journalières (°C) :")
  if(contexte$nstation == 1 & contexte$nannee != 1) ggplot <- ggplot + labs(subtitle = glue("{contexte$typemesure} : {contexte$station}"))
  if(contexte$nstation != 1 & contexte$nannee == 1) ggplot <- ggplot + labs(subtitle = glue("{contexte$typemesure} : {contexte$annee}"))
  ggplot
  
  ### Enregistrement des vues ###
  if(save == TRUE){
    if(contexte$nstation == 1 & contexte$nannee == 1) ggsave(filename = glue("{projet}/Sorties/Vues/Calendaires/Vue_chronologique_horizons_{str_to_lower(contexte$typemesure)}_{contexte$station}_{contexte$annee}.png")) # S'il y a un unique couple station-année
    if(contexte$nstation == 1 & contexte$nannee != 1) ggsave(filename = glue("{projet}/Sorties/Vues/Calendaires/Vue_chronologique_horizons_{str_to_lower(contexte$typemesure)}_{contexte$station}_pluriannuel.png")) # S'il y a une unique station
    if(contexte$nstation != 1 & contexte$nannee == 1) ggsave(filename = glue("{projet}/Sorties/Vues/Calendaires/Vue_chronologique_horizons_{str_to_lower(contexte$typemesure)}_{contexte$annee}_pluristationnel.png")) # S'il y a une unique année
    if(contexte$nstation != 1 & contexte$nannee != 1) ggsave(filename = glue("{projet}/Sorties/Vues/Calendaires/Vue_chronologique_horizons_{str_to_lower(contexte$typemesure)}_pluriannuel_pluristationnel.png")) # S'il y a plusieurs années et plusieurs stations
  
  }
  
  return(ggplot)
  
} # Fin de la fonction
