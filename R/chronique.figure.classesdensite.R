#' Classes de valeurs de chroniques sous forme de densités
#'
#' Cette fonction permet de représenter les classes de valeurs de référence pour des chroniques de mesures (température, niveaux, etc.) sous forme de densités
#' @name chronique.figure.classesdensite
#' @param data Data.frame issu de chronique.mesures (données fines)
#' @param titre Titre du graphique (vide par défaut)
#' @param origine_donnees Éventuelle source des données à afficher sur la figure (vide par défaut)
#' @param typemesure Défini le type de données et modifie les légendes en fonction. Ignoré si le champ chmes_typemesure est présent dans data
#' @param stations Dataframe contenant les stations, avec au minimum \code{chsta_coderhj} et \code{chsta_distancesource}
#' @param affichagevide Si \code{TRUE}, fait apparaître les années/stations ne contenant pas de résultats. Si \code{FALSE} (par défault), ne fait apparaître que les années/stations contenants des résultats
#' @param datedebutanneebiol Date de démarrage de l'année biologique : 10-01 (par défaut - 1er octobre), pour l'affichage sous forme de courbes
#' @param datedebutanneeneutre Date de démarrage de l'année neutre : 10-01 (par défaut - 1er octobre), pour l'affichage sous forme d'années neutres
#' @param save Si \code{FALSE} (par défault), n'enregistre pas les figures. Si \code{TRUE}, les enregistre.
#' @param estival Si \code{FALSE} (par défault), n'enregistre pas les figures avec le terme \code{_estival} dans le nom de fichier. Si \code{TRUE}, l'ajoute
#' @param projet Nom du projet
#' @param format Défini le format d'enregistrement (par défaut .png)
#' @keywords chronique
#' @import glue
#' @import scales
#' @import sf
#' @import stringr
#' @import tidyverse
#' @export
#' @examples
#' mesures_exemple %>% filter(chmes_coderhj == "HER14-8") %>% filter(chmes_validation == "Validé") %>% filter(chmes_typemesure == "Thermie") %>% chronique.figure.classesdensite()
#' mesures_exemple %>% filter(chmes_coderhj == "HER14-8") %>% filter(chmes_validation == "Validé") %>% filter(chmes_typemesure == "Thermie") %>% formatage.annee.biologique() %>% filter(chmes_anneebiol == 2021) %>% mutate(mois = format(ymd(chmes_date), "%m")) %>% filter(mois %in% c("06", "07", "08", "09")) %>% chronique.figure.classesdensite()

chronique.figure.classesdensite <- function(
  data = data,
  titre = "",
  origine_donnees = "",
  typemesure = c("Thermie", "Thermie barométrique", "Thermie piézométrique", "Barométrie", "Piézométrie", "Piézométrie brute", "Piézométrie compensée", "Piézométrie calée", "Piézométrie NGF", "Oxygénation", "Hydrologie", "Pluviométrie"),
  stations = NA_character_,
  affichagevide = FALSE,
  datedebutanneebiol = "10-01",
  datedebutanneeneutre = "10-01",
  save = FALSE,
  estival = FALSE,
  projet = NA_character_,
  format = ".png"
  )
{
  
  ## Évaluation des choix
  typemesure <- match.arg(typemesure)
  
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
  
  #### Préparation des données ####
  ##### Affichage des années vides #####
  # Les sites vides sont traités plus bas via les stations
  if(affichagevide == TRUE){
    if(contexte$nannee != 1) warning("Ajout des années vides non développé, seulement les sites")
  # attention : un complete seul ne suffit pas forcément (à moins de bien le construire), car ensuite la fonction formatage.annee.neutre supprime les lignes vides
  }
  
  ##### Établissement des classes #####
  data_calculees <-
    data %>% 
    ungroup() %>% 
    {if("chmes_anneebiol" %in% colnames(.) == FALSE) formatage.annee.biologique(., datedebutanneebiol = datedebutanneebiol) else .} %>% 
    {if("chmes_date_anneeneutre" %in% colnames(.) == FALSE) formatage.annee.neutre(., datedebutanneeneutre = datedebutanneeneutre) else .} %>% 
    mutate(chmes_anneebiol = factor(chmes_anneebiol)) %>% 
    select(chmes_coderhj, chmes_anneebiol, chmes_date_anneeneutre, chmes_date, chmes_valeur) %>% 
    mutate(voyant_valeur = cut(chmes_valeur, breaks = classes, include.lowest = TRUE, right = FALSE)) %>%
    {if(contexte$typemesure == "Thermie") mutate(., voyant_valeur = recode_factor(voyant_valeur, `[-15,18)` = "< 18", `[23,45]` = "> 23")) else .} %>% # Pour les extremums
    {if(contexte$typemesure == "Thermie") mutate(., voyant_valeur = fct_relevel(voyant_valeur, "> 23", after = Inf)) else .} %>% # Pour les extremums    {if(contexte$typemesure == "Thermie") mutate(., voyant_valeur = recode_factor(voyant_valeur, `[-15,18)` = "< 18", `[23,45]` = "> 23")) else .} %>% # Pour les extremums
    {if(contexte$typemesure == "Oxygénation") mutate(., voyant_valeur = recode_factor(voyant_valeur, `[0,20)` = "< 20", `[180,200]` = "> 180")) else .} %>% # Pour les extremums
    {if(contexte$typemesure == "Oxygénation") mutate(., voyant_valeur = fct_relevel(voyant_valeur, "> 180", after = Inf)) else .} # Pour les extremums
  
  ##### Adjonction des stations #####
  # dont affichage des stations vides présentes dans la table des stations
  data_calculees <-
    data_calculees %>% 
    {if(!is.null(nrow(stations)) & affichagevide == FALSE) left_join(., stations %>% select(chsta_coderhj, chsta_distancesource) %>% st_drop_geometry(), by = c('chmes_coderhj' = 'chsta_coderhj')) else .} %>% # Pour tri par gradient amont-aval - left-join afin de ne conserver que les stations en présence, donc uniquement celles avec données
    {if(!is.null(nrow(stations)) & affichagevide == TRUE) full_join(., stations %>% select(chsta_coderhj, chsta_distancesource) %>% st_drop_geometry(), by = c('chmes_coderhj' = 'chsta_coderhj')) else .} %>% # Pour tri par gradient amont-aval - full-join afin de conserver toutes les stations, même celles sans données
    {if(!is.null(nrow(stations))) mutate(., chmes_coderhj = fct_reorder(factor(chmes_coderhj), chsta_distancesource)) else .} # Pour tri par gradient amont-aval
  
  #### Création de la vue ####
  # if(contexte$nstation == 1) ggplot <- ggplot(data_calculees, aes(chmes_date_anneeneutre, group = voyant_valeur, fill = voyant_valeur))
  ggplot <- ggplot(data_calculees, aes(chmes_date_anneeneutre, group = voyant_valeur, fill = voyant_valeur))
  ggplot <- ggplot + geom_density(position = "fill", color = NA)
  ggplot <- ggplot + scale_y_continuous(labels = scales::percent_format())
  ggplot <- ggplot + theme_minimal()
  ggplot <- ggplot + scale_fill_manual(values = palette)
  ggplot <- ggplot + theme(axis.title.x = element_blank(),
                           axis.title.y = element_blank()
  )
  if(contexte$nmois >= 6) ggplot <- ggplot + scale_x_date(date_breaks = "3 month", date_minor_breaks = "1 month", date_labels = "%b")
  if(contexte$nmois < 6) ggplot <- ggplot + scale_x_date(date_breaks = "1 month", date_labels = "%b")
  ggplot <- ggplot + labs(fill = "Valeurs \n instantanées :")
  if(nchar(origine_donnees) != 0) ggplot <- ggplot + labs(caption = glue("Source des données : {origine_donnees}"))
  if(contexte$nstation == 1 & contexte$nannee != 1) ggplot <- ggplot + facet_wrap(vars(chmes_anneebiol))
  if(contexte$nstation != 1 & contexte$nannee == 1) ggplot <- ggplot + facet_wrap(vars(chmes_coderhj))
  if(contexte$nstation != 1 & contexte$nannee != 1) ggplot <- ggplot + facet_wrap(chmes_anneebiol ~ chmes_coderhj)
  ggplot <- ggplot + labs(subtitle = glue("{contexte$typemesure} : {contexte$station} - {contexte$annee}"))
  ggplot
  
  ### Enregistrement des vues ###
  if(save == TRUE){
    if(contexte$nstation == 1 & contexte$nannee == 1 & estival == FALSE) ggsave(filename = glue("{projet}/Sorties/Vues/Calendaires/Vue_chronologique_densite_classes_{str_to_lower(contexte$typemesure)}_{contexte$station}_{contexte$annee}.png")) # S'il y a un unique couple station-année
    if(contexte$nstation == 1 & contexte$nannee != 1 & estival == FALSE) ggsave(filename = glue("{projet}/Sorties/Vues/Calendaires/Vue_chronologique_densite_classes_{str_to_lower(contexte$typemesure)}_{contexte$station}_pluriannuel.png")) # S'il y a une unique station
    if(contexte$nstation != 1 & contexte$nannee == 1 & estival == FALSE) ggsave(filename = glue("{projet}/Sorties/Vues/Calendaires/Vue_chronologique_densite_classes_{str_to_lower(contexte$typemesure)}_{contexte$annee}_pluristationnel.png")) # S'il y a une unique année
    if(contexte$nstation != 1 & contexte$nannee != 1 & estival == FALSE) ggsave(filename = glue("{projet}/Sorties/Vues/Calendaires/Vue_chronologique_densite_classes_{str_to_lower(contexte$typemesure)}_pluriannuel_pluristationnel.png")) # S'il y a plusieurs années et plusieurs stations
  
    if(contexte$nstation == 1 & contexte$nannee == 1 & estival == TRUE) ggsave(filename = glue("{projet}/Sorties/Vues/Calendaires/Vue_chronologique_densite_classes_estival_{str_to_lower(contexte$typemesure)}_{contexte$station}_{contexte$annee}.png")) # S'il y a un unique couple station-année
    if(contexte$nstation == 1 & contexte$nannee != 1 & estival == TRUE) ggsave(filename = glue("{projet}/Sorties/Vues/Calendaires/Vue_chronologique_densite_classes_estival_{str_to_lower(contexte$typemesure)}_{contexte$station}_pluriannuel.png")) # S'il y a une unique station
    if(contexte$nstation != 1 & contexte$nannee == 1 & estival == TRUE) ggsave(filename = glue("{projet}/Sorties/Vues/Calendaires/Vue_chronologique_densite_classes_estival_{str_to_lower(contexte$typemesure)}_{contexte$annee}_pluristationnel.png")) # S'il y a une unique année
    if(contexte$nstation != 1 & contexte$nannee != 1 & estival == TRUE) ggsave(filename = glue("{projet}/Sorties/Vues/Calendaires/Vue_chronologique_densite_classes_estival_{str_to_lower(contexte$typemesure)}_pluriannuel_pluristationnel.png")) # S'il y a plusieurs années et plusieurs stations
  }
  
  return(ggplot)
  
} # Fin de la fonction
