#' Classes de valeurs de chroniques sous forme calendaire
#'
#' Cette fonction permet de représenter les classes de valeurs de référence pour des chroniques de mesures (température, niveaux, etc.) sous forme calendaire.
#' @name chronique.figure.classescalendaires
#' @param data Data.frame issu de chronique.agregation (données journalières)
#' @param Titre Titre du graphique (vide par défaut)
#' @param typemesure Défini le type de données et modifie les légendes en fonction. Ignoré si le champ chmes_typemesure est présent dans data
#' @param classe_variable Type de variable journalière agrégée à utiliser : \code{VMedJ} (par défaut), \code{VMinJ}, \code{VMoyJ}, \code{VMaxJ}, \code{VAmpliJ}, \code{VAmpliSigneJ}, \code{VarJ}, \code{NMesuresJ} ou \code{SommeMoyJ}
#' @param stations Dataframe contenant les stations, avec au minimum \code{chsta_coderhj} et \code{chsta_distancesource}
#' @param affichagevide Si \code{TRUE}, fait apparaître les années/stations ne contenant pas de résultats. Si \code{FALSE} (par défault), ne fait apparaître que les années/stations contenants des résultats
#' @param datedebutanneebiol Date de démarrage de l'année biologique : 10-01 (par défaut - 1er octobre), pour l'affichage sous forme de courbes
#' @param datedebutanneeneutre Date de démarrage de l'année neutre : 10-01 (par défaut - 1er octobre), pour l'affichage sous forme d'années neutres
#' @param origine_donnees Éventuelle source des données à afficher sur la figure (vide par défaut)
#' @param save Si \code{FALSE} (par défault), n'enregistre pas les figures. Si \code{TRUE}, les enregistre.
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
#' data %>% chronique.agregation() %>% purrr::pluck(2) %>% chronique.figure.classescalendaires()
#' data %>% chronique.agregation(instantanne = F, mensuel = F, annuel = F, integral = F) %>% chronique.figure.classescalendaires()
#' donnees_journalieres %>% chronique.figure.classescalendaires(classe_variable = "VMinJ")
#' donnees_journalieres %>% chronique.figure.classescalendaires(classe_variable = "VMaxJ", origine_donnees = "PNRHJ")

chronique.figure.classescalendaires <- function(
  data = data,
  Titre = "",
  typemesure = c("Thermie", "Thermie barométrique", "Thermie piézométrique", "Barométrie", "Piézométrie", "Piézométrie brute", "Piézométrie compensée", "Piézométrie calée", "Piézométrie NGF", "Oxygénation", "Hydrologie", "Pluviométrie"),
  classe_variable = c("VMedJ", "VMinJ", "VMoyJ", "VMaxJ", "VAmpliJ", "VAmpliSigneJ", "VarJ", "NMesuresJ", "SommeMoyJ"),
  stations = NA_character_,
  affichagevide = FALSE,
  datedebutanneebiol = "10-01",
  datedebutanneeneutre = "10-01",
  origine_donnees = NA_character_,
  save = F,
  projet = NA_character_,
  format = ".png"
  )
{
  
  ## Évaluation des choix
  typemesure <- match.arg(typemesure)
  classe_variable <- match.arg(classe_variable)
  
  ##### Contexte de la chronique #####
  contexte <- chronique.contexte(data)
  
  # Test du nombre de stations ##
  if(contexte$nstation == 0) stop("Aucune donnée dans la chronique à analyser")

  # Test des typemesure
  if(contexte$ntypemesure > 1) stop("Plusieurs chmes_typemesure au sein du jeu de données")
  
  #### Paramètres ####
  ## Titre
  if(nchar(Titre) == 0) Titre <- contexte$station
  
  ## Ajustement des paramètres en fonction du typemesure ##
  parametres <- contexte %>% chronique.figure.parametres()

  unite <- parametres$unite
  legendeY <- parametres$legendeY
  legendeTitre <- parametres$legendeTitre
  typemesureTitreSortie <- glue("{parametres$typemesureTitreSortie}classes_")
  classes <- parametres$classes
  palette <- parametres$palette
  
  #### Préparation des données ####
  ### Affichage des années vides ###
  # Les sites vides sont traités plus bas via les stations
  if(affichagevide == TRUE){
    if(contexte$nannee != 1) warning("Ajout des années vides non développé, seulement les sites")
  # attention : un complete seul ne suffit pas forcément (à moins de bien le construire), car ensuite la fonction formatage.annee.neutre supprime les lignes vides
  }
  
  ### Établissement des classes ###
  data_calculees <-
    data %>% 
    ungroup() %>% 
    {if("chmes_anneebiol" %in% colnames(.) == FALSE) formatage.annee.biologique(., datedebutanneebiol = datedebutanneebiol) else .} %>% 
    {if("chmes_date_anneeneutre" %in% colnames(.) == FALSE) formatage.annee.neutre(., datedebutanneeneutre = datedebutanneeneutre) else .} %>% 
    mutate(chmes_anneebiol = factor(chmes_anneebiol)) %>% 
    select(chmes_coderhj, chmes_anneebiol, chmes_date_anneeneutre, chmes_date, all_of(classe_variable)) %>% 
    rename(valeur = !!names(.[5])) %>% 
    mutate(voyant_valeur = cut(valeur, breaks = classes, include.lowest = TRUE, right = FALSE)) %>%
    {if(contexte$typemesure == "Thermie") mutate(., voyant_valeur = recode_factor(voyant_valeur, `[-15,18)` = "< 18", `[23,45]` = "> 23")) else .} %>% # Pour les extremums
    {if(contexte$typemesure == "Thermie") mutate(., voyant_valeur = fct_relevel(voyant_valeur, "> 23", after = Inf)) else .} %>% # Pour les extremums    {if(contexte$typemesure == "Thermie") mutate(., voyant_valeur = recode_factor(voyant_valeur, `[-15,18)` = "< 18", `[23,45]` = "> 23")) else .} %>% # Pour les extremums
    {if(contexte$typemesure == "Oxygénation") mutate(., voyant_valeur = recode_factor(voyant_valeur, `[0,20)` = "< 20", `[180,200]` = "> 180")) else .} %>% # Pour les extremums
    {if(contexte$typemesure == "Oxygénation") mutate(., voyant_valeur = fct_relevel(voyant_valeur, "> 180", after = Inf)) else .} # Pour les extremums
  
  ### Adjonction des stations ###
  # dont affichage des stations vides présentes dans la table des stations
  data_calculees <-
    data_calculees %>% 
    {if(!is.null(nrow(stations)) & affichagevide == FALSE) left_join(., stations %>% select(chsta_coderhj, chsta_distancesource) %>% st_drop_geometry(), by = c('chmes_coderhj' = 'chsta_coderhj')) else .} %>% # Pour tri par gradient amont-aval - left-join afin de ne conserver que les stations en présence, donc uniquement celles avec données
    {if(!is.null(nrow(stations)) & affichagevide == TRUE) full_join(., stations %>% select(chsta_coderhj, chsta_distancesource) %>% st_drop_geometry(), by = c('chmes_coderhj' = 'chsta_coderhj')) else .} %>% # Pour tri par gradient amont-aval - full-join afin de conserver toutes les stations, même celles sans données
    {if(!is.null(nrow(stations))) mutate(., chmes_coderhj = fct_reorder(factor(chmes_coderhj), chsta_distancesource)) else .} # Pour tri par gradient amont-aval
  
  #### Création de la vue ####
  if(contexte$nstation == 1) ggplot <- ggplot(data_calculees, aes(chmes_date_anneeneutre, chmes_anneebiol, fill = voyant_valeur))
  if(contexte$nstation != 1) ggplot <- ggplot(data_calculees, aes(chmes_date_anneeneutre, chmes_coderhj, fill = voyant_valeur))
  ggplot <- ggplot + geom_tile(height = .25)
  ggplot <- ggplot + theme_bw()
  ggplot <- ggplot + scale_fill_manual(values = palette)
  ggplot <- ggplot + theme(axis.title.x = element_blank(),
                           axis.title.y = element_blank()
  )
  ggplot <- ggplot + scale_x_date(date_labels = "%b")
  ggplot <- ggplot + labs(fill = glue("{classe_variable} :"))
  if(!is.na(origine_donnees)) ggplot <- ggplot + labs(caption = glue("Source des données : {origine_donnees}"))
  if(contexte$nstation == 1 & contexte$nannee == 1) ggplot <- ggplot + labs(subtitle = glue("{contexte$typemesure} : {contexte$station} - {contexte$annee}")) # S'il y a un unique couple station-année
  if(contexte$nstation == 1 & contexte$nannee != 1) ggplot <- ggplot + labs(subtitle = glue("{contexte$typemesure} : {contexte$station}")) # S'il y a une unique station
  if(contexte$nstation != 1 & contexte$nannee == 1) ggplot <- ggplot + labs(subtitle = glue("{contexte$typemesure} : {contexte$annee}")) # S'il y a une unique année
  ggplot
  
  ### Enregistrement des vues ###
  if(save==T){
    if(contexte$nstation == 1 & contexte$nannee == 1) ggsave(filename = glue("{projet}/Sorties/Vues/Calendaires/Vue_chronologique_classes_{str_to_lower(contexte$typemesure)}_{contexte$station}_{contexte$annee}_{classe_variable}.png")) # S'il y a un unique couple station-année
    if(contexte$nstation == 1 & contexte$nannee != 1) ggsave(filename = glue("{projet}/Sorties/Vues/Calendaires/Vue_chronologique_classes_{str_to_lower(contexte$typemesure)}_{contexte$station}_pluriannuel_{classe_variable}.png")) # S'il y a une unique station
    if(contexte$nstation != 1 & contexte$nannee == 1) ggsave(filename = glue("{projet}/Sorties/Vues/Calendaires/Vue_chronologique_classes_{str_to_lower(contexte$typemesure)}_{contexte$annee}_pluristationnel_{classe_variable}.png")) # S'il y a une unique année
  }
  
  return(ggplot)
  
} # Fin de la fonction
