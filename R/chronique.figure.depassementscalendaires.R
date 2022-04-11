#' Dépassements de valeurs seuils de chroniques
#'
#' Cette fonction permet de représenter d'éventuels dépassements de valeurs de référence pour des chroniques de mesures (température, niveaux, etc.) sous forme de comparaison interannuelle
#' @name chronique.figure.depassementscalendaires
#' @param data Data.frame issu de chronique.agregation (données journalières)
#' @param Titre Titre du graphique (vide par défaut)
#' @param typemesure Défini le type de données et modifie les légendes en fonction. Ignoré si le champ chmes_typemesure est présent dans data
#' @param seuil_variable Type de variable journalière agrégée à utiliser : \code{VMedJ} (par défaut), \code{VMinJ}, \code{VMoyJ}, \code{VMaxJ}, \code{VAmpliJ}, \code{VAmpliSigneJ}, \code{VarJ}, \code{NMesuresJ} ou \code{SommeMoyJ}
#' @param seuil_valeur Valeur de seuil à tester (valeur de \code{19} par défaut)
#' @param seuil_limite_exces Seuil en-deça duquel les dépassements sont testés par défaut, sinon ils sont testés par excès (valeur de \code{12} par défaut).
#' @param etiquette_superieure Intitulé à afficher en cas de dépassement par excès (\code{Supérieur} par défaut)
#' @param couleur_superieure Couleur à afficher en cas de dépassement par excès (\code{#2B83BA} - bleu par défaut)
#' @param etiquette_inferieure Intitulé à afficher en cas de dépassement par défaut (\code{Inférieur} par défaut)
#' @param couleur_inferieure Couleur à afficher en cas de dépassement par défaut (\code{#FDAE61} - orange par défaut)
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
#' @import glue
#' @import tidyverse
#' @export
#' @examples
#' data %>% chronique.agregation() %>% purrr::pluck(2) %>% chronique.figure.depassementscalendaires()
#' data %>% chronique.agregation(instantanne = F, mensuel = F, annuel = F, integral = F) %>% chronique.figure.depassementscalendaires()
#' MesuresCaleesAgregeesJours %>% chronique.figure.depassementscalendaires(typemesure = "Piézométrie NGF", seuil_valeur = 477.25, etiquette_superieure = "Hautes-eaux", etiquette_inferieure = "Basses-eaux")

chronique.figure.depassementscalendaires <- function(
  data = data,
  Titre = "",
  typemesure = c("Thermie", "Thermie barométrique", "Thermie piézométrique", "Barométrie", "Piézométrie", "Piézométrie brute", "Piézométrie compensée", "Piézométrie calée", "Piézométrie NGF", "Oxygénation", "Hydrologie", "Pluviométrie"),
  seuil_variable = c("VMedJ", "VMinJ", "VMoyJ", "VMaxJ", "VAmpliJ", "VAmpliSigneJ", "VarJ", "NMesuresJ", "SommeMoyJ"),
  seuil_valeur = 19,
  seuil_limite_exces = 12,
  etiquette_superieure = "Supérieur",
  couleur_superieure = "#2B83BA",
  etiquette_inferieure = "Inférieur",
  couleur_inferieure = "#FDAE61",
  datedebutanneebiol = "10-01",
  save = F,
  projet = NA_character_,
  format = ".png"
  )
{
  
  ## Évaluation des choix
  typemesure <- match.arg(typemesure)
  seuil_variable <- match.arg(seuil_variable)
  
  if(is.numeric(seuil_valeur) == FALSE) stop("seuil_valeur doit être une valeur numérique")
  if(is.numeric(seuil_limite_exces) == FALSE) stop("seuil_limite_exces doit être une valeur numérique")
  
  ##### Contexte de la chronique #####
  contexte <- chronique.contexte(data)
  
  # Test du nombre de stations ##
  if(contexte$nstation == 0) stop("Aucune donnée dans la chronique à analyser")
  if(contexte$nstation > 1) stop("Différentes stations dans la chronique à analyser - Cas à développer")

  # Test des typemesure
  if(contexte$ntypemesure > 1) stop("Plusieurs chmes_typemesure au sein du jeu de données")
  
  # Test du nombre de stations
  if(contexte$nstation > 1) stop("Plusieurs chmes_coderhj au sein du jeu de données")

  #### Paramètres ####
  ## Titre
  if(nchar(Titre) == 0) Titre <- contexte$station
  
  ## Ajustement des paramètres en fonction du typemesure ##
  parametres <- contexte %>% mutate(typemesure = "Piézométrie NGF") %>% chronique.figure.parametres()
  
  unite <- parametres$unite
  legendeY <- parametres$legendeY
  legendeTitre <- parametres$legendeTitre
  # typemesureTitreSortie <- parametres$typemesureTitreSortie
  typemesureTitreSortie <- glue("{parametres$typemesureTitreSortie}dépassement_")
  
  #### Préparation des données ####
  data_calculees <-
    data %>% 
    ungroup() %>% 
    {if("chmes_anneebiol" %in% colnames(.) == FALSE) formatage.annee.biologique(.) else .} %>% 
    # formatage.annee.biologique() %>% 
    {if("chmes_date_anneeneutre" %in% colnames(.) == FALSE) formatage.annee.neutre(.) else .} %>% 
    # formatage.annee.neutre() %>%
    select(chmes_coderhj, chmes_anneebiol, chmes_date_anneeneutre, chmes_date, all_of(seuil_variable)) %>% 
    rename(valeur = !!names(.[5])) %>% 
    rowwise() %>% 
    mutate(depassement = ifelse(seuil_valeur >= seuil_limite_exces, valeur > seuil_valeur, NA)) %>%
    mutate(depassement = ifelse(seuil_valeur < seuil_limite_exces, valeur < seuil_valeur, depassement)) %>% 
    ungroup()
  
  #### Légendes ####
  data_calculees <-
    data_calculees %>% 
    mutate(depassement = ifelse(depassement == T, etiquette_superieure, depassement)) %>% 
    mutate(depassement = ifelse(depassement == F, etiquette_inferieure, depassement))
  
  #### Création de la vue ####
  ggplot <- ggplot(data_calculees, aes(chmes_date_anneeneutre, chmes_anneebiol, fill= depassement))
  ggplot <- ggplot + geom_tile(height = .25)
  ggplot <- ggplot + theme_bw()
  ggplot <- ggplot + labs(
    subtitle = Titre,
    caption = glue("valeur seuil = {seuil_valeur} {unite}"),
    x = "Date",
    y = "Année biologique",
    fill = legendeTitre)
  ggplot <- ggplot + scale_x_date(date_labels = "%b", date_breaks = "3 months", minor_breaks = "1 month")
  cols <- structure(c(couleur_superieure, couleur_inferieure), .Names = c(etiquette_superieure, etiquette_inferieure))
  ggplot <- ggplot + scale_fill_manual(values = cols)
  ggplot
    # if(contexte$nannee == 1) ggplot <- ggplot + geom_text(data = ValeursRemarquablesVMM, aes(as.character(Annee), positionNbJ, label= paste0(NVMM, " année")), size = 2.5)
    # if(contexte$nannee != 1) ggplot <- ggplot + geom_text(data = ValeursRemarquablesVMM, aes(as.character(Annee), positionNbJ, label= paste0(NVMM, " années")), size = 2.5)
  
  # Enregistrement
  if(save==T){
    ggsave(file = glue("{projet}/Sorties/Vues/Interannuelles/Interannuelle{typemesureTitreSortie}{Titre}{format}"))
      }
  
  return(ggplot)
  
} # Fin de la fonction
