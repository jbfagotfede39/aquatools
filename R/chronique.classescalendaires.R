#' Calcul de classes de valeurs de chroniques
#'
#' Cette fonction permet de calculer les classes de valeurs de référence pour des chroniques de mesures (température, niveaux, etc.).
#' @name chronique.classescalendaires
#' @param data Data.frame issu de chronique.agregation (données journalières)
#' @param typemesure Défini le type de données et modifie les légendes en fonction. Ignoré si le champ chmes_typemesure est présent dans data
#' @param classe_variable Type de variable journalière agrégée à utiliser : \code{VMedJ} (par défaut), \code{VMinJ}, \code{VMoyJ}, \code{VMaxJ}, \code{VAmpliJ}, \code{VAmpliSigneJ}, \code{VarJ}, \code{NMesuresJ} ou \code{SommeMoyJ}
#' @param datedebutanneebiol Date de démarrage de l'année biologique : 10-01 (par défaut - 1er octobre), pour l'affichage sous forme de courbes
#' @param datedebutanneeneutre Date de démarrage de l'année neutre : 10-01 (par défaut - 1er octobre), pour l'affichage sous forme d'années neutres
#' @keywords chronique
#' @import tidyverse
#' @export
#' @examples
#' data %>% chronique.agregation() %>% purrr::pluck(2) %>% chronique.classescalendaires()
#' data %>% chronique.agregation(instantanne = F, mensuel = F, annuel = F, integral = F) %>% chronique.classescalendaires()
#' donnees_journalieres %>% chronique.classescalendaires(classe_variable = "VMinJ")
#' donnees_journalieres %>% chronique.classescalendaires(classe_variable = "VMaxJ")

chronique.classescalendaires <- function(
  data = data,
  typemesure = c("Thermie", "Thermie barométrique", "Thermie piézométrique", "Barométrie", "Piézométrie", "Piézométrie brute", "Piézométrie compensée", "Piézométrie calée", "Piézométrie NGF", "Oxygénation", "Hydrologie", "Pluviométrie"),
  classe_variable = c("VMedJ", "VMinJ", "VMoyJ", "VMaxJ", "VAmpliJ", "VAmpliSigneJ", "VarJ", "NMesuresJ", "SommeMoyJ"),
  datedebutanneebiol = "10-01",
  datedebutanneeneutre = "10-01"
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
  ##### Ajustement des paramètres en fonction du typemesure #####
  parametres <- contexte %>% chronique.figure.parametres()
  classes <- parametres$classes
  
  #### Calcul ####
  data_calculees <-
    data %>% 
    ungroup() %>% 
    {if("chmes_anneebiol" %in% colnames(.) == FALSE) formatage.annee.biologique(., datedebutanneebiol = datedebutanneebiol) else .} %>% 
    {if("chmes_date_anneeneutre" %in% colnames(.) == FALSE) formatage.annee.neutre(., datedebutanneeneutre = datedebutanneeneutre) else .} %>% 
    mutate(chmes_anneebiol = factor(chmes_anneebiol)) %>% 
    select(chmes_coderhj, chmes_anneebiol, chmes_date_anneeneutre, chmes_date, all_of(classe_variable)) %>% 
    rename(valeur = !!names(.[5])) %>% 
    mutate(classe = cut(valeur, breaks = classes, include.lowest = TRUE, right = FALSE)) %>%
    {if(contexte$typemesure == "Thermie") mutate(., classe = recode_factor(classe, `[-15,18)` = "< 18", `[23,45]` = "> 23")) else .} %>% # Pour les extremums
    {if(contexte$typemesure == "Thermie") mutate(., classe = fct_relevel(classe, "> 23", after = Inf)) else .} %>% # Pour les extremums    {if(contexte$typemesure == "Thermie") mutate(., classe = recode_factor(classe, `[-15,18)` = "< 18", `[23,45]` = "> 23")) else .} %>% # Pour les extremums
    {if(contexte$typemesure == "Oxygénation") mutate(., classe = recode_factor(classe, `[0,20)` = "< 20", `[180,200]` = "> 180")) else .} %>% # Pour les extremums
    {if(contexte$typemesure == "Oxygénation") mutate(., classe = fct_relevel(classe, "> 180", after = Inf)) else .} # Pour les extremums
  
  #### Sortie ####
  return(data_calculees)
  
} # Fin de la fonction
