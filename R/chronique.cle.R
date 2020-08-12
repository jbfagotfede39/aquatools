#' Créer d'une clé pour les chroniques
#'
#' Cette fonction permet de créer une clé (champ Cle) pour les chroniques (mesures, résultats, etc.)
#' @name chronique.cle
#' @param data Data.frame issu de la fonction chronique.mesures ou chronique.resultats
#' @param anneebiologique Si \code{TRUE} (par défault), s'appuie sur l'année biologique lorsque le paramètre année est inclus à la clé
#' @param formatcle Format de la clé d'identification, par exemple de la forme coderhj_annee_typemesure (SAT) (par défault) ou milieu_annee (MA)
#' @keywords chronique
#' @import tidyverse
#' @export
#' @examples
#' Resultats %>% chronique.cle()
#' Resultats %>% chronique.cle(anneebiologique = F, formatcle = "SA")

##### TODO LIST #####
# De manière générale, création de nouvelles combinaisons au fil des besoins
# Ajout de la gestion du paramètre milieu
# Implémentation de anneebiologique = FALSE
#####################

chronique.cle <- function(
  data = data,
  anneebiologique = TRUE,
  formatcle = c("SAT", "SA", "MA")
)
{
  
  #### Évaluation des choix ####
  formatcle <- match.arg(formatcle)
  
  #### Tests ####
  if(anneebiologique == FALSE) stop("Implémentation de anneebiologique = FALSE à réaliser")
  
  #### Homogénéisation des noms de champs ####
  datarenomees <-
    data %>% 
    # Stations
    {if("chmes_coderhj" %in% colnames(.)) mutate(., coderhj = chmes_coderhj) else .} %>% 
    {if("chsta_coderhj" %in% colnames(.)) mutate(., coderhj = chsta_coderhj) else .} %>% 
    {if("chres_coderhj" %in% colnames(.)) mutate(., coderhj = chres_coderhj) else .} %>% 
    {if("Coderhj" %in% colnames(.)) mutate(., coderhj = Coderhj) else .} %>% 
    # Type de mesures
    {if("chmes_typemesure" %in% colnames(.)) mutate(., typemesure = chmes_typemesure) else .} %>% 
    {if("chres_typemesure" %in% colnames(.)) mutate(., typemesure = chres_typemesure) else .} %>% 
    {if("Typemesure" %in% colnames(.)) mutate(., typemesure = Typemesure) else .} %>% 
    # Année
    {if("chmes_anneebiol" %in% colnames(.)) mutate(., annee = chmes_anneebiol) else .} %>% 
    {if("chres_anneevmm" %in% colnames(.)) mutate(., annee = chres_anneevmm) else .} %>% 
    {if("AnneeVMM" %in% colnames(.)) mutate(., annee = AnneeVMM) else .} %>% 
    # Milieu
    {if("chsta_milieu" %in% colnames(.)) mutate(., milieu = chsta_milieu) else .}

  #### Création des données manquantes ####
  if(grepl("S", formatcle, fixed=TRUE) & ("coderhj" %in% colnames(datarenomees) == FALSE)) warning("Attention il n'y a pas de champs coderhj dans les données d'entrée")
  if(grepl("T", formatcle, fixed=TRUE) & ("typemesure" %in% colnames(datarenomees) == FALSE)) warning("Attention il n'y a pas de champs typemesure dans les données d'entrée")
  if(grepl("M", formatcle, fixed=TRUE) & ("milieu" %in% colnames(datarenomees) == FALSE)) warning("Attention il n'y a pas de champs milieu dans les données d'entrée")
  
  datacompletees <-
    datarenomees %>% 
    {if("coderhj" %in% colnames(.) == FALSE) mutate(., coderhj = NA_character_) else .} %>% 
    {if("typemesure" %in% colnames(.) == FALSE) mutate(., typemesure = NA_character_) else .} %>% 
    {if("milieu" %in% colnames(.) == FALSE) mutate(., milieu = NA_character_) else .}
  
  #### Création d'une clé ####
    Vue <-
      datacompletees %>%
      {if(formatcle == "SAT") mutate(., Cle = as.character(glue("{coderhj}_{annee}_{typemesure}"))) else .} %>% # normalement on doit pouvoir enlever les as.character quand tout sera en R4.0 et dplyr 1.0.0 je pense
      {if(formatcle == "SA") mutate(., Cle = as.character(glue("{coderhj}_{annee}"))) else .} %>% # normalement on doit pouvoir enlever les as.character quand tout sera en R4.0 et dplyr 1.0.0 je pense
      {if(formatcle == "MA") mutate(., Cle = as.character(glue("{milieu}_{annee}"))) else .} %>% # normalement on doit pouvoir enlever les as.character quand tout sera en R4.0 et dplyr 1.0.0 je pense
      select(Cle, everything(), -coderhj, -typemesure, -annee, -milieu)

  #### Affichage des résultats ####
  return(Vue)
}