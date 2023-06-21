#' Créer d'une clé pour les chroniques
#'
#' Cette fonction permet de créer une clé (champ Cle) pour les chroniques (mesures, résultats, etc.)
#' @name chronique.cle
#' @param data Data.frame issu de la fonction chronique.mesures ou chronique.resultats
#' @param anneebiologique Si \code{TRUE} (par défault), s'appuie sur l'année biologique lorsque le paramètre année est inclus à la clé
#' @param formatcle Format de la clé d'identification, avec coderhj (S), annee (A), typemesure (T), nbj (D), Milieu (M), Date-Heure (H), Unité (U) par exemple de la forme coderhj_annee_typemesure (SAT) (par défault), coderhj_annee_typemesure_nbj (SATD), coderhj_annee (SA), coderhj_typemesure (ST), milieu_annee (MA), time_coderhj_typemesure (HST), time_typemesure (HT), time_coderhj (HS), coderhj_typemesure_unite (STU) ou milieu_coderhj_typemesure_unite (MSTU)
#' @keywords chronique
#' @import tidyverse
#' @export
#' @examples
#' Resultats %>% chronique.cle()
#' Resultats %>% chronique.cle(anneebiologique = F, formatcle = "SA")

chronique.cle <- function(
  data = data,
  anneebiologique = TRUE,
  formatcle = c("SAT", "SATD", "SA", "ST", "MA", "HST", "HT", "HS", "STU", "MSTU")
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
    {if("chmesgr_coderhj_id" %in% colnames(.)) mutate(., coderhj = chmesgr_coderhj_id) else .} %>% 
    {if("chsta_coderhj" %in% colnames(.)) mutate(., coderhj = chsta_coderhj) else .} %>% 
    {if("chsvi_coderhj" %in% colnames(.)) mutate(., coderhj = chsvi_coderhj) else .} %>% 
    {if("chres_coderhj" %in% colnames(.)) mutate(., coderhj = chres_coderhj) else .} %>% 
    {if("Coderhj" %in% colnames(.)) mutate(., coderhj = Coderhj) else .} %>% 
    # Type de mesures
    {if("chmes_typemesure" %in% colnames(.)) mutate(., typemesure = chmes_typemesure) else .} %>% 
    {if("chmesgr_typemesure" %in% colnames(.)) mutate(., typemesure = chmesgr_typemesure) else .} %>% 
    {if("chres_typemesure" %in% colnames(.)) mutate(., typemesure = chres_typemesure) else .} %>% 
    {if("chsvi_typemesure" %in% colnames(.)) mutate(., typemesure = chsvi_typemesure) else .} %>% 
    {if("Typemesure" %in% colnames(.)) mutate(., typemesure = Typemesure) else .} %>% 
    # Année
    # {if("chres_anneevmm" %in% colnames(.)) mutate(., annee = chres_anneevmm) else .} %>% # Basée sur l'année de VMM (mais parfois deux même année VMM lors de la présence d'un morceau d'année qui s'arrête en février)
    # {if("AnneeVMM" %in% colnames(.)) mutate(., annee = AnneeVMM) else .} %>%  # Basée sur l'année de VMM (mais parfois deux même année VMM lors de la présence d'un morceau d'année qui s'arrête en février)
    {if("chres_annee" %in% colnames(.)) mutate(., annee = chres_annee) else .} %>% 
    {if("Annee" %in% colnames(.)) mutate(., annee = Annee) else .} %>% 
    {if("chmes_anneebiol" %in% colnames(.)) mutate(., annee = chmes_anneebiol) else .} %>% 
    {if("chres_anneebiol" %in% colnames(.)) mutate(., annee = chres_anneebiol) else .} %>% 
    # Milieu
    {if("chsta_milieu" %in% colnames(.)) mutate(., milieu = chsta_milieu) else .} %>% 
    # Nbj
    {if("chres_nbj" %in% colnames(.)) mutate(., nbj = chres_nbj) else .} %>% 
    {if("NbJ" %in% colnames(.)) mutate(., nbj = NbJ) else .} %>% 
    # Heure
    {if("chmes_date" %in% colnames(.)) mutate(., date = chmes_date) else .} %>% 
    {if("chmes_heure" %in% colnames(.)) mutate(., heure = chmes_heure) else .} %>% 
    {if("chsvi_date" %in% colnames(.)) mutate(., date = chsvi_date) else .} %>% 
    {if("chsvi_heure" %in% colnames(.)) mutate(., heure = chsvi_heure) else .} %>% 
    # Unité 
    {if("chmes_unite" %in% colnames(.)) mutate(., unite = chmes_unite) else .} %>% 
    {if("chsvi_unite" %in% colnames(.)) mutate(., unite = chsvi_unite) else .}

  #### Création des données manquantes ####
  if(grepl("A", formatcle, fixed=TRUE) & ("annee" %in% colnames(datarenomees) == FALSE)) warning("Attention il n'y a pas de champs annee dans les données d'entrée")
  if(grepl("S", formatcle, fixed=TRUE) & ("coderhj" %in% colnames(datarenomees) == FALSE)) warning("Attention il n'y a pas de champs coderhj dans les données d'entrée")
  if(grepl("T", formatcle, fixed=TRUE) & ("typemesure" %in% colnames(datarenomees) == FALSE)) warning("Attention il n'y a pas de champs typemesure dans les données d'entrée")
  if(grepl("M", formatcle, fixed=TRUE) & ("milieu" %in% colnames(datarenomees) == FALSE)) warning("Attention il n'y a pas de champs milieu dans les données d'entrée")
  if(grepl("D", formatcle, fixed=TRUE) & ("nbj" %in% colnames(datarenomees) == FALSE)) warning("Attention il n'y a pas de champs nbj dans les données d'entrée")
  if(grepl("H", formatcle, fixed=TRUE) & ("date" %in% colnames(datarenomees) == FALSE)) warning("Attention il n'y a pas de champs date dans les données d'entrée")
  if(grepl("H", formatcle, fixed=TRUE) & ("heure" %in% colnames(datarenomees) == FALSE)) warning("Attention il n'y a pas de champs heure dans les données d'entrée")
  if(grepl("U", formatcle, fixed=TRUE) & ("unite" %in% colnames(datarenomees) == FALSE)) warning("Attention il n'y a pas de champs unite dans les données d'entrée")
  
  datacompletees <-
    datarenomees %>% 
    {if("annee" %in% colnames(.) == FALSE) mutate(., annee = NA_character_) else .} %>% 
    {if("coderhj" %in% colnames(.) == FALSE) mutate(., coderhj = NA_character_) else .} %>% 
    {if("typemesure" %in% colnames(.) == FALSE) mutate(., typemesure = NA_character_) else .} %>% 
    {if("milieu" %in% colnames(.) == FALSE) mutate(., milieu = NA_character_) else .} %>% 
    {if("nbj" %in% colnames(.) == FALSE) mutate(., nbj = NA_integer_) else .} %>% 
    {if("date" %in% colnames(.) == FALSE) mutate(., date = NA_character_) else .} %>% 
    {if("heure" %in% colnames(.) == FALSE) mutate(., heure = NA_character_) else .} %>% 
    {if("unite" %in% colnames(.) == FALSE) mutate(., unite = NA_character_) else .}
  
  #### Création d'une clé ####
  Vue <-
    datacompletees %>%
    {if(formatcle == "SA") mutate(., Cle = as.character(glue("{coderhj}_{annee}"))) else .} %>% # normalement on doit pouvoir enlever les as.character quand tout sera en R4.0 et dplyr 1.0.0 je pense
    {if(formatcle == "SAT") mutate(., Cle = as.character(glue("{coderhj}_{annee}_{typemesure}"))) else .} %>% # normalement on doit pouvoir enlever les as.character quand tout sera en R4.0 et dplyr 1.0.0 je pense
    {if(formatcle == "SATD") mutate(., Cle = as.character(glue("{coderhj}_{annee}_{typemesure}_{nbj}"))) else .} %>% # normalement on doit pouvoir enlever les as.character quand tout sera en R4.0 et dplyr 1.0.0 je pense
    {if(formatcle == "ST") mutate(., Cle = as.character(glue("{coderhj}_{typemesure}"))) else .} %>% # normalement on doit pouvoir enlever les as.character quand tout sera en R4.0 et dplyr 1.0.0 je pense
    {if(formatcle == "MA") mutate(., Cle = as.character(glue("{milieu}_{annee}"))) else .} %>% # normalement on doit pouvoir enlever les as.character quand tout sera en R4.0 et dplyr 1.0.0 je pense
    {if(formatcle == "HST") mutate(., Cle = as.character(glue("{date}_{heure}_{coderhj}_{typemesure}"))) else .} %>% # normalement on doit pouvoir enlever les as.character quand tout sera en R4.0 et dplyr 1.0.0 je pense
    {if(formatcle == "HS") mutate(., Cle = as.character(glue("{date}_{heure}_{coderhj}"))) else .} %>% # normalement on doit pouvoir enlever les as.character quand tout sera en R4.0 et dplyr 1.0.0 je pense
    {if(formatcle == "HT") mutate(., Cle = as.character(glue("{date}_{heure}_{typemesure}"))) else .} %>% # normalement on doit pouvoir enlever les as.character quand tout sera en R4.0 et dplyr 1.0.0 je pense
    {if(formatcle == "STU") mutate(., Cle = as.character(glue("{coderhj}_{typemesure}_{unite}"))) else .} %>% # normalement on doit pouvoir enlever les as.character quand tout sera en R4.0 et dplyr 1.0.0 je pense
    {if(formatcle == "MSTU") mutate(., Cle = as.character(glue("{milieu}_{coderhj}_{typemesure}_{unite}"))) else .} %>% # normalement on doit pouvoir enlever les as.character quand tout sera en R4.0 et dplyr 1.0.0 je pense
    {if("coderhj" %in% colnames(.)) dplyr::select(., -coderhj) else .}  %>% 
    {if("typemesure" %in% colnames(.)) dplyr::select(., -typemesure) else .}  %>% 
    {if("annee" %in% colnames(.)) dplyr::select(., -annee) else .}  %>% 
    {if("milieu" %in% colnames(.)) dplyr::select(., -milieu) else .}  %>% 
    {if("nbj" %in% colnames(.)) dplyr::select(., -nbj) else .} %>% 
    {if("date" %in% colnames(.)) dplyr::select(., -date) else .} %>% 
    {if("heure" %in% colnames(.)) dplyr::select(., -heure) else .} %>% 
    {if("unite" %in% colnames(.)) dplyr::select(., -unite) else .} %>% 
    dplyr::select(Cle, everything())

  #### Affichage des résultats ####
  return(Vue)
}