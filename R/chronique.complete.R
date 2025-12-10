#' Complément de chroniques
#'
#' Cette fonction permet de compléter les chroniques présentant des valeurs manquantes (une station avec un unique paramètre)
#' @name chronique.complete
#' @param data Data.frame d'une série de mesures de chroniques au format \code(mesures_structure)
#' @keywords chronique
#' @import lubridate
#' @import tidyverse
#' @export
#' @examples
#' chronique.complete(data)
#' data %>% group_split(chmes_coderhj) %>% map(~ chronique.complete(.)) %>% list_rbind() 

chronique.complete <- function(
    data = data)
{
  #### Contexte ####
  contexte <- data %>% chronique.contexte()
  
  #### Test de cohérence ####
  if(!("chmes_date" %in% names(data))) stop("Pas de colonne 'chmes_date' dans le jeu de données fourni")
  if(!("chmes_heure" %in% names(data))) stop("Pas de colonne 'chmes_heure' dans le jeu de données fourni")
  if(!("chmes_valeur" %in% names(data))) stop("Pas de colonne 'chmes_valeur' dans le jeu de données fourni")
  if(contexte$nstation != 1) stop("Plusieurs stations dans le jeu de données fourni : il faut passer par un map")
  if(contexte$ntypemesure != 1) stop("Plusieurs typemesure dans le jeu de données fourni : il faut passer par un map")
  if(contexte$nunite != 1) stop("Plusieurs unite dans le jeu de données fourni : il faut passer par un map")
  
  #### Calcul ####
  ##### Calcul d'un couple date-heure ####
  data_v2 <-
    data %>% 
    formatage.time()
  
  ##### Calcul des début et fin de la série ####
  debut <- min(data_v2$time)# %>% floor_date("hour")
  fin <- max(data_v2$time)# %>% ceiling_date("hour")
  
  ##### Création de la série complète ####
  data_v3 <- 
    data_v2 %>% 
    complete(time = seq(debut, fin, by = "hour"), fill = list(value = NA)) %>% 
    mutate(chmes_coderhj = contexte$station) %>% 
    mutate(chmes_typemesure = contexte$typemesure) %>% 
    mutate(chmes_unite = contexte$unite) %>% 
    mutate(chmes_validation = "Validé") %>% 
    formatage.date.heure() %>% 
    mutate(`_modif_date` = NA) # Car supprimé par étape préalable : formatage.date.heure()
  # view()
  
  #### Nettoyage & reformatage ####
  data_v4 <-
    data_v3 %>% 
    arrange(chmes_coderhj, chmes_date, chmes_heure) %>% 
    select(match(colnames(data), names(.)))
  
  #### Sortie ####
  return(data_v4)
  
  
  return(data)
  
}