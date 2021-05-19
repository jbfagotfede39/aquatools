#' Renommage des variables de chroniques
#'
#' Cette fonction permet de renommer des variables de traitement de données de chroniques
#' @name chronique.variables.renommage
#' @param data Data.frame contenant des variables à renommer
#' @param formatentree Type de format de noms de champs en entrée : \code{Tous} (par défaut), ou bien uniquement pour convertir certains : \code{chmes}, \code{chmesgr}, \code{chsta}, \code{chsvi}, \code{chres}
#' @param formatsortie Type de format de noms de champs en sortie : \code{chmes} (par défaut), \code{chmesgr}, \code{chsta}, \code{chsvi}, \code{chres}, \code{param} (pour utilisation générique)
#' @keywords chronique
#' @import glue
#' @import stringr
#' @import tidyverse
#' @export
#' @examples
#' Resultats %>% chronique.variables.renommage()
#' Resultats %>% chronique.variables.renommage(Sortie = "chmesgr")

##### TODO LIST #####
# Implanter cette fonction dans chronique.resultat.filtrage() + chronique.cle() + chronique.resultats.periode
# 
#####################

chronique.variables.renommage <- function(
  data = data,
  formatentree = c("Tous", "chmes", "chmesgr", "chsta", "chsvi", "chres"),
  formatsortie = c("chmes", "chmesgr", "chsta", "chsvi", "chres", "param")
  )
{
  
  #### Évaluation des choix ####
  formatentree <- match.arg(formatentree)
  formatsortie <- match.arg(formatsortie)
  
  #### Modification des entrées ####
  formatentree <- glue("{formatentree}_")
  formatsortie <- glue("{formatsortie}_")
  listecompleteformatentree <- c("chmes", "chmesgr", "chsta", "chsvi", "chres")
  
  #### Renommage des champs ####
  if(formatentree != "Tous_"){
  data <-
    data %>% 
    rename_with(~str_replace(., formatentree, formatsortie), .cols = everything())
  }
  
  if(formatentree == "Tous_"){
    for(i in 1:length(listecompleteformatentree)){
    data <-
      data %>% 
      rename_with(~str_replace(., listecompleteformatentree[i], formatsortie), .cols = everything())
    }
  }
  
  #### Mise en minuscules ####
  data <-
    data %>% 
    rename_with(stringr::str_to_lower, locale = "fr")
  
  #### Affichage des résultats ####
  return(data)
}
