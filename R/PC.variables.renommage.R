#' Renommage des variables de physico-chimie
#'
#' Cette fonction permet de renommer des variables de traitement de données de physico-chimie
#' @name PC.variables.renommage
#' @param data Data.frame contenant des variables à renommer
#' @param formatentree Type de format de noms de champs en entrée : \code{Tous} (par défaut), ou bien uniquement pour convertir certains : \code{pcmes} et \code{pcsvi}
#' @param formatsortie Type de format de noms de champs en sortie : \code{pcmes} (par défaut), \code{pcsvi} ou \code{param} (pour utilisation générique)
#' @keywords chronique
#' @import glue
#' @import stringr
#' @import tidyverse
#' @export
#' @examples
#' Resultats %>% PC.variables.renommage()
#' Resultats %>% PC.variables.renommage(formatsortie = "param")

PC.variables.renommage <- function(
  data = data,
  formatentree = c("Tous", "pcmes", "pcsvi"),
  formatsortie = c("chmes", "pcsvi", "param")
  )
{
  
  #### Évaluation des choix ####
  formatentree <- match.arg(formatentree)
  formatsortie <- match.arg(formatsortie)
  
  #### Modification des entrées ####
  formatentree <- glue("{formatentree}_")
  formatsortie <- glue("{formatsortie}")
  listecompleteformatentree <- c("pcmes", "pcsvi")
  
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
