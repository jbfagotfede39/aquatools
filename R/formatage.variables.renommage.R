#' Renommage des variables de chroniques et physico-chimie
#'
#' Cette fonction permet de renommer des variables de traitement de données de chroniques et de physico-chimie
#' @name formatage.variables.renommage
#' @param data Data.frame contenant des variables à renommer
#' @keywords chronique
#' @import glue
#' @import stringr
#' @import tidyverse
#' @export
#' @examples
#' Resultats %>% formatage.variables.renommage()

formatage.variables.renommage <- function(
  data = data
  )
{
  
  
  #### Renommage des champs ####
  data <-
    data %>% 
    PC.variables.renommage(formatsortie = "param") %>% 
    chronique.variables.renommage(formatsortie = "param")
    
  
  #### Affichage des résultats ####
  return(data)
}
