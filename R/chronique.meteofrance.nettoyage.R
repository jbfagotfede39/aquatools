#' Nettoyage de données Météo-France
#'
#' Cette fonction permet de nettoyer des données Météo-France collectées à partir de l'API
#' @name chronique.meteofrance.nettoyage
#' @param data Données brutes issues de \code{chronique.meteofrance.commande()}
#' @param format Format de sortie : \code{Long} par défaut, ou \code{Large}
#' @import httr2
#' @import tidyverse
#' @export
#' @examples
#' chronique.meteofrance.mesures("Quotidien", "39159002", "2025-01-01T00:00:00Z", "2025-04-01T00:00:00Z", token) %>% chronique.meteofrance.commande(token) %>% chronique.meteofrance.nettoyage()
#' chronique.meteofrance.mesures("Quotidien", "39159002", "2025-01-01T00:00:00Z", "2025-04-01T00:00:00Z", token) %>% chronique.meteofrance.commande(token) %>% chronique.meteofrance.nettoyage(format = "Large")

chronique.meteofrance.nettoyage <- function(
    data = NA_character_,
    format = c("Long", "Large")
    
)
{
  #### Évaluation des choix ####
  format <- match.arg(format)
  
  #### Test de cohérence ####
  if(length(data) == 0) stop("Aucune donnée fournie")
  
  #### Contexte ####
  type_date <- data %>% head(5) %>% mutate(nchar = nchar(DATE)) %>% distinct(nchar)
  if(length(type_date) != 1) stop("Attention : plusieurs formats de date dans le jeu de données fourni")  

  #### Nettoyage & reformatage ####
  ##### Tri #####
  data_to_add_2 <-
    data %>%
    arrange(DATE)

  ##### Reformatage des dates #####
  data_to_add_3 <-
    data_to_add_2 %>% 
    {if(type_date == 6) mutate(., DATE = ym(DATE)) else .} %>% # 6 caractères = mois
    {if(type_date == 8) mutate(., DATE = ymd(DATE)) else .} %>% # 8 caractères = jour
    {if(type_date == 10) mutate(., DATE = ymd_h(DATE)) else .} %>% # 10 caractères = horaire
    {if(type_date == 12) mutate(., DATE = ymd_hm(DATE)) else .} # 12 caractères = 6 minutes

  ##### Renommage des colonnes #####
  data_to_add_4 <-
    data_to_add_3 %>% 
    mutate(POSTE = as.character(POSTE)) %>% 
    rename(chmes_coderhj = POSTE) %>% 
    rename(chmes_date = DATE) %>% 
    {if(type_date <= 8) rename_with(., ~str_replace(., "chmes", "chmesgr"), .cols = everything()) else .}

  ##### Format long ou format large #####
  data_to_add_5 <-
    data_to_add_4 %>% 
    # {if(format == "Long") pivot_longer(., cols = -c("POSTE", "DATE"), names_to = "chmes_typemesure", values_to = "chmes_valeur") else .}
    {if(format == "Long") pivot_longer(., cols = -contains("chmes"), names_to = "chmes_typemesure", values_to = "chmes_valeur") else .}

  #### Sortie des données ####
  return(data_to_add_5)

} # Fin de la fonction
