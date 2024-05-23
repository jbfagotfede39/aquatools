#' Reformatage de dates et heures en une colonne time
#'
#' Reformate les dates et les heures en une nouvelle colonne time
#' @name formatage.time
#' @param data Jeu de données contenant une colonne `chmes_date`, `chmesgr_date`, `chsvi_date`, `chres_date`, `pcmes_date` ou `pcsvi_date` et une colonne `chmes_heure`, `chmesgr_heure`, `chsvi_heure`, `chres_heure`, `pcmes_heure` ou `pcsvi_heure`.
#' @keywords donnees
#' @import glue
#' @import tidyverse
#' @export
#' @examples
#' formatage.time(data)

formatage.time <- function(
  data)
  {

  #### Formatage ####
  data <- 
    data %>% 
    dplyr::select(-contains("time")) # On l'enlève si elle existe déjà pour être certain du résultat
  
  data_renommees <- 
    data %>% 
    formatage.variables.renommage()
  
  #### Calculs ####
  ## Préparation des données ##
    data_corrigees <- 
      data_renommees
    
    ## Mesures ##
    data_corrigees <- 
      data_corrigees %>% 
      mutate(time = ymd_hms(glue("{param_date} {param_heure}")), .after = "param_heure")

    ## Nettoyage des données ##
    data_nettoyees <- 
      data_corrigees
    
    ## Renommage correct ##
    data_ok <-
      data_nettoyees %>% 
      {if(data %>% select(contains("chmes_date")) %>% ncol() != 0) rename(., chmes_date = param_date) else .} %>% 
      {if(data %>% select(contains("chmesgr_date")) %>% ncol() != 0) rename(., chmesgr_date = param_date) else .} %>% 
      {if(data %>% select(contains("chsvi_date")) %>% ncol() != 0) rename(., chsvi_date = param_date) else .} %>% 
      {if(data %>% select(contains("chres_date")) %>% ncol() != 0) rename(., chres_date = param_date) else .} %>% 
      {if(data %>% select(contains("pcmes_date")) %>% ncol() != 0) rename(., pcmes_date = param_date) else .} %>% 
      {if(data %>% select(contains("pcsvi_date")) %>% ncol() != 0) rename(., pcsvi_date = param_date) else .} %>% 
      {if(data %>% select(contains("chmes_heure")) %>% ncol() != 0) rename(., chmes_heure = param_heure) else .} %>%
      # {if(data %>% select(contains("chmesgr_heure")) %>% ncol() != 0) rename(., chmesgr_heure = param_heure) else .} %>% 
      {if(data %>% select(contains("chsvi_heure")) %>% ncol() != 0) rename(., chsvi_heure = param_heure) else .} %>% 
      {if(data %>% select(contains("chres_heure")) %>% ncol() != 0) rename(., chres_heure = param_heure) else .} %>% 
      {if(data %>% select(contains("pcmes_heure")) %>% ncol() != 0) rename(., pcmes_heure = param_heure) else .} %>% 
      {if(data %>% select(contains("pcsvi_heure")) %>% ncol() != 0) rename(., pcsvi_heure = param_heure) else .} %>% 
      {if(data %>% select(contains("chmes_valeur")) %>% ncol() != 0) rename(., chmes_valeur = param_valeur) else .} %>%
      {if(data %>% select(contains("chmesgr_valeur")) %>% ncol() != 0) rename(., chmesgr_valeur = param_valeur) else .} %>%
      {if(data %>% select(contains("chsvi_valeur")) %>% ncol() != 0) rename(., chsvi_valeur = param_valeur) else .} %>% 
      {if(data %>% select(contains("chres_valeur")) %>% ncol() != 0) rename(., chres_valeur = param_valeur) else .} %>% 
      {if(data %>% select(contains("pcmes_valeur")) %>% ncol() != 0) rename(., pcmes_valeur = param_valeur) else .} %>% 
      {if(data %>% select(contains("pcsvi_valeur")) %>% ncol() != 0) rename(., pcsvi_valeur = param_valeur) else .} %>% 
      {if(data %>% select(contains("chmes_typemesure")) %>% ncol() != 0) rename(., chmes_typemesure = param_typemesure) else .} %>%
      {if(data %>% select(contains("chmesgr_typemesure")) %>% ncol() != 0) rename(., chmesgr_typemesure = param_typemesure) else .} %>%
      {if(data %>% select(contains("chsvi_typemesure")) %>% ncol() != 0) rename(., chsvi_typemesure = param_typemesure) else .} %>% 
      {if(data %>% select(contains("chres_typemesure")) %>% ncol() != 0) rename(., chres_typemesure = param_typemesure) else .} %>% 
      {if(data %>% select(contains("pcmes_typemesure")) %>% ncol() != 0) rename(., pcmes_typemesure = param_typemesure) else .} %>% 
      {if(data %>% select(contains("pcsvi_typemesure")) %>% ncol() != 0) rename(., pcsvi_typemesure = param_typemesure) else .} %>% 
      {if(data %>% select(contains("chmes_unite")) %>% ncol() != 0) rename(., chmes_unite = param_unite) else .} %>%
      {if(data %>% select(contains("chmesgr_unite")) %>% ncol() != 0) rename(., chmesgr_unite = param_unite) else .} %>%
      {if(data %>% select(contains("chsvi_unite")) %>% ncol() != 0) rename(., chsvi_unite = param_unite) else .} %>% 
      {if(data %>% select(contains("chres_unite")) %>% ncol() != 0) rename(., chres_unite = param_unite) else .} %>% 
      {if(data %>% select(contains("pcmes_unite")) %>% ncol() != 0) rename(., pcmes_unite = param_unite) else .} %>% 
      {if(data %>% select(contains("pcsvi_unite")) %>% ncol() != 0) rename(., pcsvi_unite = param_unite) else .}
  
return(data_ok)

} # Fin de la fonction
