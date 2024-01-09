#' Reformatage de dates en année biologique
#'
#' Reformate les dates selon l'année biologique (en rapport au 1 octobre de chaque année) dans une colonne chmes_anneebiol ou chsvi_anneebiol
#' @name formatage.annee.biologique
#' @param data Jeu de données contenant une colonne `chmes_date`, `chmesgr_date`, `chsvi_date`, `chres_date`, `pcmes_date` ou `pcsvi_date` au format Date
#' @param datedebutanneebiol Date de démarrage de l'année biologique : 10-01 (par défaut - 1er octobre)
#' @keywords donnees
#' @import tibble
#' @import tidyverse
#' @export
#' @examples
#' formatage.annee.biologique(data)

formatage.annee.biologique <- function(
  data,
  datedebutanneebiol = "10-01")
  {

##### TODO LIST #####
# Il faudrait ajouter un test pour connaître le coltype du champ qui contient la chaîne date # if(inherits(data %>% select(contains("date"), -`_modif_date`), "date") == FALSE)stop("Le champ de date en entrée doit être au format date") # À amélorier car ne fonctionne pas en l'état
#####################
  
  #### Vérifications ####
  if(nchar(datedebutanneebiol) != 5) stop("Mauvais format de date de départ")
  
  #### Formatage ####
  data <- 
    data %>% 
    dplyr::select(-contains("anneebiol")) # On l'enlève si elle existe déjà pour être certain de calculer avec la bonne date de seuil
  
  data_renommees <- 
    data %>% 
    # chronique.variables.renommage(formatentree = "Tous", formatsortie = "param") %>% 
    {if("chmes_date" %in% colnames(.)) rename(., param_date = chmes_date) else .} %>% 
    {if("chmesgr_date" %in% colnames(.)) rename(., param_date = chmesgr_date) else .} %>% 
    {if("chsvi_date" %in% colnames(.)) rename(., param_date = chsvi_date) else .} %>% 
    {if("chres_date" %in% colnames(.)) rename(., param_date = chres_date) else .} %>% 
    {if("pcmes_date" %in% colnames(.)) rename(., param_date = pcmes_date) else .} %>% 
    {if("pcsvi_date" %in% colnames(.)) rename(., param_date = pcsvi_date) else .}
  
  #### Calculs ####
  ## Préparation des données ##
    data_corrigees <- 
      data_renommees %>% 
      add_column(datedebutanneebiol = datedebutanneebiol)
    
    ## Mesures ##
    data_corrigees <- 
      data_corrigees %>% 
      mutate(datesansannee = str_sub(param_date, 6, 10)) %>% 
      mutate(chmes_anneebiol = ifelse(datedebutanneebiol < "07-01" & datesansannee >= datedebutanneebiol, year(param_date), NA_integer_)) %>% 
      mutate(chmes_anneebiol = ifelse(datedebutanneebiol < "07-01" & datesansannee < datedebutanneebiol & !is.na(datesansannee), year(param_date)-1, chmes_anneebiol)) %>% 
      mutate(chmes_anneebiol = ifelse(datedebutanneebiol >= "07-01" & datesansannee >= datedebutanneebiol, year(param_date) + 1, chmes_anneebiol)) %>% 
      mutate(chmes_anneebiol = ifelse(datedebutanneebiol >= "07-01" & datesansannee < datedebutanneebiol & !is.na(datesansannee), year(param_date), chmes_anneebiol))

    ## Nettoyage des données ##
    data_nettoyees <- 
      data_corrigees %>% 
      dplyr::select(-datesansannee, -datedebutanneebiol)
    
    ## Renommage correct ##
    data_ok <-
      data_nettoyees %>% 
      {if(data %>% select(contains("chmes_date")) %>% ncol() != 0) rename(., chmes_date = param_date) else .} %>% 
      {if(data %>% select(contains("chmesgr_date")) %>% ncol() != 0) rename(., chmesgr_date = param_date) else .} %>% 
      {if(data %>% select(contains("chsvi_date")) %>% ncol() != 0) rename(., chsvi_date = param_date) else .} %>% 
      {if(data %>% select(contains("chres_date")) %>% ncol() != 0) rename(., chres_date = param_date) else .} %>% 
      {if(data %>% select(contains("pcmes_date")) %>% ncol() != 0) rename(., pcmes_date = param_date) else .} %>% 
      {if(data %>% select(contains("pcsvi_date")) %>% ncol() != 0) rename(., pcsvi_date = param_date) else .} %>% 
      # {if(data %>% select(contains("chmes_date")) %>% ncol() != 0) rename(., chmes_anneebiol = chmes_anneebiol) else .} %>% 
      {if(data %>% select(contains("chmesgr_date")) %>% ncol() != 0) rename(., chmesgr_anneebiol = chmes_anneebiol) else .} %>% 
      {if(data %>% select(contains("chsvi_date")) %>% ncol() != 0) rename(., chsvi_anneebiol = chmes_anneebiol) else .} %>% 
      {if(data %>% select(contains("chres_date")) %>% ncol() != 0) rename(., chres_anneebiol = chmes_anneebiol) else .} %>% 
      {if(data %>% select(contains("pcmes_date")) %>% ncol() != 0) rename(., pcmes_anneebiol = chmes_anneebiol) else .} %>% 
      {if(data %>% select(contains("pcsvi_date")) %>% ncol() != 0) rename(., pcsvi_anneebiol = chmes_anneebiol) else .}
  
return(data_ok)

} # Fin de la fonction
