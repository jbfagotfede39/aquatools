#' Reformatage de dates en année biologique
#'
#' Reformate les dates selon l'année biologique (en rapport au 1 octobre de chaque année) dans une colonne chmes_anneebiol ou chsvi_anneebiol
#' @name formatage.annee.biologique
#' @param data Jeu de données contenant une colonne chmes_date ou chsvi_date au format Date
#' @param datedebutanneebiol Date de démarrage de l'année biologique : 10-01 (par défaut - 1er octobre)
#' @keywords donnees
#' @import tidyverse
#' @export
#' @examples
#' formatage.annee.biologique(data)

formatage.annee.biologique <- function(
  data = data,
  datedebutanneebiol = "10-01")
  {

##### TODO LIST #####
# Il faudrait ajouter un test pour connaître le coltype du champ qui contient la chaîne date # if(inherits(data %>% select(contains("date"), -`_modif_date`), "date") == FALSE)stop("Le champ de date en entrée doit être au format date") # À amélorier car ne fonctionne pas en l'état
# Utiliser la fonction chronique.renommage.variables() pour pouvoir utiliser la fonction avec chmesgr_date
#####################
  #### Vérifications ####
  if(nchar(datedebutanneebiol) != 5) stop("Mauvais format de date de départ")
  
  #### Calculs ####
  data <- 
    data %>% 
    {if("chmes_anneebiol" %in% names(data) == T) dplyr::select(., -chmes_anneebiol) else .} # On l'enlève si elle existe déjà pour être certain de calculer avec la bonne date de seuil
  
### Ancien fonctionnement qu'on maintient par sécurité ###
if (datedebutanneebiol == "10-01") {
  ## Mesures ##
  if ("chmes_date" %in% colnames(data)) {
    data$chmes_anneebiol <- ifelse(month(data$chmes_date) < 10, year(data$chmes_date), year(data$chmes_date) + 1)
  }

  ## Suivi ##
  if ("chsvi_date" %in% colnames(data)) {
    data$chsvi_anneebiol <- ifelse(month(data$chsvi_date) < 10, year(data$chsvi_date), year(data$chsvi_date) + 1)
  }
}
  
  # Nouveau fonctionnement avec calcul de la date
  if (datedebutanneebiol != "10-01") {
    ## Préparation des données ##
    data <- 
      data %>% 
      add_column(datedebutanneebiol = datedebutanneebiol)
    
    ## Mesures ##
    if ("chmes_date" %in% colnames(data)) {
      data <- 
        data %>% 
        mutate(datesansannee = str_sub(chmes_date, 6, 10)) %>% 
        mutate(chmes_anneebiol = ifelse(datedebutanneebiol < "07-01" & datesansannee >= datedebutanneebiol, year(chmes_date), NA_integer_)) %>% 
        mutate(chmes_anneebiol = ifelse(datedebutanneebiol < "07-01" & datesansannee < datedebutanneebiol & !is.na(datesansannee), year(chmes_date)-1, chmes_anneebiol)) %>% 
        mutate(chmes_anneebiol = ifelse(datedebutanneebiol >= "07-01" & datesansannee >= datedebutanneebiol, year(chmes_date) + 1, chmes_anneebiol)) %>% 
        mutate(chmes_anneebiol = ifelse(datedebutanneebiol >= "07-01" & datesansannee < datedebutanneebiol & !is.na(datesansannee), year(chmes_date), chmes_anneebiol)) 
    }
    
    ## Suivi ##
    if ("chsvi_date" %in% colnames(data)) {
      data <- 
        data %>% 
        mutate(datesansannee = str_sub(chsvi_date, 6, 10)) %>% 
        mutate(chsvi_anneebiol = ifelse(datedebutanneebiol < "07-01" & datesansannee >= datedebutanneebiol, year(chsvi_date), NA_character_)) %>% 
        mutate(chsvi_anneebiol = ifelse(datedebutanneebiol < "07-01" & datesansannee < datedebutanneebiol & !is.na(datesansannee), year(chsvi_date)-1, chsvi_anneebiol)) %>% 
        mutate(chsvi_anneebiol = ifelse(datedebutanneebiol >= "07-01" & datesansannee >= datedebutanneebiol, year(chsvi_date) + 1, chsvi_anneebiol)) %>% 
        mutate(chsvi_anneebiol = ifelse(datedebutanneebiol >= "07-01" & datesansannee < datedebutanneebiol & !is.na(datesansannee), year(chsvi_date), chsvi_anneebiol)) 
    }
    
    ## Nettoyage des données ##
    data <- 
      data %>% 
      dplyr::select(-datesansannee, -datedebutanneebiol)
  }
  
return(data)

} # Fin de la fonction