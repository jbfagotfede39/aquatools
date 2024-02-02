#' Refactoring des données de l'API HydroVu
#'
#' Cette fonction permet de reformater les données collectées sur l'API HydroVu
#' @name hydrovu.refactoring
#' @param data Données collectées sur l'API via \code{hydrovu.donnees}
#' @keywords chronique
#' @import glue
#' @import lubridate
#' @import tidyverse
#' @export 
#' @examples
#' hydrovu.authentification(id_user, id_key) %>% hydrovu.donnees("4722100874248192") %>% hydrovu.refactoring()

hydrovu.refactoring <- function(
    data
  )
{
  
  #### Calcul ####
  data_2 <-
    data %>% 
    unlist() %>% 
    enframe()
  
  if(nrow(data_2) == 4) data_3 <- NULL # Cas avec absence de données
  # if(nrow(data_2) == 4) data_3 <- invisible(NULL) # Cas avec absence de données
  
  if(nrow(data_2) != 4){ # Cas avec présence de données
  #### Contexte ####
  station <- data_2 %>% filter(grepl("locationId", name)) %>% pull(value)
  parametres_unites <-
    data_2 %>% 
    filter(grepl("parameterId|unitId", name)) %>% 
    mutate(unite = ifelse(grepl("unitId", name), value, NA_character_)) %>% 
    fill(unite, .direction = "up") %>% 
    filter(grepl("parameterId", name)) %>% 
    select(-name) %>% 
    rename(parametre = value)
  
  #### Nettoyage & reformatage ####
  data_3 <-
    data_2 %>% 
    mutate(parametre = ifelse(grepl("parameterId", name), value, NA_character_)) %>% 
    fill(parametre, .direction = "down") %>% 
    filter(!grepl("locationId|parameterId|unitId|customParameter", name)) %>% 
    mutate(cle = ifelse(row_number() %% 2 == 1, row_number(), NA)) %>% fill(cle, .direction = "down") %>% 
    mutate(type_valeur = str_split_i(.$name, "\\.", 3)) %>%
    select(cle, parametre, type_valeur, value) %>% 
    pivot_wider(id_cols = c(cle, parametre), names_from = "type_valeur", values_from = "value") %>% 
    mutate(time = lubridate::as_datetime(as.numeric(timestamp)), .keep = "unused") %>% 
    mutate(chmes_date = format(time, format = "%Y-%m-%d")) %>% 
    mutate(chmes_heure = format(time, format = "%H:%M:%S")) %>% 
    mutate(chmes_coderhj = station) %>% 
    select(chmes_coderhj, chmes_date, chmes_heure, value, parametre) %>%
    left_join(parametres_unites, by = join_by(parametre)) %>% 
    rename(chmes_valeur = value) %>% 
    rename(chmes_typemesure = parametre) %>% 
    rename(chmes_unite = unite) %>% 
    mutate(chmes_typemesure = ifelse(chmes_typemesure == "1", "Thermie piézométrique", chmes_typemesure)) %>% 
    mutate(chmes_typemesure = ifelse(chmes_typemesure == "2", "Piézométrie compensée", chmes_typemesure)) %>% # Pressure
    mutate(chmes_typemesure = ifelse(chmes_typemesure == "3", "Piézométrie compensée", chmes_typemesure)) %>% # Depth
    mutate(chmes_typemesure = ifelse(chmes_typemesure == "5", "Piézométrie compensée", chmes_typemesure)) %>% # Level, Surface Elevation (Enable with VuSitu)
    mutate(chmes_typemesure = ifelse(chmes_typemesure == "16", "Barométrie", chmes_typemesure)) %>% # Pression atmosphérique en PSI
    mutate(chmes_typemesure = ifelse(chmes_typemesure == "33", "Niveau de batterie", chmes_typemesure)) %>% 
    mutate(chmes_typemesure = ifelse(chmes_typemesure == "Conductivite", "Conductivité", chmes_typemesure)) %>% 
    mutate(chmes_typemesure = ifelse(chmes_typemesure == "Temperature", "Thermie", chmes_typemesure)) %>% 
    mutate(chmes_unite = ifelse(chmes_unite == "1", "°C", chmes_unite)) %>% 
    mutate(chmes_unite = ifelse(chmes_unite == "17", "PSI", chmes_unite)) %>% 
    mutate(chmes_unite = ifelse(chmes_unite == "35", "m", chmes_unite)) %>% 
    mutate(chmes_unite = ifelse(chmes_unite == "241", "%", chmes_unite)) %>% 
    mutate(chmes_unite = ifelse(chmes_unite == "Unspecified", NA_character_, chmes_unite)) %>% 
    mutate(tracking = now())
  }
  
  #### Sortie ####
  return(data_3)
  
} # Fin de la fonction