#' Rattachement de valeurs piézométriques manquantes
#'
#' Permet de rattracher une valeur piézométrique à la valeur barométrique la plus proche temporellement
#' @name chronique.rattachement.barometrie
#' @param data Chronique à modifier, avec un champ chmes_date, un champ \code{time} et les jeux de données en colonne (format large)
#' @param duree_max_rattachement Durée maximale de rattachement, en heures - \code{1} (par défaut)
#' @keywords chronique
#' @import tidyverse
#' @export
#' @examples
#' chronique.rattachement.barometrie(data)
#' chronique.rattachement.barometrie(duree_max_rattachement = 2)

chronique.rattachement.barometrie <- function(data,
                                              duree_max_rattachement = 1)
{
  
  #### Évaluation des choix ####
  # sortie <- match.arg(sortie)
  
  #### Calcul ####  
  ##### Paramètres #####
  duree_max_rattachement_minutes <- duree_max_rattachement * 60
  
  ##### Calculs en tant que tels #####
  data_v2 <-
    data %>% 
    rename(baro = contains("baro")) %>% 
    ### Bloc montant ###
    # Duplication de la colonne _baro dans une nouvelle, avec complément vers le haut des trous
    mutate(baro_montant = baro) %>% 
    fill(baro_montant, .direction = "up") %>% 
    fill(baro_montant, .direction = "down") %>% # s'il manque des valeurs au départ
    # mutate(time_baro_montant = ifelse(is.na(baro), NA, as.character(time))) %>% 
    mutate(time_baro_montant = ifelse(is.na(baro), NA, format(time, format="%Y-%m-%d %H:%M:%S"))) %>% 
    fill(time_baro_montant, .direction = "up") %>% 
    fill(time_baro_montant, .direction = "down") %>% # s'il manque des valeurs au départ
    mutate(ecart_time_baro_montant = abs(ymd_hms(time_baro_montant) - time)) %>% 
    
    ### Bloc descendant ###
    # Duplication de la colonne _baro dans une nouvelle, avec complément vers le bas des trous
    mutate(baro_descendant = baro) %>% 
    fill(baro_descendant, .direction = "down") %>% 
    fill(baro_descendant, .direction = "up") %>% # s'il manque des valeurs au départ
    # mutate(time_baro_descendant = ifelse(is.na(baro), NA, as.character(time))) %>% 
    mutate(time_baro_descendant = ifelse(is.na(baro), NA, format(time, format="%Y-%m-%d_%H:%M:%S"))) %>% 
    fill(time_baro_descendant, .direction = "down") %>% 
    fill(time_baro_descendant, .direction = "up") %>% # s'il manque des valeurs au départ
    mutate(ecart_time_baro_descendant = abs(ymd_hms(time_baro_descendant) - time))
  
  # Calcul des écarts temporels entre les deux versions afin de ne conserver que les lignes avec les plus faibles #
  data_v3 <-
    data_v2 %>% 
    rowwise() %>% 
    mutate(ecart_time_baro_min_retenu = min(ecart_time_baro_descendant, ecart_time_baro_montant), .after = "baro") %>% 
    ungroup() %>% 
    mutate(time_baro_retenu = ifelse(ecart_time_baro_min_retenu == ecart_time_baro_descendant, time_baro_descendant, time_baro_montant), .after = "baro") %>% 
    mutate(valeur_baro_retenu = ifelse(ecart_time_baro_min_retenu == ecart_time_baro_descendant, baro_descendant, baro_montant), .after = "baro")
  
  ## Nettoyage ##
  data_v4 <-
    data_v3 %>% 
    select(-contains("montant"), -contains("descendant")) %>% # Nettoyage des colonnes
    filter(if_any(contains("piezo"), ~ !is.na(.))) %>% # Nettoyage des lignes
    mutate(baro = valeur_baro_retenu) %>% # Reversement de la barométrie dans la colonne ad-hoc
    relocate(baro, .after = time) %>% # Reversement de la barométrie dans la colonne ad-hoc
    mutate(baro = ifelse(ecart_time_baro_min_retenu <= minutes(duree_max_rattachement_minutes), baro, NA)) # Cas où la durée de rattachement de la piézométrie est supérieure à la valeur seuil duree_max_rattachement_minutes

  ## Sortie ##
  return(data_v4)
  
} # Fin de la fonction
