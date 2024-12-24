#' Compensation barométrique des séries piézométriques
#'
#' Permet de compenser barométriquement des séries piézométriques, avec la valeur barométrique la plus proche
#' @name chronique.compensation.barometrie
#' @param data Chronique à modifier, avec un champ chmes_date, un champ chmes_heure et un champ chmes_valeur
#' @param duree_max_rattachement Durée maximale de rattachement, en heures - \code{1} (par défaut)
#' @param sortie Format de sortie - \code{compensé} (par défaut) : uniquement le jeu de données nouvellement compensé, \code{compensé_avec_vide} : uniquement le jeu de données nouvellement compensé, \code{tout} : jeu de données initial + jeu de données compensées, \code{large} : format large pour étude intermédiaire.
#' @keywords chronique
#' @import tidyverse
#' @export
#' @examples
#' chronique.compensation.barometrie(data)
#' chronique.compensation.barometrie(data, decalage = 258.58)
#' chronique.compensation.barometrie(decalage = -57)

chronique.compensation.barometrie <- function(data, 
                                              duree_max_rattachement = 1,
                                              sortie = c("compensé", "compensé_avec_vide", "tout", "large"))
{

  #### Évaluation des choix ####
  sortie <- match.arg(sortie)
  
  #### Nettoyage et reformatage ####
  data_v2 <-
    data %>% 
    formatage.time() %>% 
    filter(!is.na(time)) %>% 
    arrange(time) %>% 
    mutate(chmes_typemesure_simpl = case_when(chmes_typemesure == "Barométrie" ~ "baro",
                                              grepl("Piézométrie", chmes_typemesure) ~ "piezo"
    ), .after = "chmes_typemesure")
  
  
  data_v2_baro <- data_v2 %>% filter(chmes_typemesure_simpl == "baro") %>% rename(chmes_valeur_baro = chmes_valeur)
  data_v2_piezo <- data_v2 %>% filter(chmes_typemesure_simpl == "piezo")
  
  #### Contexte ####
  contexte_baro <- data %>% filter(grepl("aro", chmes_typemesure)) %>% chronique.contexte()
  contexte_piezo <- data %>% filter(grepl("iézo", chmes_typemesure)) %>% chronique.contexte()
  
  #### Test de cohérence ####
  if(contexte_baro$nstation != 1) stop(glue("Attention : il y a différentes stations de barométrie : {contexte_baro$station}"))
  if(contexte_baro$ntypemesure != 1) stop(glue("Attention : il y a différents types de mesures de barométrie : {contexte_baro$typemesure}"))
  if(contexte_piezo$ntypemesure != 1) stop(glue("Attention : il y a différents types de mesures de piézométrie : {contexte_piezo$typemesure}"))
  if(contexte_baro$nunite != 1) stop(glue("Attention : il y a différents unités de barométrie : {contexte_baro$unite}"))
  if(contexte_piezo$nunite != 1) stop(glue("Attention : il y a différents unités de piézométrie : {contexte_piezo$unite}"))
  if(contexte_piezo$unite != contexte_baro$unite) stop(glue("Attention : il y a différentes unités entre piézométrie et barométrie : respectivement {contexte_piezo$unite} et {contexte_baro$unite}"))
  
  # Test du nb de mesures pas uniques #
  baro_avec_doublons <-
    data_v2_baro %>% 
    summarise(n = n(), 
              .by = c(time, chmes_coderhj, chmes_typemesure_simpl)) %>% 
    filter(n > 1)
  
  piezo_avec_doublons <-
    data_v2_baro %>% 
    summarise(n = n(), 
              .by = c(time, chmes_coderhj, chmes_typemesure_simpl)) %>% 
    filter(n > 1)
  
  if(baro_avec_doublons %>% nrow() != 0) stop(glue("Présence de doublons dans la barométrie : {baro_avec_doublons$time}"))
  if(piezo_avec_doublons %>% nrow() != 0) stop(glue("Présence de doublons dans la piézométrie : {piezo_avec_doublons$time}"))
  
  #### Calcul ####
  ##### Paramètres #####
  duree_max_rattachement_minutes <- duree_max_rattachement * 60
  
  ##### Calculs à proprement parler #####
  data_v3 <-
    data_v2 %>% 
    select(chmes_coderhj, chmes_typemesure_simpl, time, chmes_valeur) %>% 
    pivot_wider(id_cols = time, names_from = c("chmes_coderhj", "chmes_typemesure_simpl"), values_from = "chmes_valeur")
  
  data_v4 <-
    data_v3 %>% 
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
  data_v5 <-
    data_v4 %>% 
    rowwise() %>% 
    mutate(ecart_time_baro_min_retenu = min(ecart_time_baro_descendant, ecart_time_baro_montant), .after = "baro") %>% 
    ungroup() %>% 
    mutate(time_baro_retenu = ifelse(ecart_time_baro_min_retenu == ecart_time_baro_descendant, time_baro_descendant, time_baro_montant), .after = "baro") %>% 
    mutate(valeur_baro_retenu = ifelse(ecart_time_baro_min_retenu == ecart_time_baro_descendant, baro_descendant, baro_montant), .after = "baro")
  
  ## Nettoyage ##
  data_v6 <-
    data_v5 %>% 
    # Nettoyage des colonnes #
    select(-contains("montant"), -contains("descendant")) %>% 
    # Nettoyage des lignes #
    filter(if_any(contains("piezo"), ~ !is.na(.)))
  
  ## Calcul de compensation et remise en format long ##
  data_v7 <-
    data_v6 %>% 
    mutate(baro = valeur_baro_retenu) %>% # Reversement de la barométrie dans la colonne ad-hoc
    mutate(baro = ifelse(ecart_time_baro_min_retenu <= minutes(duree_max_rattachement_minutes), baro, NA)) # Cas où la durée de rattachement de la piézométrie est supérieure à la valeur seuil duree_max_rattachement_minutes
  
  data_v8 <-
    data_v7 %>% 
    mutate(across(contains("piezo"), ~ .x - baro)) %>% # Calcul à proprement parler
    select(-contains("baro")) %>% # Nettoyage
    pivot_longer(cols = contains("piezo"), names_to = "chmes_coderhj", values_to = "chmes_valeur_comp") %>% # Remise en format long
    mutate(chmes_coderhj = str_replace(chmes_coderhj, "_piezo", ""))
  
  data_v7_nettoyees <-
    data_v7 %>% 
    select(-contains("retenu")) # Nettoyage pour affichage en sortie == "large"
  
  ## Re-jointure avec le jeu de données initial ##
  mesures_compensees <-
    data_v8 %>% 
    formatage.date.heure() %>% 
    chronique.cle("HS") %>% 
    select(-chmes_date, -chmes_heure, -chmes_coderhj) %>% 
    left_join(data %>% 
                chronique.cle("HS"), 
              by = join_by(Cle)) %>% 
    mutate(chmes_valeur = chmes_valeur_comp) %>% 
    mutate(id = NA_integer_) %>% 
    mutate(chmes_typemesure = "Piézométrie compensée") %>% 
    mutate(chmes_validation = "À valider") %>% 
    mutate(chmes_mode_acquisition = "Calculé") %>% 
    select(-Cle, -chmes_valeur_comp)
  
  #### Vérification ####
  mesures_compensees_sans_na <-
    mesures_compensees %>% 
    filter(!is.na(chmes_valeur))
  nb_mesures_sans_baro <- contexte_piezo$nmesure - nrow(mesures_compensees_sans_na)
  
  if(nb_mesures_sans_baro == 1 & duree_max_rattachement <= 1) warning(glue("Il y a {nb_mesures_sans_baro} mesure sans compensation barométrique avec un seuil de sensibilité à {duree_max_rattachement} heure"))
  if(nb_mesures_sans_baro == 1 & duree_max_rattachement > 1) warning(glue("Il y a {nb_mesures_sans_baro} mesure sans compensation barométrique avec un seuil de sensibilité à {duree_max_rattachement} heures"))
  if(nb_mesures_sans_baro > 1 & duree_max_rattachement <= 1) warning(glue("Il y a {nb_mesures_sans_baro} mesures sans compensation barométrique avec un seuil de sensibilité à {duree_max_rattachement} heure"))
  if(nb_mesures_sans_baro > 1 & duree_max_rattachement > 1) warning(glue("Il y a {nb_mesures_sans_baro} mesures sans compensation barométrique avec un seuil de sensibilité à {duree_max_rattachement} heures"))
  
  #### Sortie ####
  ## Différents formats de sortie ##
  if(sortie == "compensé") data_sortie <- mesures_compensees_sans_na
  if(sortie == "compensé_avec_vide") data_sortie <- mesures_compensees
  if(sortie == "tout") data_sortie <- data %>% bind_rows(mesures_compensees_sans_na)
  if(sortie == "large") data_sortie <- data_v7_nettoyees
  
  ## Sortie ##
  return(data_sortie)
  
} # Fin de la fonction