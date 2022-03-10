#' Collecte des relevés sur meteociel.fr
#'
#' Cette fonction permet d'extraire depuis meteociel.fr les observations d'une station donnée pour un mois donné
#' @name chronique.meteociel
#' @param codestationrecherche Station Météo-France recherchée, au format 39013004
#' @param moisrecherche Mois recherché, au format 01
#' @param anneerecherche Année recherchée, au format 2020
#' @keywords chronique
#' @import glue
#' @import lubridate
#' @import rvest
#' @import tidyverse
#' @import xml2
#' @export
#' @examples
#' DataToAdd <- chronique.meteociel("39013004", 01, 2020)

chronique.meteociel <- function(
  codestationrecherche = NA_integer_,
  moisrecherche = NA_integer_,
  anneerecherche = NA_integer_
)
{
  
  #### Évaluation des choix ####
  # Type <- match.arg(Type)
  
  #### Adresse recherchée ####
  target_link <- glue('https://www.meteociel.fr/cartes_obs/climato2v4.php?code={codestationrecherche}&mois={moisrecherche}&annee={anneerecherche}&print=1')
  
  #### Web-scraping ####
  ### Chargement de la page ###
  target_page <- read_html(target_link)
  
  ### Décodage ###
  ## Test si la page contient des données ##
  valeur_retour <-
    target_page %>%
    html_nodes("body") %>%
    .[[1]] %>% 
    html_nodes("center") %>%
    .[[2]] %>% 
    html_nodes("table") %>% 
    # .[[2]] %>%
    .[[1]] %>% 
    html_text()
  
  if(valeur_retour != "Données non disponibles"){
  data_collectees <- 
    target_page %>%
    html_nodes("body") %>%
    .[[1]] %>% 
    html_nodes("center") %>%
    .[[2]] %>% 
    html_nodes("table") %>% 
    .[[2]] %>%
    html_table(header=T) %>% 
    as_tibble()
  } # Fin du test
  
  if(exists("data_collectees") == FALSE){ # Cas où la lecture n'a pas été réalisée
    data_collectees <- structure(list(Jour = character(0), `Température max.` = character(0), 
                           `Température min.` = character(0), `Précipitations 24h` = character(0), 
                           Ensoleillement = logical(0)), row.names = integer(0), class = c("tbl_df", 
                                                                                           "tbl", "data.frame"))
  }
  
  if(nrow(data_collectees) == 0){ # Cas où il n'y a pas de donnes disponibles
    data_collectees <- structure(list(Jour = character(0), `Température max.` = character(0), 
                           `Température min.` = character(0), `Précipitations 24h` = character(0), 
                           Ensoleillement = logical(0)), row.names = integer(0), class = c("tbl_df", 
                                                                                           "tbl", "data.frame"))
  }
  
  ### Renommage ###
  dataV2 <-
    data_collectees %>% 
    rename(date = Jour) %>%
    rename(tmax = `Température max.`) %>% 
    rename(tmin = `Température min.`) %>% 
    rename(precipitations = `Précipitations 24h`) %>% 
    {if ("Ensoleillement" %in% names(.)) dplyr::select(., -Ensoleillement) else .}
  
  ### Nettoyage ###
  dataV3 <-
    dataV2 %>% 
    filter(date != "") %>% # pour supprimer le total
    filter(date != glue('{wday(today(), label = TRUE, abbr = TRUE)}. {mday(today())}')) %>% # pour supprimer la journée en cours, qui se termine par (*)
    filter(!grepl("---", precipitations)) %>% # pour supprimer la journée en cours, peut contenir --- (les journées sans mesures contiennent également ça)
    mutate(tmax = str_replace(tmax, " °C", "")) %>% # suppression des degrés, plus propre et sans warning que conversion directe en numeric
    mutate(tmin = str_replace(tmin, " °C", "")) %>% # suppression des degrés, plus propre et sans warning que conversion directe en numeric
    mutate(precipitations = str_replace(precipitations, " mm", "")) %>% # suppression des mm, plus propre et sans warning que conversion directe en numeric
    mutate(precipitations = str_replace(precipitations, "(?s) .*", "")) %>% # suppression des (Tr), plus propre et sans warning que conversion directe en numeric
    mutate(tmax = as.numeric(sub(",", ".", tmax))) %>% 
    mutate(tmin = as.numeric(sub(",", ".", tmin))) %>% 
    mutate(precipitations = as.numeric(sub(",", ".", precipitations)))
  
  ### Mise au format long ###
  dataV4 <-
    dataV3 %>% 
    tidyr::pivot_longer(tmax:precipitations, names_to = "typemesure", values_to = "valeur")
  
  ### Complément ###
  dataV5 <-
    dataV4 %>% 
    mutate(date = str_replace(date, "[[:alpha:]]{3}[[:punct:]][[:space:]]", "")) %>% # ré-écriture de la date
    mutate(date = ymd(glue('{anneerecherche}-{moisrecherche}-{.$date}'))) %>% 
    mutate(coderhj_id = codestationrecherche) %>% 
    mutate(capteur_id = NA_integer_) %>% 
    mutate(periodicite = "Journalière") %>% 
    mutate(typeagregation = case_when(.$typemesure == "tmax" ~ "Maximum",
                                      .$typemesure == "tmin" ~ "Minimum",
                                      .$typemesure == "precipitations" ~ "Cumul")
    ) %>% 
    mutate(typemesure = case_when(.$typemesure == "tmax" ~ "Thermie barométrique",
                                  .$typemesure == "tmin" ~ "Thermie barométrique",
                                  .$typemesure == "precipitations" ~ "Pluviométrie")
    ) %>% 
    mutate(unite = case_when(.$typemesure == "Thermie barométrique" ~ "°C",
                             .$typemesure == "Pluviométrie" ~ "mm")) %>% 
    mutate(validation = NA_character_) %>% 
    mutate(mode_acquisition = NA_character_) %>% 
    mutate(mode_integration = NA_character_)
  
  ### Ré-organisation ###
  dataV6 <-
    dataV5 %>% 
    dplyr::select(coderhj_id, capteur_id, date, periodicite, typeagregation, valeur, unite, typemesure, validation, mode_acquisition, mode_integration) %>% 
    rename_all(list(~stringi::stri_trans_general(., "latin-ascii"))) %>% # Pour remplacer les caractères accentués par les mêmes sans accents
    rename_all(list(~paste0("chmesgr_",.))) %>% 
    rename_all(list(~gsub("[[:punct:]]", "_", .))) %>% 
    rename_all(list(~tolower(.)))

  #### Sortie des résultats ####
  return(dataV6)
  
} # Fin de la fonction
