#' Calcul des CPUE de captures
#'
#' Cette fonction permet de calculer les CPUE de captures en grands milieux
#' @name poissons.cpue
#' @param captures Données de captures (format Multifish issu de \code{poissons.captures})
#' @param placettes Données de placettes (format Multifish issu de \code{poissons.placettes})
#' @keywords poissons 
#' @returns \code{contexte_operations}, \code{effort_peche_filets_brut}, \code{effort_peche_filets_synthese}, \code{captures_n}, \code{captures_b}, \code{cpue_n}, \code{cpue_b}
#' @export
#' @import glue
#' @import openxlsx
#' @import tidyverse
#' @importFrom dplyr select
#' @importFrom dplyr setdiff
#' @importFrom dplyr union
#' @examples
#' poissons.cpue(captures, placettes)

poissons.cpue <- function(
  captures,
  placettes
)
{
  
  #### Contexte ####
  # contexte_operations <- operations %>% poissons.contexte()
  n_inventaires_captures <- n_distinct(captures$codeinventaire)
  n_inventaires_placettes <- n_distinct(placettes$codeinventaire)

  #### Test de cohérence ####
  ## Inventaires et opérations ##
  # if(contexte_operations %>% pull(n_inventaires) != 1) stop(glue("Présence de plusieurs inventaires dans le jeu de données : {contexte_operations %>% pull(inventaires)}"))
  if(n_inventaires_captures != 1) stop(glue("Présence de plusieurs inventaires dans le jeu de données de captures : {distinct(captures$codeinventaire)}"))
  if(n_inventaires_placettes != 1) stop(glue("Présence de plusieurs inventaires dans le jeu de données de placettes : {distinct(placettes$codeinventaire)}"))
  
  ## Placettes ##
  # placettes_sans_operation <- placettes %>% filter(!(codeoperation) %in% operations$CodeOperation)
  # if(placettes_sans_operation %>% nrow() != 0) stop(glue("Placettes non rattachées à une opération fournie : {sort(placettes_sans_operation$codeplacette) %>% glue_collapse(sep = ';')}"))
  placettes_sans_surface <- placettes %>% filter(is.na(surfaceprospectee))
  if(placettes_sans_surface %>% nrow() != 0) stop(glue("Placettes sans surface prospectée : {sort(placettes_sans_surface$codeplacette) %>% glue_collapse(sep = ';')}"))
  
  ## Captures ##
  # captures_sans_operation <- captures %>% filter(!(codeoperation) %in% operations$CodeOperation)
  # if(captures_sans_operation %>% nrow() != 0) stop(glue("Captures non rattachées à une opération fournie : {sort(captures_sans_operation$codecapture) %>% glue_collapse(sep = ';')}"))
  captures_sans_placettes <- captures %>% filter(!(codeplacette) %in% placettes$codeplacette)
  if(captures_sans_placettes %>% nrow() != 0) stop(glue("Captures non rattachées à une placette fournie : {sort(captures_sans_placettes$codecapture) %>% glue_collapse(sep = ';')}"))
  captures_sans_effectif <- captures %>% filter(is.na(nombre) | nombre == 0)
  if(captures_sans_effectif %>% nrow() != 0) stop(glue("Captures sans effectif : {sort(captures_sans_effectif$codecapture) %>% glue_collapse(sep = ';')}"))
  captures_sans_biomasse <- captures %>% filter(is.na(poids) | poids == 0)
  if(captures_sans_biomasse %>% nrow() != 0) stop(glue("Captures sans biomasse : {sort(captures_sans_biomasse$codecapture) %>% glue_collapse(sep = ';')}"))
  
  #### Nettoyage & reformatage ####
  ### Réencodage ###
  placettes_v2 <-
    placettes %>%
    mutate(modeechantillonnage = recode(typedefilet,
                                        "Vertical_benthic_net" = "FVL",
                                        "Vertical_pelagic_battery" = "FVS",
                                        "Vertical_pelagic_net" = "FVS",
                                        "Électricité" = "EX1",
                                        "Électricité" = "EX2",
                                        "Électricité" = "EX3",
                                        "electric_fishing_boat" = "EBP",
                                        "Electric_fishing_ECD_boat" = "EBP",
                                        "electric_fishing_foot" = "EPP",
                                        "Electric_fishing" = "EPP",
                                        "CEN_benthic_net" = "CENb",
                                        "CEN_pelagic_net" = "CENp"
    )
    ) %>%
    mutate(methode = recode(modeechantillonnage,
                            "FVL" = "Benthique",
                            "FVS" = "Pélagique",
                            "EX1" = "Électricité",
                            "EX2" = "Électricité",
                            "EX3" = "Électricité",
                            "EBP" = "Électricité",
                            "EPP" = "Électricité",
                            "CENb" = "Benthique",
                            "CENp" = "Pélagique"
    )
    ) %>%
    mutate(norme = recode(modeechantillonnage,
                          "FVL" = "FV",
                          "FVS" = "FV",
                          "EX1" = "FV",
                          "EX2" = "FV",
                          "EX3" = "FV",
                          "EBP" = "FV",
                          "EPP" = "FV",
                          "CENb" = "CEN",
                          "CENp" = "CEN"
    )
    ) %>% 
    mutate(modeechantillonnage = factor(modeechantillonnage, levels = c("CENb","FVL","CENp","FVS","EBP","EPP")))
  
  placettes_sans_exhaustif_v3 <-
    placettes_v2 %>% 
    filter(modeechantillonnage != "EX1") %>% 
    filter(modeechantillonnage != "EX2") %>% 
    filter(modeechantillonnage != "EX3") %>% 
    filter(modeechantillonnage == "CENb" | modeechantillonnage == "CENp" | modeechantillonnage == "EBP" | modeechantillonnage == "EPP" | modeechantillonnage == "FVL" | modeechantillonnage == "FVS")
  
  #### Regroupement ####
  ### Nombre de captures par type de prospection par lac par profondeur ###
  ## Regroupement des données ##
  synthese_captures <- 
    captures %>% 
    left_join(placettes_sans_exhaustif_v3, by="codeplacette") %>% 
    filter(!is.na(codeplacette)) %>% 
    mutate(annee = year(datedebut))
  
  #### Calcul ####
  ### Synthèse des surfaces pêchantes ###
  # Calcul #
  effort_peche_filets_brut <-
    placettes_v2 %>% 
    # filter(methode != "Électricité") %>%
    mutate(annee = year(dateheurereleve)) %>% 
    group_by(annee, norme, methode) %>%
    summarise(
      nombre = n(),
      surface = sum(surfacefiletplacette)
    ) %>%
    ungroup()
  
  # Synthèse #
  effort_peche_filets_synthese <-
    effort_peche_filets_brut %>% 
    # filter(annee == annee_la_plus_recente) %>% 
    pivot_longer(cols = c(nombre, surface), names_to = "Type", values_to = "Valeur") %>% 
    # select(methode, annee, Type, norme, Valeur) %>% 
    pivot_wider(names_from = c(annee, Type, norme), values_from = Valeur) %>% 
    relocate(c(ncol(.)-1), .before = c(ncol(.)-2))
  
  ### Calcul des résultats (biomasse et abondance) bruts ###
  ## Calcul des captures brutes ##
  captures_n <-
    synthese_captures %>%
    select(nom.x, annee, codeespece, modeechantillonnage, nombre) %>%
    pivot_wider(names_from = "modeechantillonnage", values_from = "nombre", values_fn = sum, names_sort = T) %>%
    rename(Lac = nom.x) %>%
    # select(-Lac, -annee) %>%
    mutate(annee = as.character(annee)) %>% 
    bind_rows(summarise(., across(where(is.numeric), ~ sum(., na.rm = T)),
                        across(where(is.character), ~ 'Total'))) %>%
    rowwise() %>%
    mutate(Total = sum(c_across(where(is.numeric)), na.rm = T)) %>% 
    ungroup()# %>% 
  # mutate(annee = as.numeric(annee))
  
  ## Calcul des biomasses brutes ##
  captures_b <-
    synthese_captures %>%
    mutate(poids = poids/1000) %>% # passage en kg
    select(nom.x, annee, codeespece, modeechantillonnage, poids) %>%
    pivot_wider(names_from = "modeechantillonnage", values_from = "poids", values_fn = sum, names_sort = T) %>%
    filter(annee != "(all)") %>%
    rename(Lac = nom.x) %>%
    # select(-Lac, -annee) %>%
    mutate(annee = as.character(annee)) %>% 
    bind_rows(summarise(., across(where(is.numeric), ~ sum(., na.rm = T)),
                        across(where(is.character), ~ 'Total'))) %>%
    rowwise() %>%
    mutate(Total = sum(c_across(where(is.numeric)), na.rm = T)) %>% 
    ungroup()# %>% 
  # mutate(annee = as.numeric(annee))
  
  ### Calcul des rendements de capture - CPUE ###
  ## Calcul de la seule surface de placettes par méthode ##
  surface_placettes <-
    placettes_sans_exhaustif_v3 %>%
    select(modeechantillonnage, surfaceprospectee) %>% 
    pivot_wider(names_from = "modeechantillonnage", values_from = "surfaceprospectee", values_fn = sum, names_sort = T)
  
  ## CPUE numériques ##
  cpue_n <-
    captures_n %>%
    select(-Total) %>%
    filter(codeespece != "Total") %>%
    full_join(surface_placettes) %>% # fusion avec les surfaces pêchantes en bas
    mutate(across(where(is.numeric), ~ round(.*1000/last(.), 2))) %>%  # on divise tout par la dernière ligne en rapportant ça à 1000 m2
    filter(row_number() != n())

  ## CPUE biomasse ##
  cpue_b <-
    captures_b %>%
    select(-Total) %>%
    filter(codeespece != "Total") %>%
    full_join(surface_placettes) %>% # fusion avec les surfaces pêchantes en bas
    mutate(across(where(is.numeric), ~ round(.*1000/last(.), 2))) %>%  # on divise tout par la dernière ligne en rapportant ça à 1000 m2
    filter(row_number() != n())
  
  #### Sortie ####
  sortie <- list(#contexte_operations = contexte_operations, 
                 effort_peche_filets_brut = effort_peche_filets_brut,
                 effort_peche_filets_synthese = effort_peche_filets_synthese,
                 captures_n = captures_n,
                 captures_b = captures_b,
                 cpue_n = cpue_n,
                 cpue_b = cpue_b
                 )
  
  return(sortie)
  
} # Fin de la fonction
