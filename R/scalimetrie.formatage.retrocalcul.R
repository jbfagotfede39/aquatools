#' Mise en forme des mesures de scalimétrie pour rétro-calcul avec RFishBC
#'
#' Cette fonction permet de mettre en forme les données de scalimétrie en prévision d'un rétro-calcul d'âge avec RFishBC
#' @name scalimetrie.formatage.retrocalcul
#' @param lectures Lectures des âges à traiter
#' @param mesures Mesures des stries de croissance à traiter
#' @keywords scalimétrie poissons
#' @import tidyverse 
#' @export
#' @examples
#' lectures %>% scalimetrie.formatage.retrocalcul(mesures)

scalimetrie.formatage.retrocalcul <- function(
    lectures = NA_character_,
    mesures = NA_character_
)
{
  
  #### Test de cohérence ####
  # if(is.na(lectures)) stop("Des lectures d'âge doivent être fournies")
  # if(is.na(mesures)) stop("Les mesures des stries de croissance doivent être fournies")
  
  #### Nettoyage & reformatage ####
  data_sortie <-
    lectures %>% 
    rename(id_poissons = num_individu) %>% 
    # rename(lencap = taille) %>% 
    # rename(agecap = age) %>% 
    # select(id:agecap) %>% 
    select(id:age) %>% 
    select(-sexe) %>% 
    left_join(mesures %>% 
                # select(id, id_poissons, Length_μm) %>% 
                # select(id, Length_μm) %>% 
                select(id:Length_μm) %>% 
                select(-contains("id_poissons")) %>% 
                mutate(id = as.numeric(id)), 
              join_by(id))
  
  data_sortie_v2 <-
    data_sortie %>% 
    # Calcul de la distance réelle entre le centre de l'écaille et le marquage
    group_by(id) %>%
    mutate(
      Length_cumul_μm = if_else(
        row_number() == 1,
        Length_μm, # La première ligne reste inchangée
        cumsum(Length_μm) # Calcul cumulatif de Length_μm
      )
    ) %>%
    ungroup() %>%
    select(-Length_μm) %>% 
    # Ajout de suffixe dans les valeurs de la colonne "type" pour le format large
    group_by(id) %>%
    mutate(type = paste0(type, "_position_", row_number())) %>% # Ajouter les positions
    ungroup() %>% 
    # Transformation en format large et ajout de la colonne "total"
    pivot_wider(
      names_from = type, # Utiliser les valeurs de `type` comme noms de colonnes
      values_from = Length_cumul_μm # Utiliser les valeurs de `Length_cumul_μm` comme contenu
    ) %>%
    mutate(
      total_length_μm = rowSums(across(starts_with("mesures")), na.rm = TRUE) # Somme des colonnes "mesures"
    ) %>%
    select(id, contains("id_poissons"), espece, taille, poids, age, total_length_μm, starts_with("mesures")) %>% 
    # Changement des noms de colonnes
    rename_with(
      .cols = starts_with("mesures_position"),
      .fn = ~ gsub("mesures_position_(\\d+)", "rad\\1", .x)
    ) %>%
    rename(radcap = total_length_μm, 
           agecap = age, 
           lencap = taille) 
  
  data_sortie_v3 <- 
    data_sortie_v2 %>% 
    filter(!is.na(agecap)) %>% 
    arrange(lencap)
  
  #### Sortie ####
  return(data_sortie_v3)
  
} # Fin de la fonction