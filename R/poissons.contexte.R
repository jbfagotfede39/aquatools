#' contexte de données piscicoles
#'
#' Cette fonction permet d'établir le contexte de données piscicoles
#' @name poissons.contexte
#' @param data Data.frame issu de la fonction \code{poissons.operations}
#' @keywords poissons
#' @import glue
#' @import tidyverse
#' @export
#' @examples
#' poissons.operations(data.frame(Nom = "SOR10-2"), CodeOperation = TRUE, Sortie = "Complet") %>% poissons.contexte()

poissons.contexte <- function(
  data = data
)
{

  #### Nettoyage & reformatage ####
  ### Homogénéisation des noms de champs ###
  datarenomees <-
    data %>% 
    # Dates
    rename_at(vars(contains("Date")), list( ~ str_replace(., "Date", "date"))) %>% 
    # Stations
    rename_at(vars(contains("Station")), list( ~ str_replace(., "Station", "station"))) %>% 
    # Opérations
    rename_at(vars(contains("CodeOperation")), list( ~ str_replace(., "CodeOperation", "codeoperation")))
  
  #### Création des données manquantes ####
  datacompletees <-
    datarenomees #%>% 
    # {if("coderhj" %in% colnames(.) == FALSE) mutate(., coderhj = NA_character_) else .} %>% 
    # {if("mo" %in% colnames(.) == FALSE) mutate(., mo = NA_character_) else .} %>% 
    # {if("typemesure" %in% colnames(.) == FALSE) mutate(., typemesure = NA_character_) else .} %>% 
    # {if("unite" %in% colnames(.) == FALSE) mutate(., unite = NA_character_) else .} %>% 
    # {if("annee" %in% colnames(.) == FALSE) mutate(., annee = NA_character_) else .} %>% 
    # {if("milieu" %in% colnames(.) == FALSE) mutate(., milieu = NA_character_) else .}
    
  #### Calcul des indicateurs numériques ####
  contexte <- 
    tibble(n_stations = n_distinct(datacompletees$station)) %>% 
    add_column(n_dates = n_distinct(datacompletees$date)) %>% 
    add_column(n_operations = n_distinct(datacompletees$codeoperation)) %>% 
    add_column(n_inventaires = n_distinct(datacompletees$codeinventaire)) #%>% 
    # add_column(nmo = n_distinct(datacompletees$mo)) %>% 
    # add_column(ntypemesure = n_distinct(datacompletees$typemesure)) %>% 
    # add_column(nunite = n_distinct(datacompletees$unite, na.rm = T)) %>% 
    # add_column(nannee = n_distinct(datacompletees$annee)) %>% 
    # add_column(nmilieu = n_distinct(datacompletees$milieu, na.rm = T))
  
  #### Extraction sous forme de liste ####
  contexte <- 
    contexte %>% 
    add_column(stations = unique(datacompletees$station) %>% glue_collapse(sep = ";")) %>% 
    add_column(dates = unique(datacompletees$date) %>% glue_collapse(sep = ";")) %>% 
    add_column(operations = unique(datacompletees$codeoperation) %>% glue_collapse(sep = ";")) %>% 
    add_column(inventaires = unique(datacompletees$codeinventaire) %>% glue_collapse(sep = ";")) #%>% 
    # add_column(mo = unique(datacompletees$mo) %>% glue_collapse(., sep = ", ", last = " et ")) %>%
    # add_column(typemesure = unique(datacompletees$typemesure) %>% glue_collapse(., sep = ";")) %>%
    # add_column(unite = unique(datacompletees$unite) %>% glue_collapse(., sep = ";")) %>% 
    # add_column(annee = unique(datacompletees$annee) %>% glue_collapse(., sep = ";")) %>% 
    # add_column(milieu = unique(datacompletees$milieu) %>% glue_collapse(., sep = ";"))
 
   #### Sortie ####
  return(contexte)
} 
