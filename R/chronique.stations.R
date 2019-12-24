#' Listage des stations de chroniques
#'
#' Cette fonction permet de lister les stations de la BDD Chroniques
#' @name chronique.stations
#' @param Territoire Territoire concerné. Unique ou sous forme de vecteur (c("Villerserine", "Villers-Robert") par exemple)
#' @param Echelle Échelle spatiale de la synthèse (commune, canton, communauté de communes, département, région, contexte de PDPG, Hydro-écorégion, entité GEMAPI, Maître d'ouvrage, Milieu, Bassin versant, Sous-bassin versant, Polygone autre)
#' @param Sortie Forme du dataframe de sortie - \code{Simple} (par défault), \code{Propre} (format diffusable, avec stations) ou \code{Complet} (tous les champs)
#' @keywords stations
#' @import dplyr
#' @import sf
#' @export
#' @examples
#' chronique.stations("CD39","MO")
#' chronique.stations("Suran","Milieu")
#' chronique.stations("Ain","Bassin")
#' chronique.stations("Serpentine","Sous-bassin")
#' chronique.stations("Lons-le-Saunier","Commune")
#' chronique.stations("39","Département")

##### TODO LIST #####
# 
#####################

chronique.stations <- function(Territoire = NA_character_, 
                               Echelle = c("Commune", "Canton", "ComCom", "Département", "Région", "ContextePDPG", "HER", "GEMAPI", "MO", "Milieu", "Bassin", "Sous-bassin", "Polygone"),
                               Sortie = c("Complet","Propre","Simple")
)
{
  
  ## Évaluation des choix
  Echelle <- match.arg(Echelle)
  Sortie <- match.arg(Sortie)
  
  #### Chargement des données ####
  dbD <- BDD.ouverture("Data")
  Stations <- 
    sf::st_read(dbD, query = "SELECT * FROM fd_production.chroniques_stations;") %>% 
    arrange(chsta_coderhj)
  DBI::dbDisconnect(dbD)
  
  #### Filtrage ####
  if(Echelle == "MO"){
    Vue <-
    Stations %>% 
    filter(grepl(Territoire, chsta_mo)) %>% 
    arrange(chsta_coderhj)
  }
  
  if(Echelle == "Milieu"){
    Vue <-
    Stations %>% 
    filter(chsta_milieu == Territoire) %>% 
    arrange(chsta_coderhj)
  }
  
  if(Echelle == "Bassin" | Echelle == "Sous-bassin" | Echelle == "Commune" | Echelle == "Canton" | Echelle == "ComCom" | Echelle == "Département" | Echelle == "Région" | Echelle == "ContextePDPG" | Echelle == "HER" | Echelle == "GEMAPI" | Echelle == "Polygone"){ 
    TerritoireRecherche <- stations.territoire(Echelle = Echelle, Territoire = Territoire)
    Vue <-
    Stations %>% 
    st_join(TerritoireRecherche, left = FALSE) %>% 
    arrange(chsta_coderhj)
  }
    
  # if(Echelle == "Bassin" | Echelle == "Sous-bassin" | Echelle == "Commune" | Echelle == "Canton" | Echelle == "ComCom" | Echelle == "Département" | Echelle == "Région" | Echelle == "ContextePDPG" | Echelle == "HER" | Echelle == "GEMAPI") 
  #   TerritoireRecherche <- stations.territoire(Echelle = Echelle, Territoire = Territoire)
  #   Vue <-
  #   Stations %>% 
  #   st_join(TerritoireRecherche, left = FALSE) %>% 
  #   arrange(chsta_coderhj)
  
  ## Simplification ##
  if(Sortie == "Propre"){
    Vue <- 
      Vue %>% 
      rename_at(vars(matches("id.x")), list(~str_replace(., "id.x", "id"))) %>%
      select(id:chsta_milieu, chsta_commune, chsta_departement, chsta_coord_x:chsta_reseauthermietype)
    }
  
  ## Affichage des résultats ##
  return(Vue)
}