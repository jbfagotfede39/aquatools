#' Jointure des territoires SDDLP aux SAPL
#'
#' Cette fonction permet d'associer le territoire au sens du SDDLP à chaque SAPL
#' @name sddlp.territoires
#' @param data Jeu de données contenant une colonne plosapl_sapl
#' @keywords sddlp
#' @import sf
#' @import tidyverse
#' @export
#' @examples
#' sddlp.territoires(data)
#' sapl <- sf::st_read(dbD, query = "select * from fd_production.pecheloisir_sapl;") %>%  sddlp.territoires()

sddlp.territoires <- function(data)
  {
  
#### Collecte des données ####
dbD <- BDD.ouverture("Data")
Territoires <- sf::st_read(dbD, query = "select * from fd_production.sddlp_territoires;")

#### Jointure des tables ####
SaplTerritoires <-
  st_join(data, Territoires) %>% 
  mutate(sddlpterrit_libelle = ifelse(plosapl_sapl == "La Truite de l'Ain", "Rivière d'Ain - Région des Lacs", sddlpterrit_libelle)) %>% 
  rename(id = id.x) %>% 
  select("id", "plosapl_sapl", "plosapl_nom_usage", "sddlpterrit_libelle", "plosapl_type", "plosapl_agrement", "plosapl_siege_ville", "plosapl_insee", 
         "plosapl_siege_numero_voie", "plosapl_siege_libelle_voie", "plosapl_siege_codepostal", "plosapl_coord_x", "plosapl_coord_y",
         "plosapl_coord_type", "plosapl_activite") 

#### Sortie des données ####
return(SaplTerritoires)

} # Fin de la fonction
