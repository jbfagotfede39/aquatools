#' Création d'un transect
#'
#' Cette fonction permet de créer un transect à partir de deux points
#' @name topographie.transect
#' @param data Dataframe contenant deux points - L'ordre de tri des points constituera le sens du transect
#' @param epsg EPSG à attribuer au transect
#' @keywords topographie
#' @import glue
#' @import magrittr
#' @import sf
#' @import tidyverse
#' @export 
#' @examples
#' data %>% topographie.transect()

topographie.transect <- function(
  data = NA_character_
)
{
  #### Tests ####
  if(!(any(class(data) %in% c("sf")) == TRUE)) stop("Jeu de données en entrée pas au format sf")

  #### Calcul du transect ####
  existence_transect <- FALSE

  if(nrow(data) == 1 & length(unique(st_geometry_type(data))) == 1 & unique(st_geometry_type(data)) %in% c("LINESTRING", "MULTILINESTRING")){
    # On ne touche pas à transect car c'est déjà une ligne
    transect <- data
    existence_transect <- TRUE
  }
  
  if(nrow(data) == 1 & length(unique(st_geometry_type(data))) == 1 & unique(st_geometry_type(data)) %in% c("POINT")){
    # On créé deux objets spatiaux points à partir de deux points présents sous la forme d'une seule ligne d'objet spatial point
    epsg <- st_crs(data) # Extraction du crs à partir des données en entrée
    
    data <-
      data %>% 
      st_drop_geometry() %>%
      dplyr::select(contains("id"), contains("_coord_x"), contains("_coord_y"))
    if(ncol(data) != 4) stop("Impossible de créer un transect à partir des deux points contenus dans une seule ligne en entrée")
    
    data <-
      data %>% 
        matrix(ncol = 2) %>%
        apply(2, as.numeric) %>% 
        data.frame() %>%
        as_tibble() %>%
        sf::st_as_sf(coords = c("X1","X2"), remove = FALSE) %>%
        st_set_crs(epsg)
    
    # Ces points sont ensuite envoyés dans le bloc suivant pour création d'un transect
  }
  
  if(nrow(data) == 2 & length(unique(st_geometry_type(data))) == 1 & unique(st_geometry_type(data)) == "POINT"){
    epsg <- st_crs(data) # Extraction du crs à partir des données en entrée
    transect <- 
      data %>% 
      st_coordinates() %>% 
      as.matrix() %>% 
      st_linestring() %>% 
      st_sfc(crs = epsg) %>%
      st_sf()
    if("id" %in% names(data)) transect <- transect %>% bind_cols(data %>% distinct(id))
    existence_transect <- TRUE
  }
  
  #### Tests de fin ####
  if(existence_transect == FALSE) stop("Attention le transect n'est pas opérationnel - Fonction ou données en entrée à vérifier")
  
  #### Sortie des résultats ####
  return(transect)
  
} # Fin de la fonction