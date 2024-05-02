#' Calcul d'un profil topographique le long d'un transect
#'
#' Cette fonction permet de calculer le profil topographique ou autres le long d'un transect à partir d'un nuage de points
#' @name topographie.profil
#' @param leves Nuage de points issu de `topographie.mesures`
#' @param transects Dataframe au format sf contenant 2 points ordonnées (départ -> arrivée) ou bien une LINESTRING (cours d'eau par exemple)
#' @param buffer Distance maximale entre les points et le transect
#' @keywords topographie
#' @import sf
#' @import tidyverse
#' @export 
#' @examples
#' 
#' topographie.profil(leves_1, transects, 150)

topographie.profil <- function(
  leves = NA_character_,
  transects = NA,
  buffer = 10
  )
{

  #### Nettoyage & reformatage ###
  ### Importation/création du transect ###
  transects_1 <- 
    transects %>% 
    topographie.transect()
  
  id_transect <- unique(transects_1$id)
  
  #### Filtrage ####
  leves_2 <-
    leves %>% 
    st_filter(st_buffer(transects_1, buffer))
  
  #### Calcul ####
  ### Calcul des extrémités du transect ###
  extremites_transect <-
    # Extraction des coordonnées des premier et dernier point du transect
    transects_1 %>% 
    st_line_sample(sample=c(0,1)) %>%
    st_cast("POINT") %>%
    st_as_sf() %>% 
    mutate(tplv_coord_x = st_coordinates(.)[,1]) %>%
    mutate(tplv_coord_y = st_coordinates(.)[,2])
  
  ### Calcul des distances à l'origine sur le transect ###
  points_projetes_sur_transect <-
    leves_2 %>% 
    st_nearest_points(transects_1) %>% # On obtient des LINESTRING avec le point de départ et d'arrivée de la plus courte distance entre chaque point et le transect
    cbind(leves_2 %>% 
            # select(tplv_coderhj, geom) %>%
            dplyr::select(id, geom) %>%
            mutate(tplv_coord_x = st_coordinates(.)[,1]) %>%
            mutate(tplv_coord_y = st_coordinates(.)[,2]) %>%
            mutate(tplv_coord_z = st_coordinates(.)[,3]) %>%
            st_drop_geometry()
    ) %>% 
    st_as_sf() %>% 
    st_cast("POINT") %>% 
    mutate(point_projete_coord_x = st_coordinates(.)[,1]) %>% 
    mutate(point_projete_coord_y = st_coordinates(.)[,2]) %>%
    filter((tplv_coord_x != point_projete_coord_x) | tplv_coord_x %in% extremites_transect$tplv_coord_x) %>% # Car on souhaite également conserver le premier/dernier, pour calculer leur distance à l'origine pour les situer sur les graphiques ensuite
    # Nettoyage d'un doublon du premier/dernier point du transect
    rownames_to_column() %>% 
    filter(grepl(".1", rowname)) %>% 
    dplyr::select(-rowname) %>% 
    # Calcul de la distance entre le point initial et le point projeté
    mutate(id_transect = id_transect, .after = "id") %>% 
    # Calcul de la distance entre le point initial et le point projeté
    bind_cols(., 
              st_distance(., leves_2, by_element = T) %>%
                as_tibble() %>% 
                rename(distance_reel_projete = value)
    ) %>% 
    units::drop_units() %>% 
    # Calcul de la distance depuis le point de départ du transect
    bind_cols(., 
              st_distance(., extremites_transect %>% 
                            filter(row_number() == 1)) %>% # On calcule par rapport à la première extrémité du transect
                as_tibble() %>% 
                rename(dist_cum = value)
    ) %>% 
    units::drop_units()

  #### Sortie ####
  return(points_projetes_sur_transect)

} # Fin de la fonction