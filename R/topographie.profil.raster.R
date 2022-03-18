#' Calcul d'un profil raster le long d'un transect
#'
#' Cette fonction permet de calculer le profil topographique ou autres le long d'un transect à partir d'un raster (MNT, etc.)
#' @name topographie.profil.raster
#' @param raster Raster déjà importé et éventuellement regroupé ou localisation relative du raster
#' @param epsg EPSG du raster (\code{2154} par défaut)
#' @param transect Dataframe au format sf contenant 2 points ordonnées (départ -> arrivée) ou bien une LINESTRING (cours d'eau par exemple)
#' @param points_projetes Dataframe au format sf contenant les points que l'on souhaite projeter sur le profil
#' @param points_projetes_position Position souhaitée pour l'affichage des points remarquables : \code{lissee} (par défaut) : sur une valeur unique, \code{intermediaire} ou \code{reelle}
#' @keywords topographie
#' @import glue
#' @import raster
#' @import sf
#' @import tidyverse
#' @export 
#' @examples
#' topographie.profil.raster(raster = mon_raster_regroupe, transect = StationsTransect, points_projetes = Stations, points_projetes_position = "intermediaire") %>% magrittr::extract2(2)
#' topographie.profil.raster(raster = "Downloads/RGEALTI_FXX_0895_6595_MNT_LAMB93_IGN69.asc", transect = StationsTransect, points_projetes = Stations, points_projetes_position = "intermediaire") %>% magrittr::extract2(2)

topographie.profil.raster <- function(
  raster = NA_character_,
  epsg = 2154,
  transect = NA,
  points_projetes = tibble(var_name_1 = numeric()),
  points_projetes_position = c("lissee", "intermediaire", "reelle")
  )
{
  
  #### Importation du raster ####
  ### Importation du raster si nécessaire ###
  if(length(str_subset(raster, ".asc$")) != 0){ # Sinon le raster est déjà importé en tant que tel, ce n'est pas une adresse de fichier
  if(is.na(raster)) stop("Pas de localisation de raster spécifiée")
  fichierraster <- adresse.switch(raster)
  raster <- raster(fichierraster)
  }
  
  ### Test qu'on a bien un raster ###
  if(class(raster)[1] != "RasterLayer") stop("Le raster en entrée n'en est pas un après le pré-traitement")
  
  ### Transformation du raster ###
  if(epsg != 2154) stop(glue("EPSG {epsg} différent de 2154 : cas à développer"))
  if(epsg == 2154) projection <- "+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" # issu de https://epsg.io/2154 -> PROJ.4
  crs(raster) <- projection
  
  #### Importation/création du transect ####
  transect <- 
    transect %>% 
    topographie.transect()

  #### Calcul des valeurs du raster le long du transect ####
  extractionrasterbrute <- raster::extract(raster, transect, 
                                        along = TRUE, cellnumbers = TRUE)
  
  ### Vérification de l'emprise du transect par rapport à celle du raster ###
  if(is.null(extractionrasterbrute[[1]])) stop("L'emprise du raster est différente de celle du transect")
  
  ### Calcul à proprement parler ###
  extractionrasterbrute_df <- purrr::map_dfr(extractionrasterbrute, as_tibble, .id = "ID")
  transect_coords <- xyFromCell(raster, extractionrasterbrute_df$cell) %>% as_tibble()
  jointureAltCoord <- 
    bind_cols(extractionrasterbrute_df, transect_coords) %>% 
    sf::st_as_sf(coords = c("x","y"), remove = FALSE) %>% 
    st_set_crs(2154)
  
  names(jointureAltCoord)[3] <- c("valeur_raster")
    
  distance_valeur_raster <-
    jointureAltCoord %>%
    bind_cols(
      st_distance(jointureAltCoord) %>%
        as_tibble() %>%
        dplyr::select(V1)
    ) %>%
    rename(dist_cum = V1) # On a ici la distance entre le premier point et les suivants, donc pas besoin de cumuler
  
  distance_valeur_raster <- 
    distance_valeur_raster %>% 
    units::drop_units()
  
  #### Projection des points début/fin et intermédiaires sur le transect et le profil ####
  
  ### Calcul des extrémités du transect ###
  if(nrow(points_projetes) != 0){
  ## Vérifications ##
  if(!(all(class(points_projetes) %in% c("sf", "data.frame")) == TRUE)) stop("Paramètre points_projetes pas au format sf")
  ## Calcul ##
  extremites_transect <-
    # Extraction des coordonnées des premier et dernier point du transect
    st_line_sample(transect,sample=c(0,1)) %>%
    st_cast("POINT") %>%
    st_as_sf() %>% 
    mutate(chsta_coord_x = st_coordinates(.)[,1]) %>%
    mutate(chsta_coord_y = st_coordinates(.)[,2]) #%>%
    # st_drop_geometry()
  
  ### Calcul des distances à l'origine sur le transect ###
  points_projetes_sur_transect <-
    points_projetes %>% 
    st_nearest_points(transect) %>% # On obtient des LINESTRING avec le point de départ et d'arrivée de la plus courte distance entre chaque point et le transect
    cbind(points_projetes %>% 
            # select(chsta_coderhj, geom) %>%
            dplyr::select(contains("coderhj"), geom) %>%
            mutate(chsta_coord_x = st_coordinates(.)[,1]) %>%
            mutate(chsta_coord_y = st_coordinates(.)[,2]) %>%
            st_drop_geometry()
    ) %>% 
    st_as_sf() %>% 
    st_cast("POINT") %>% 
    mutate(point_projete_coord_x = st_coordinates(.)[,1]) %>% 
    mutate(point_projete_coord_y = st_coordinates(.)[,2]) %>% ## test
    filter((chsta_coord_x != point_projete_coord_x) | chsta_coord_x %in% extremites_transect$chsta_coord_x) %>% # Car on souhaite également conserver le premier/dernier, pour calculer leur distance à l'origine pour les situer sur les graphiques ensuite
    # Nettoyage d'un doublon du premier/dernier point du transect
    rownames_to_column() %>% 
    filter(grepl(".1", rowname)) %>% 
    dplyr::select(-rowname) %>% 
    # Calcul de la distance depuis le point de départ du transect
    bind_cols(., 
              st_distance(., extremites_transect %>% 
                            filter(row_number() == 1)) %>% # On calcule par rapport à la première extrémité du transect
                as_tibble() %>% 
                rename(dist_cum = value)
    ) %>% 
    units::drop_units()
  
  ### Calcul des valeurs du raster de l'ensemble des points remarquables ###
  points_remarquables <-
    points_projetes_sur_transect %>% 
    bind_cols(raster::extract(raster, points_projetes_sur_transect, 
                              cellnumbers = TRUE) %>% 
                as_tibble() %>% 
                dplyr::select(-cells)
    ) %>% 
    rename(valeur_raster = layer)
  
  # names(points_remarquables)[6] <- c("valeur_raster")
  
  ### Calcul de la position à afficher et formatage du type de point ###
  points_remarquables <-
    points_remarquables %>% 
    mutate(type = ifelse(chsta_coord_x %in% extremites_transect$chsta_coord_x, "Extrémité", "Projeté")) %>% 
    {if(points_projetes_position == "reelle") mutate(., valeur_raster_etiquette = valeur_raster) else .} %>% 
    {if(points_projetes_position == "intermediaire") mutate(., valeur_raster_etiquette = valeur_raster + (max(.$valeur_raster) - min(.$valeur_raster))*0.2) else .} %>% 
    {if(points_projetes_position == "lissee") mutate(., valeur_raster_etiquette = (max(.$valeur_raster) + (max(.$valeur_raster) - min(.$valeur_raster))*0.1) ) else .}
  }
  
  #### Sortie des données ####
  if(nrow(points_projetes) == 0) return(list(distance_valeur_raster))
  if(nrow(points_projetes) != 0) return(list(distance_valeur_raster, points_remarquables))
  
} # Fin de la fonction