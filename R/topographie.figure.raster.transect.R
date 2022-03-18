#' Vue d'un transect sur un raster
#'
#' Cette fonction permet de représenter un transect sur un raster
#' @name topographie.figure.raster.transect
#' @param raster Raster sur lequel on souhaite représenter le transect
#' @param transect Transect à représenter, en sortie de topographie.transect()
#' @param points_projetes Points d'intérêt à projeter le long du transect
#' @param lignes_projetees_sur_transect Transects de projection des points à représenter, en sortie de topographie.transect()
#' @param etiquette Affichage des étiquettes des points d'intérêt : \code{TRUE} (par défaut) ou \code{FALSE}
#' @param angle_etiquette Angle de rotation à appliquer aux étiquettes (0 par défaut)
#' @param position_fleche_nord Position de la flèche du nord (\code{"bl"} par défaut)
#' @param facteur_largeur Facteur d'extension du raster en largeur par rapport aux données (0.4 par défaut)
#' @param facteur_hauteur Facteur d'extension du raster en hauteur par rapport aux données (0.4 par défaut)
#' @param valeur_min Valeur minimale à représenter sur le raster et dans la légende (valeur minimale du raster par défaut)
#' @param valeur_max Valeur maximale à représenter sur le raster et dans la légende (valeur maximale du raster par défaut)
#' @param save Si \code{FALSE} (par défaut), n'enregistre pas les figures. Si \code{TRUE}, les enregistre.
#' @keywords topographie
#' @import ggspatial
#' @import glue
#' @import raster
#' @import sf
#' @import tidyverse
#' @export 
#' @examples
#' topographie.figure.raster.transect(raster, transect)
#' topographie.figure.raster.transect(raster, transect, points_projetes)
#' topographie.figure.raster.transect(raster, transect, points_projetes, lignes_projetees_sur_transect)
#' topographie.figure.raster.transect(raster, transect, points_projetes, lignes_projetees_sur_transect, facteur_largeur = 0.75, facteur_hauteur = 0.3)
#' topographie.figure.raster.transect(raster, transect, points_projetes, lignes_projetees_sur_transect, save = T)

topographie.figure.raster.transect <- function(
  raster = NA_character_,
  transect = NA_character_,
  points_projetes = NA_character_,
  lignes_projetees_sur_transect = NA_character_,
  etiquette = T,
  angle_etiquette = 0,
  position_fleche_nord = c("bl", "tl", "tr", "br"),
  facteur_largeur = 0.4,
  facteur_hauteur = 0.4,
  valeur_min = NA_real_,
  valeur_max = NA_real_,
  save = F
)
{

  ### Test format de données ###
  if(!(any(class(raster) %in% c("RasterLayer")) == TRUE)) stop("Paramètre raster pas au format RasterLayer")
  if(!(any(class(transect) %in% c("sf")) == TRUE)) stop("Paramètre transect pas au format sf")
  if(length(points_projetes) != 1){
    if(!(any(class(points_projetes) %in% c("sf")) == TRUE)) stop("Paramètre points_projetes pas au format sf")}
  if(length(lignes_projetees_sur_transect) != 1){
    if(!(any(class(lignes_projetees_sur_transect) %in% c("sf")) == TRUE)) stop("Paramètre lignes_projetees_sur_transect pas au format sf")}
  if(class(valeur_min) != "numeric") stop("valeur_min pas au format numérique")
  if(class(valeur_max) != "numeric") stop("valeur_max pas au format numérique")
  
  ### Évaluation des choix
  position_fleche_nord <- match.arg(position_fleche_nord)
  
  ### Découpage du raster directement en fonction du transect ###
  ## Définition de la nouvelle enveloppe ##
  if(length(points_projetes) != 1){
    extensionenveloppex <- (st_bbox(points_projetes)[3] - st_bbox(points_projetes)[1])*facteur_largeur
    extensionenveloppey <- (st_bbox(points_projetes)[4] - st_bbox(points_projetes)[2])*facteur_hauteur
    nouvelle_enveloppe <- extent(st_bbox(points_projetes)[1] - extensionenveloppex, st_bbox(points_projetes)[3] + extensionenveloppex, st_bbox(points_projetes)[2] - extensionenveloppey, st_bbox(points_projetes)[4] + extensionenveloppey)
  }
  
  if(length(points_projetes) == 1){
    extensionenveloppex <- (st_bbox(transect)[3] - st_bbox(transect)[1])*facteur_largeur
    extensionenveloppey <- (st_bbox(transect)[4] - st_bbox(transect)[2])*facteur_hauteur
    nouvelle_enveloppe <- extent(st_bbox(transect)[1] - extensionenveloppex, st_bbox(transect)[3] + extensionenveloppex, st_bbox(transect)[2] - extensionenveloppey, st_bbox(transect)[4] + extensionenveloppey)
  }
  
  ## Découpage avec une enveloppe plus large ##
  raster_decoupe <-
    crop(raster, nouvelle_enveloppe)
  
  # Il faut mettre le raster en dataframe
  raster_pts <- rasterToPoints(raster_decoupe, spatial = TRUE) # First, to a SpatialPointsDataFrame
  raster_df  <- data.frame(raster_pts) # Then to a 'conventional' dataframe
  names(raster_df)[1] <- c("valeur_raster")
  
  # Calcul des bornes de valeurs
  if(is.na(valeur_min)) valeur_min <- min(raster_df$valeur_raster)
  if(is.na(valeur_max)) valeur_max <- max(raster_df$valeur_raster)
  
  ### Vue transect sur raster ###
  ggplot <- ggplot() + geom_raster(data = raster_df , aes(x = x, y = y,
                                                              fill = valeur_raster))
  ggplot <- ggplot + geom_sf(data = transect, color = "red", fill = NA)
  if(!is.null(nrow(lignes_projetees_sur_transect))) ggplot <- ggplot + geom_sf(data = lignes_projetees_sur_transect, color = "blue", fill = NA, linetype = "dashed")
  if(length(points_projetes) != 1) ggplot <- ggplot + geom_sf(data = points_projetes)
  if(length(points_projetes) != 1 & etiquette == T) ggplot <- ggplot + geom_text_repel(data = points_projetes, aes(x = chsta_coord_x, y = chsta_coord_y, label = chsta_coderhj, angle = angle_etiquette), size = 3)
  ggplot <- ggplot + scale_fill_gradientn(name = "Altitude (NGF)", colors = terrain.colors(10), limits = c(valeur_min, valeur_max), na.value="transparent")
  # ggplot <- ggplot + coord_sf(crs = 2154, datum = sf::st_crs(2154)) # Pour afficher les axes en Lambert 93
  ggplot <- ggplot + ggspatial::annotation_north_arrow(
    location = position_fleche_nord,
    height = unit(0.75, "cm"),
    width = unit(0.75, "cm"),
    which_north = "true", #grid pour le haut simple, true pour le nord véritable
    # pad_x = unit(0.75, "in"),
    pad_y = unit(1.5, "cm"))#,
  # style = north_arrow_fancy_orienteering)
  ggplot <- ggplot + theme_classic()
  ggplot <- ggplot + theme(legend.position="left")
  ggplot <- ggplot + theme(axis.line.x = element_blank(),
                                   axis.line.y = element_blank(),
                                   axis.ticks.x = element_blank(),
                                   axis.ticks.y = element_blank(),
                                   axis.title.x = element_blank(),
                                   axis.title.y = element_blank(),
                                   axis.text.x  = element_blank(),
                                   axis.text.y = element_blank()
  )
  
  #### Sortie des résultats ####
  # Enregistrement
  if(save==T){
    ggsave(file = glue("{today()}_Vue_transect_sur_raster.png"))
  }
  
  # Affichage
  return(ggplot)
  
} # Fin de la fonction