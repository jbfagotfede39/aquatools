#' Vue d'un profil le long d'un transect
#'
#' Cette fonction permet de représenter un profil (topographique par exemple) d'un transect
#' @name topographie.figure.profil
#' @param data Jeu de données en sortie de topographie.profil.raster()
#' @param etiquette Affichage des étiquettes des points d'intérêt : \code{TRUE} (par défaut) ou \code{FALSE}
#' @param pointsprojetes Affichage des points d'intérêt projetés le long du transect : \code{TRUE} (par défaut) ou \code{FALSE}
#' @param save Si \code{FALSE} (par défaut), n'enregistre pas les figures. Si \code{TRUE}, les enregistre.
#' @keywords topographie
#' @import glue
#' @import magrittr
#' @import sf
#' @import tidyverse
#' @export 
#' @examples
#' data %>% topographie.figure.profil()
#' topographie.figure.profil(data, etiquette = F, pointsprojetes = F)
#' data %>% topographie.figure.profil(save = T)
#' topographie.figure.profil(raster = mon_raster_regroupe, transect = StationsTransect, points_projetes = Stations, points_projetes_position = "intermediaire") %>% topographie.figure.profil()

topographie.figure.profil <- function(
  data = NA_character_,
  etiquette = T,
  pointsprojetes = T,
  save = F
)
{
  
  #### Extraction des données ####
  donnees_profil_complet <-
    data %>% 
    magrittr::extract2(1)
  
  donnees_points_remarquables <-
    data %>% 
    magrittr::extract2(2)
  
  ### Test format de données ###
  if(!(any(class(donnees_profil_complet) %in% c("sf")) == TRUE)) stop("Paramètre donnees_profil_complet pas au format sf")
  if(!(any(class(donnees_points_remarquables) %in% c("sf")) == TRUE)) stop("Paramètre donnees_points_remarquables pas au format sf")
  
  ### Vue profil altitudinal ###
  ggplot <- ggplot(donnees_profil_complet, aes(dist_cum, valeur_raster))
  ggplot <- ggplot + geom_line()
  if(pointsprojetes == T) ggplot <- ggplot + geom_point(data = filter(donnees_points_remarquables, type == "Projeté"), aes(dist_cum, valeur_raster_etiquette))
  if(pointsprojetes == T) ggplot <- ggplot + geom_segment(data = filter(donnees_points_remarquables, type == "Projeté"), aes(x = dist_cum, xend = dist_cum, y = valeur_raster, yend = valeur_raster_etiquette))
  if(etiquette == T) ggplot <- ggplot + geom_text_repel(data = filter(donnees_points_remarquables, type == "Projeté"), aes(x = dist_cum, y = valeur_raster_etiquette, label = chsta_coderhj), size = 3, point.padding = 0.2, direction = "x")
  ggplot <- ggplot + geom_point(data = filter(donnees_points_remarquables, type == "Extrémité"), aes(dist_cum, valeur_raster))
  if(etiquette == T) ggplot <- ggplot + geom_text_repel(data = filter(donnees_points_remarquables, type == "Extrémité"), aes(x = dist_cum, y = valeur_raster, label = chsta_coderhj), size = 3, point.padding = 1.25, direction = "y")
  # ggplot <- ggplot + scale_colour_manual(values = PaletteSite)
  ggplot <- ggplot + labs(x = "Distance cumulée (m)", y = "Altitude (NGF)") # Pour changer le titre
  ggplot <- ggplot + theme_bw()
  
  #### Sortie des résultats ####
  # Enregistrement
  if(save==T){
    ggsave(file = glue("{today()}_Vue_profil.png"))
  }
  
  # Affichage
  return(ggplot)
  
} # Fin de la fonction