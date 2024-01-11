#' Vue d'un profil le long d'un transect
#'
#' Cette fonction permet de représenter un profil (topographique par exemple) d'un transect
#' @name topographie.figure.profil
#' @param data_profil_complet Jeu de données en sortie de \code{topographie.profil} ou de \code{topographie.profil.raster}
#' @param data_points_remarquables Jeu de données en sortie de \code{topographie.profil} ou de \code{topographie.profil.raster}
#' @param altitude_ligne_deau Altitude de la ligne d'eau à afficher. Si \code{0} (par défaut), pas d'affichage
#' @param points_remarquables Affichage des points remarquables projetés le long du transect : \code{TRUE} (par défaut) ou \code{FALSE}
#' @param points_remarquables_etiquette Affichage des étiquettes des points remarquables : \code{TRUE} (par défaut) ou \code{FALSE}
#' @param save Si \code{FALSE} (par défaut), n'enregistre pas les figures. Si \code{TRUE}, les enregistre.
#' @keywords topographie
#' @import glue
#' @import sf
#' @import tidyverse
#' @export 
#' @examples
#' data %>% topographie.figure.profil()
#' topographie.figure.profil(data, points_remarquables_etiquette = F, points_remarquables = F)
#' data %>% topographie.figure.profil(save = T)
#' topographie.figure.profil(raster = mon_raster_regroupe, transect = StationsTransect, points_projetes = Stations, points_projetes_position = "intermediaire") %>% topographie.figure.profil()

topographie.figure.profil <- function(
  data_profil_complet = NA_character_,
  data_points_remarquables = NA_character_,
  altitude_ligne_deau = 0,
  points_remarquables = FALSE,
  points_remarquables_etiquette = FALSE,
  save = FALSE
)
{
  
  #### Nettoyage & reformatage ####
  if(is.na(data_points_remarquables)) points_remarquables <- FALSE
  if(is.na(data_points_remarquables)) points_remarquables_etiquette <- FALSE
  data_profil_complet_1 <-
    data_profil_complet %>% 
    {if("tplv_coord_z" %in% names(.)) rename(., valeur = tplv_coord_z) else .} %>% 
    {if("valeur_raster" %in% names(.)) rename(., valeur = valeur_raster) else .}
  
  #### Test de cohérence ####
  if(!(any(class(data_profil_complet) %in% c("sf")) == TRUE)) stop("Paramètre 'data_profil_complet' pas au format sf")
  if(points_remarquables == T) {if(!(any(class(data_points_remarquables) %in% c("sf")) == TRUE)) stop("Paramètre 'data_points_remarquables' pas au format sf")}
  
  #### Vue profil altitudinal ####
  ggplot <- ggplot(data_profil_complet_1, aes(dist_cum, valeur))
  ggplot <- ggplot + geom_line()
  if(points_remarquables == T) ggplot <- ggplot + geom_point(data = filter(data_points_remarquables, type == "Projeté"), aes(dist_cum, valeur_raster_etiquette))
  if(points_remarquables == T) ggplot <- ggplot + geom_segment(data = filter(data_points_remarquables, type == "Projeté"), aes(x = dist_cum, xend = dist_cum, y = valeur, yend = valeur_raster_etiquette))
  if(points_remarquables_etiquette == T) ggplot <- ggplot + geom_text_repel(data = filter(data_points_remarquables, type == "Projeté"), aes(x = dist_cum, y = valeur_raster_etiquette, label = chsta_coderhj), size = 3, point.padding = 0.2, direction = "x")
  if(points_remarquables == T) ggplot <- ggplot + geom_point(data = filter(data_points_remarquables, type == "Extrémité"), aes(dist_cum, valeur))
  if(points_remarquables_etiquette == T) ggplot <- ggplot + geom_text_repel(data = filter(data_points_remarquables, type == "Extrémité"), aes(x = dist_cum, y = valeur, label = chsta_coderhj), size = 3, point.padding = 1.25, direction = "y")
  if(altitude_ligne_deau != 0) ggplot <- ggplot + geom_hline(yintercept = altitude_ligne_deau, col = "blue")
  # ggplot <- ggplot + scale_colour_manual(values = PaletteSite)
  ggplot <- ggplot + labs(x = "Distance cumulée (m)", y = "Altitude (NGF)") # Pour changer le titre
  ggplot <- ggplot + theme_minimal()
  
  #### Sortie des résultats ####
  # Enregistrement
  if(save==T){
    ggsave(file = glue("{today()}_Vue_profil.png"))
  }
  
  # Affichage
  return(ggplot)
  
} # Fin de la fonction