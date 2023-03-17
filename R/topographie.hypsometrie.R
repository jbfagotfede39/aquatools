#' Création d'une hypsométrie
#'
#' Cette fonction permet de créer une vue hypsométrique
#' @name topographie.hypsometrie
#' @param data Dataframe contenant une colonne `chsta_coord_z` ou une géométrie de type POINTZ. Affichage de différentes courbes si présence d'une colonne `plan_deau`
#' @param epsg EPSG à attribuer au 
#' @keywords topographie
#' @import ggplot2
#' @import sf
#' @import tidyverse
#' @export 
#' @examples
#' data %>% topographie.hypsometrie()

topographie.hypsometrie <- function(
  data = NA_character_
)
{
  
  #### Préparation des données ####
  if(any(class(data) %in% c("sf")) == TRUE){
    if(st_coordinates(data) %>% ncol() == 3){
      data_v2 <-
        data %>% 
        mutate(chsta_coord_z = -1*st_coordinates(.)[,3], .before = "geom")
    }
  }
  
  # if(!(any(class(data) %in% c("sf"))) == TRUE){
  if(exists("data_v2") == FALSE){
    data_v2 <- data
  }
  
  #### Tests ####
  if(!("chsta_coord_z" %in% names(data_v2))) stop("Pas de colonne chsta_coord_z, ou pas de composante Z dans les coordonnées")
  if("plan_deau" %in% names(data_v2)){
    if(n_distinct(data_v2$plan_deau) > 5) stop("Seulement 5 modalités colorimétriques en place dans l'immédiat")
  }
    
  #### Représentation ####
  hypsographie <- ggplot(bathymetrie_ponctuels_par_plan_deau, aes(-chsta_coord_z))
  if(!("plan_deau" %in% names(data_v2))) hypsographie <- hypsographie + geom_line(aes(y = 1 - ..y..), stat='ecdf')
  if("plan_deau" %in% names(data_v2)) hypsographie <- hypsographie + geom_line(aes(y = 1 - ..y.., color = plan_deau), stat='ecdf')
  hypsographie <- hypsographie + coord_flip()
  hypsographie <- hypsographie + theme_bw()
  hypsographie <- hypsographie + labs(y = "Fréquence cumulée", x = "Profondeur (mètres)")
  if("plan_deau" %in% names(data_v2)) hypsographie <- hypsographie + labs(color = "Plan d'eau")
  if("plan_deau" %in% names(data_v2)) hypsographie <- hypsographie + scale_colour_manual(values = c("#E97F02", "#273582", "#228B22", "#F8CA00", "#BD1550"))
  
  #### Sortie des résultats ####
  return(hypsographie)
  
} # Fin de la fonction