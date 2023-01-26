#' Création de vues spatialisée de profils verticaux lacustres
#'
#' Cette fonction permet de créer des vues spatialisée de profils verticaux lacustres de paramètres physico-chimiques
#' @name PC.lac.vuespatiale
#' @param data Jeu de données à représenter, issu de fd_production.physicochimie_mesures
#' @param contourplandeau Contour de lac au format sf
#' @param profondeur Profondeur à laquelle les données sont à représenter (\code{zmax} par défaut, sinon valeur libre positive à saisir)
#' @param legendemin Valeur minimale de la légende (valeur par défaut fonction du paramètre représenté)
#' @param legendemax Valeur maximale de la légende (valeur par défaut fonction du paramètre représenté)
#' @param facetting Variable sur laquelle réaliser un facetting (aucun par défaut)
#' @param save Si \code{FALSE} (par défault), n'enregistre pas les figures. Si \code{TRUE}, les enregistre.
#' @param projet Nom du projet
#' @param format Défini le format d'enregistrement (par défaut .png)
#' @keywords physico-chimie
#' @export
#' @import cowplot
#' @import ggspatial
#' @import glue
#' @import grid
#' @import sf
#' @import tidyverse
#' @examples
#' PC %>% PC.lac.vuespatiale()
#' PC %>% PC.lac.vuespatiale(contourplandeau = ContourPlandeau)
#' PC %>% PC.lac.vuespatiale(contourplandeau = ContourPlandeau, facetting = "pcmes_date")
#' PC %>% PC.lac.vuespatiale(contourplandeau = ContourPlandeau, facetting = "pcmes_date", profondeur = "zmax", legendemin = 0, legendemax = 100)

##### -------------- A FAIRE -------------- #####
# Développement pour des lacs différents
# Développement pour des paramètres différents
# Les valeurs de référence pour les limites de base seraient à mettre dans un dataframe à part, qu'on pourrait appeler/consulter sous forme de fonction, afin de pouvoir les connaître sans aller dans le code (et envoyer ce dataframe de référence comme argument, si on souhaite faire des modifs automatisées en fct de son propre jeu de données)
##### -------------- A FAIRE -------------- #####

PC.lac.vuespatiale <- function(
  data,
  contourplandeau,
  profondeur = c("zmax", "2"),
  legendemin = NA_real_,
  legendemax = NA_real_,
  facetting = NA_character_,
  save=F,
  projet = NA_character_,
  format=".png"
  )
  {
  
  #### Évaluation des choix ####
  # profondeur <- match.arg(profondeur)
  
  #### Contexte ####
  Contexte <- PC.contexte(data)
  
  #### Vérifications ####
  if("sf" %in% class(data) == FALSE) stop("Jeu de onnées en entrée pas au format sf")
  if("sf" %in% class(contourplandeau) == FALSE) stop("Contour du lac pas au format sf")
  if(Contexte$nmilieu != 1) stop("Plusieurs milieux concernés")
  if(Contexte$ntypemesure != 1) stop("Plusieurs paramètres concernés")
  if(!(Contexte$typemesure) %in% c("Oxygène dissous (saturation)", "Oxygène dissous", "Température", "Conductivité à 25°C", "pH", "Potentiel d’oxydo-réduction", "Phycocyanine", "Chlorophylle")) stop("Paramètre des données en entrée non développé")
  if(profondeur != "zmax"){
    profondeur <- as.numeric(profondeur)
    if(profondeur > max(data$pcmes_profondeurlacustre, na.rm = T)) stop("Valeur de profondeur fournie supérieure à la valeur maximale du jeu de données")
  }
  
  if(!is.na(facetting)){
    if(facetting %in% names(data) == FALSE) stop("La variable de facetting n'est pas présente dans le jeu de données fourni")
  }
  
  #### Transformation des données ####
  datafiltrees <-
    data %>% 
    group_by(pcmes_date, pcmes_coderhj) %>% 
    {if(Contexte$typemesure %in% c("Phycocyanine", "Chlorophylle")) filter(., pcmes_unitesandre == 133) else .} %>% # On ne conserve que les ug/L pour les pigments
    {if(profondeur == "zmax") slice_max(., pcmes_profondeurlacustre) else .} %>% # On conserve la valeur maximale
    {if(profondeur != "zmax") slice(., which.min(abs(pcmes_profondeurlacustre - profondeur))) else .} %>% # On conserve la valeur la plus proche de notre référence
    {if(profondeur != "zmax") filter(., pcmes_profondeurlacustre > (profondeur-1)) else .} # Pour ne conserver que les valeurs ayant un sens : si je souhaite un affichage à 10m, je ne veux pas les dernières valeurs des points peu profonds à 6m : je veux uniquement les points où il y a des valeurs proches de 10m

  #### Représentations ####
  gg <- ggplot()
  gg <- gg + geom_sf(data = contourplandeau)
  gg <- gg + geom_sf(data = datafiltrees, aes(color = pcmes_valeur))
  
  ### Légende ###
  #### Paramètres de légende ####
  parametres <- Contexte %>% PC.figure.parametres(typefigure = "valeurs")
  legendeY <- parametres$legendeY
  legendeTitre <- parametres$legendeTitre
  typemesureTitreSortie <- parametres$typemesureTitreSortie
  if(profondeur == "zmax") titre <- "fond du lac"
  if(profondeur != "zmax" & profondeur == 1) titre <- glue("-{profondeur} mètre")
  if(profondeur != "zmax" & profondeur != 1) titre <- glue("-{profondeur} mètres")
  
  gg <- gg + labs(color = legendeTitre) # Pour changer le titre
  
  if(Contexte$typemesure == "Température"){
    if(is.na(legendemin)) legendemin <- 0
    if(is.na(legendemax)) legendemax <- 25
  }
  if(Contexte$typemesure == "Oxygène dissous"){
    if(is.na(legendemin)) legendemin <- 0
    if(is.na(legendemax)) legendemax <- 12.5
  }
  if(Contexte$typemesure == "Oxygène dissous (saturation)"){
    if(is.na(legendemin)) legendemin <- 0
    if(is.na(legendemax)) legendemax <- 120
  }
  if(Contexte$typemesure == "Conductivité à 25°C"){
    if(is.na(legendemin)) legendemin <- 200
    if(is.na(legendemax)) legendemax <- 300
  }
  if(Contexte$typemesure == "pH"){
    if(is.na(legendemin)) legendemin <- 6.9
    if(is.na(legendemax)) legendemax <- 8.7
  }
  if(Contexte$typemesure == "Potentiel d’oxydo-réduction"){
    if(is.na(legendemin)) legendemin <- -250
    if(is.na(legendemax)) legendemax <- 300
  }
  if(Contexte$typemesure == "Phycocyanine"){
    if(is.na(legendemin)) legendemin <- 0
    if(is.na(legendemax)) legendemax <- 5
  }
  if(Contexte$typemesure == "Chlorophylle"){
    if(is.na(legendemin)) legendemin <- 0
    if(is.na(legendemax)) legendemax <- 5
  }
  
  
  if(Contexte$typemesure %in% c("Oxygène dissous", "Oxygène dissous (saturation)")){
    gg <- gg + scale_colour_gradient2(low = "red", mid = "yellow", high = "blue",
                                      midpoint = (legendemin + legendemax)/2,
                                      limits=c(legendemin, legendemax)
    )
  }
  
  if(Contexte$typemesure %in% c("Température", "Conductivité à 25°C", "pH", "Potentiel d’oxydo-réduction", "Phycocyanine", "Chlorophylle")){
    gg <- gg + scale_colour_gradient2(low = "blue", mid = "yellow", high = "red",
                                      midpoint = (legendemin + legendemax)/2,
                                      limits=c(legendemin, legendemax)
    )
  }
  
  if(Contexte$typemesure %in% c("Oxygène dissous", "Oxygène dissous (saturation)", "Température")){
    gg <- gg + guides(colour = guide_colourbar(reverse = TRUE)) # Pour inverser le sens de la légende : grandes valeurs en premier
  }
  
  legende <- cowplot::get_legend(gg + theme(legend.position="bottom")) # Extraction de la légende pour la réutiliser ensuite de manière autonome

  ### Flèche du nord et échelle ###
  if(!is.na(facetting)){
  scale_params <- tibble::tibble(
    pcmes_date = c("2020-12-14"),
    width_hint = 0.25,
    style = c("bar"),
    location = c("br"),
    unit_category = c("metric"),
    text_col = c("black"),
    line_col = c("black")
  )
  north_arrow_params <- tibble::tibble(
    pcmes_date = c("2020-12-14"),
    location = c("tl"),
    which_north = "true"
  )
  gg <- gg + annotation_scale(aes(width_hint = width_hint, style = style, location = location, unit_category = unit_category, text_col = text_col, line_col = line_col),
                              data = scale_params,
                              plot_unit = "m") # Visible sur une seule carte
  gg <- gg + annotation_north_arrow(aes(location = location, which_north = which_north),
                                    data = north_arrow_params,
                                    height = unit(0.75, "cm"), width = unit(0.75, "cm")) # Visible sur une seule carte
  }
  if(is.na(facetting)){
    gg <- gg + annotation_scale(location = "br", width_hint = 0.15, pad_x = unit(0.25, "cm"))
    gg <- gg + annotation_north_arrow(
      location = "bl",
      height = unit(0.75, "cm"),
      width = unit(0.75, "cm"),
      which_north = "true", #grid pour le haut simple, true pour le nord véritable
      # pad_x = unit(0.75, "in"),
      pad_y = unit(1.5, "cm"))#,
    # style = north_arrow_fancy_orienteering)
  }
  
  ### Facetting ###
  if(!is.na(facetting)){
  # gg <- gg + facet_wrap(~ pcmes_date, ncol = 4)
  # gg <- gg + facet_wrap(~ quo_name(facetting), ncol = 4)
  # gg <- gg + facet_wrap(~ enquo(facetting), ncol = 4)
  gg <- gg + facet_wrap(vars(!!sym(facetting)), ncol = 4)
  }
  
  ### Cosmétique générale ###
  gg <- gg + coord_sf(crs = 2154, datum = sf::st_crs(2154))
  gg <- gg + theme_void() # Pas d'axes de coordonnées
  gg <- gg + theme(legend.position="none") # Pas de légende
  gg
  
  #### Assemblage des différentes parties ####
  TitreFigure <- grid::textGrob(glue("{Contexte$milieu} ({titre})"), gp = grid::gpar(fontface="bold"))
  g <- cowplot::plot_grid(TitreFigure, gg, legende, 
                          ncol = 1,
                          rel_heights = c(.05, 1, .1))
  g
  
  #### Affichage des résultats ####
  if(save==T){
    ggsave(file = glue("{projet}{Contexte$milieu}{typemesureTitreSortie}{titre}{format}"))
  }
  if(save==F){return(g)}

} # Fin de la fonction
