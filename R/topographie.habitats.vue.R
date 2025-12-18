#' Classes de valeurs de chroniques sous forme calendaire
#'
#' Cette fonction permet de représenter les classes de valeurs de référence pour des chroniques de mesures (température, niveaux, etc.) sous forme calendaire.
#' @name topographie.habitats.vue
#' @param data Objet au format sf contenant les données de substrats, hauteurs d'eau et vitesses de courant
#' @param type Type de représentation graphique : \code{Substrats}, \code{Vitesses} de courant, \code{Hauteurs} d'eau ou \code{Pôles} d'attraction
#' @param proportions Affichage des proportions absolues de chaque habitat : \code{TRUE} par défaut
#' @param titre Titre du graphique (vide par défaut)
#' @param caption Légende de bas de graphique : \code{Données FDPPMA39} par défaut
#' @param position_echelle Position de l'échelle (\code{tr} par défaut)
#' @param ecoulement_orientation Orientation par rapport au nord, en degrés, dans le sens des aiguilles d'une montre
#' @param export Si \code{TRUE}, exporte les différents résultats
#' @param format Défini le format d'enregistrement (par défaut .png)
#' @keywords chronique
#' @import ggpattern
#' @import ggspatial
#' @import glue
#' @import scales
#' @import sf
#' @import stringr
#' @import tidyverse
#' @import units
#' @export
#' @examples
#' topographie.habitats(36) %>% topographie.habitats.vue("Substrats")
#' topographie.habitats(tpiam_op_id) %>% topographie.habitats.vue("Vitesses")
#' topographie.habitats(tpiam_op_id) %>% topographie.habitats.vue("Hauteurs")
#' data(habitats_exemple) ; habitats_exemple %>% topographie.habitats.vue("Substrats", proportions = FALSE)
#' data(habitats_exemple) ; habitats_exemple %>% topographie.habitats.vue("Pôles", position_echelle = "br", caption = "Données CD39/FDPPMA39") + ggspatial::annotation_north_arrow(location = "tl")

topographie.habitats.vue <- function(
  data = data,
  type = c("Substrats", "Vitesses", "Hauteurs", "Pôles"),
  proportions = TRUE,
  titre = "",
  caption = "FDPPMA39",
  position_echelle = "tr",
  ecoulement_orientation = 0,
  export = FALSE,
  format = ".png"
  )
{
  
  ## Évaluation des choix
  type <- match.arg(type)
  
  #### Contexte ####
  habitats_type_n <- n_distinct(data$tphab_type)

  #### Test de cohérence ####
  ##### Objet spatial en entrée #####
  if(!(any(class(data) %in% c("sf")) == TRUE)) stop("Jeu de données en entrée pas au format sf") # Doit contenir "sf"
  if(inherits(data, "sf") == FALSE) stop("Jeu de données en entrée pas au format sf") # Doit renvoyer TRUE
  # if(st_geometry(data) == NULL) stop("Pas de colonne de géométrie") # Doit renvoyer la colonne de géométrie
  if(ecoulement_orientation != 0) warning("Attention : l'orientation de l'écoulement n'est pas encore développée")
  if(type == "Pôles") message("Seules les étiquettes des 15 plus grands pôles sont affichées")
  
  ##### Projection #####
  # En Lambert 93
  
  ##### Unicité de l'opération #####
  if(n_distinct(data$tphab_tphabop_id) != 1) stop(glue("Présence de {n_distinct(tphab_tphabop_id)} opérations, au lieu d'une unique"))
  
  #### Données de référence ####
  ##### Collecte de l'attractivité par substrats et des regroupements de substrats (BLO/BLS, etc.) #####
  data(iam_cotation)
  # iam_cotation
  data <- 
    data %>% 
    left_join(iam_cotation, join_by(tphab_valeur_principale == habitat))
  
  #### Filtrage ####
  substrats <- data %>% filter(tphab_type == "Substrat")
  vitesses <- data %>% filter(tphab_type == "Vitesse")
  hauteurs <- data %>% filter(tphab_type == "Hauteur")
  if(type == "Substrats") data_v2 <- substrats
  if(type == "Vitesses") data_v2 <- vitesses
  if(type == "Hauteurs") data_v2 <- hauteurs
  if(type == "Pôles") {
    st_agr(substrats) = "constant"
    st_agr(hauteurs) = "constant"
    st_agr(vitesses) = "constant"
    data_v2 <- 
      substrats %>% 
      st_intersection(vitesses) %>% 
      st_intersection(hauteurs) %>% 
      mutate(pole_libelle = glue("{tphab_valeur_principale}_{tphab_valeur_principale.1}_{tphab_valeur_principale.2}"))
  }
  
  #### Calcul ####
  surface_totale <- data_v2 %>% mutate(surface_locale = st_area(geom)) %>% st_drop_geometry() %>% summarise(surface_totale = sum(surface_locale)) %>% pull() %>% units::drop_units()
  data_v2 <-
    data_v2 %>% 
    left_join(
      data_v2 %>% 
        mutate(surface_locale = st_area(geom)) %>% 
        st_drop_geometry() %>% 
        group_by(tphab_valeur_principale) %>% 
        summarise(surface = sum(surface_locale)) %>% 
        mutate(proportion = round(surface/surface_totale, 4)) %>% 
        mutate(description_complete = glue("{tphab_valeur_principale} ({proportion*100}%)")),
      join_by(tphab_valeur_principale)
    ) %>% 
    mutate(particularites = ifelse(is.na(particularites), "Aucune", particularites)) %>% 
    mutate(particularites = as_factor(particularites)) %>% 
    {if(type == "Pôles") mutate(., description_complete = glue("{tphab_valeur_principale}_{tphab_valeur_principale.1}_{tphab_valeur_principale.2} ({proportion*100}%)")) else .} %>% 
    {if(type != "Pôles") mutate(., tphab_valeur_principale = fct_reorder(tphab_valeur_principale, ordre, .desc = FALSE)) else .} %>% 
    {if(type != "Pôles") mutate(., description_complete = fct_reorder(.$description_complete, ordre, .desc = FALSE)) else .}

  palette_habitats_modifiee_avec_proportions <- setNames(data_v2$html_color, data_v2$description_complete)

  #### Création de la vue ####
  ##### Paramètres #####
  legende_titre <- type
  
  data_v2_top15 <- 
    data_v2 %>%
    arrange(desc(surface)) %>%
    head(15)   

  ##### Représentation #####
  gg <- ggplot(data_v2)
  if(type == "Pôles") gg <- gg + geom_sf(color = "black")
  if(type == "Vitesses" | type == "Hauteurs") {
    if(proportions == FALSE) gg <- gg + geom_sf(aes(fill = tphab_valeur_principale), color = "black")
    if(proportions == TRUE) gg <- gg + geom_sf(aes(fill = description_complete), color = "black")
  }
  if(type == "Substrats") {
    if(proportions == FALSE) gg <- gg + geom_sf_pattern(aes(fill = tphab_valeur_principale), pattern = "none")
    if(proportions == TRUE) gg <- gg + geom_sf_pattern(aes(fill = description_complete), pattern = "none")
    gg <- gg + geom_sf_pattern(aes(pattern = particularites), fill = "transparent")
    gg <- gg + 
      scale_pattern_manual(
        values = c(
          "Colmatage" = "stripe",
          "Pavage" = "circle",
          # "Organique" = "point", # Ne fonctionne pas
          "Organique" = "circle",
          "Dense" = "crosshatch",
          "Éparse" = "stripe",
          "Aucune" = "none"
        ))
  }
  if(type != "Pôles") {
    if(proportions == FALSE) gg <- gg + scale_fill_manual(values = palette_habitats)
    if(proportions == TRUE) gg <- gg + scale_fill_manual(values = palette_habitats_modifiee_avec_proportions)
  }
  if(type == "Pôles") gg <- gg + geom_sf_text(data = data_v2_top15, aes(label = description_complete), size = 2,
                                              nudge_x = 8,
                                              nudge_y = 8,
                                              check_overlap = TRUE)

  gg <- gg + theme_minimal()
  gg <- gg + ggspatial::annotation_scale(location = position_echelle)
  if(type != "Pôles") gg <- gg + labs(fill = legende_titre)
  if(type == "Substrats") gg <- gg + labs(pattern = "Particularités")
  gg <- gg + xlab(NULL)
  gg <- gg + ylab(NULL)
  gg <- gg + labs(caption = glue("Source des données : {caption}"))
  gg

  #### Export ####
  if(export == TRUE){
    ggsave(glue("{today()}_Vue_habitats_{titre}_{str_to_lower(type)}_{format}"))
  }
  
  #### Sortie ####
  return(gg)
  
} # Fin de la fonction
