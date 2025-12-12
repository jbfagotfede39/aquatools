#' Calcul d'un IAM
#'
#' Cette fonction permet de calculer les résultats d'un IAM à partir des cartographies d'habitats
#' @name topographie.iam
#' @param data Objet au format sf contenant les données de substrats, hauteurs d'eau et vitesses de courant
#' @param operation Dataframe contenant les caractéristiques de l'opération (date, cours d'eau, coordonnées, largeur moyenne, affluents/résurgences/sources, etc.)
#' @param export Si \code{TRUE}, exporte les différents résultats
#' @keywords topographie
#' @import glue
#' @import openxlsx2
#' @import sf
#' @import tidyverse
#' @import units
#' @export
#' @examples
#' tpiam_op_id <- "12";iam <- topographie.iam(topographie.habitats(tpiam_op_id), iam.resultats(tpiam_op_id))
#' iam <- topographie.iam(topographie.habitats("12"), iam.resultats("12"))
#' iam <- topographie.iam(habitats, operation, export = T)

topographie.iam <- function(
    data = NA_character_,
    operation = NA_character_,
    export = FALSE
)
{
  
  #### Contexte ####
  habitats_type_n <- n_distinct(data$tphab_type)
  
  #### Test de cohérence ####
  ##### Objet spatial en entrée #####
  if(!(any(class(data) %in% c("sf")) == TRUE)) stop("Jeu de données en entrée pas au format sf") # Doit contenir "sf"
  if(inherits(data, "sf") == FALSE) stop("Jeu de données en entrée pas au format sf") # Doit renvoyer TRUE
  # if(st_geometry(data) == NULL) stop("Pas de colonne de géométrie") # Doit renvoyer la colonne de géométrie
  
  ##### Projection #####
  # En Lambert 93
  
  ##### Complétude des données d'habitats #####
  if(habitats_type_n == 0 | habitats_type_n == 1) stop(glue("Présence de {habitats_type_n} type d'habitats, au lieu de 3 attendus"))
  if(habitats_type_n == 2 | habitats_type_n > 3) stop(glue("Présence de {habitats_type_n} types d'habitats, au lieu de 3 attendus"))

  #### Nettoyage & reformatage ####
  data_v2 <-
    data %>% 
    mutate(surface = round(st_area(geom), 2))
  substrats <- data_v2 %>% filter(tphab_type == "Substrat") %>% rename_with(~str_c("sub_", .), .cols = "tphab_valeur_principale")
  vitesses <- data_v2 %>% filter(tphab_type == "Vitesse") %>% rename_with(~str_c("vit_", .), .cols = "tphab_valeur_principale")
  hauteurs <- data_v2 %>% filter(tphab_type == "Hauteur") %>% rename_with(~str_c("hau_", .), .cols = "tphab_valeur_principale")
  
  #### Test de cohérence (suite) ####
  ##### Surfaces identiques #####
  habitats_type_surfaces <- 
    data_v2 %>% 
    group_by(tphab_type) %>% 
    summarise(surf_tot_brute = round(sum(surface), 2),
              surf_tot = round(sum(surface), 0)) %>% 
    st_drop_geometry() %>% 
    mutate(affichage = glue("{tphab_type} = {surf_tot_brute}"))
  habitats_type_surfaces_brute <- habitats_type_surfaces %>% pull(affichage)
  habitats_type_surfaces <- habitats_type_surfaces %>% pull(surf_tot)
  if(all(habitats_type_surfaces == habitats_type_surfaces[1]) == FALSE) stop(glue("Les surfaces des 3 groupes d'habitats (substrats, hauteurs et vitesses) ne sont pas égales : {glue_collapse(habitats_type_surfaces_brute, sep = ', ', last = ' et ')}"))
  
  #### Données de référence ####
  # Collecte de l'attractivité par substrats
  data(iam)
  # cotation_iam
  
  #### Calcul ####
  ##### Calcul des pôles d'attraction #####
  st_agr(substrats) = "constant"
  st_agr(hauteurs) = "constant"
  st_agr(vitesses) = "constant"
  poles <- 
    substrats %>% 
    # st_geometry(substrats) %>% 
    st_intersection(vitesses) %>% 
    # st_intersection(st_geometry(vitesses)) %>% 
    st_intersection(hauteurs)
    # st_intersection(st_geometry(hauteurs))
  
  ##### Établissement des codes/surfaces des pôles #####
  poles_v2 <-
    poles %>% 
    # Établissement des codes par pôle #
    mutate(code = glue("{sub_tphab_valeur_principale} - {vit_tphab_valeur_principale} - {hau_tphab_valeur_principale}")) %>% 
    # Regroupement des surfaces par pôle #
    select(-contains("surface")) %>% 
    mutate(surface = round(st_area(geom), 2))
  
  ##### Calcul des surfaces par pôle #####
  poles_v3 <-
    poles_v2 %>% 
    group_by(code) %>% 
    summarise(surface = sum(surface))
    
  poles_surfaces <-
    poles_v3 %>% 
    st_drop_geometry()
  
  ##### Métriques #####
  # Surface totale
  surface_totale <- poles_surfaces %>% summarise(surface = sum(surface)) %>% pull(surface)
  # Largeur moyenne
  largeur_mouillee_moyenne <- operation$tpiam_largeur_mouillee
  # Bonus affluents/résurgences/sources
  if(operation$tpiam_affluents_presence == FALSE) bonus <- 1
  if(operation$tpiam_affluents_presence == TRUE) bonus <- 1.25
  # Variété classes de substrats
  var_substrats <- n_distinct(poles_v2$sub_tphab_valeur_principale)
  # Variété classes de vitesses
  var_vitesses <- n_distinct(poles_v2$vit_tphab_valeur_principale)
  # Variété classes de hauteurs
  var_hauteurs <- n_distinct(poles_v2$hau_tphab_valeur_principale)
  # Variété max de pôles
  var_max_th <- var_substrats*var_vitesses*var_hauteurs
  # Variété réelle de pôles
  var_max_reelle <- n_distinct(poles_v3$code)
  # % variété réelle/théorique de pôles
  var_poles_perc <- var_max_reelle/var_max_th
  # Indice de diversité
  indice_diversite <- 
    poles_surfaces %>% 
    mutate(proportion = surface/surface_totale) %>% 
    mutate(log_proportion = log10(proportion)) %>% 
    mutate(proportion_log_proportion = proportion*log_proportion) %>% 
    summarise(proportion_log_proportion = -sum(proportion_log_proportion)) %>% 
    pull(proportion_log_proportion) %>% 
    units::drop_units()
  # Indice de régularité
  indice_regularite <- indice_diversite/-log10(1/var_max_reelle)
  # Proportion des substrats
  prop_substrats <- 
    poles_v2 %>% 
    group_by(sub_tphab_valeur_principale) %>% 
    summarise(surface = sum(surface)) %>% 
    st_drop_geometry() 
  # Proportion des vitesses
  prop_vitesses <- 
    poles_v2 %>% 
    group_by(vit_tphab_valeur_principale) %>% 
    summarise(surface = sum(surface)) %>% 
    st_drop_geometry() 
  # Proportion des hauteurs
  prop_hauteurs <- 
    poles_v2 %>% 
    group_by(hau_tphab_valeur_principale) %>% 
    summarise(surface = sum(surface)) %>% 
    st_drop_geometry() 

  # IAM observé
  iam_isca_calcul <-
    prop_substrats %>% 
    mutate(prop = surface/surface_totale) %>% 
    left_join(iam_cotation, join_by(sub_tphab_valeur_principale == substrat)) %>% 
    mutate(iam_note = prop*attractivité_iam, .after = attractivité_iam) %>% 
    mutate(isca_note = prop*attractivite_isca, .after = attractivite_isca)
  iam_obs <- iam_isca_calcul %>% units::drop_units() %>% summarise(iam_obs = sum(iam_note)) %>% pull(iam_obs) * bonus * var_substrats * var_vitesses * var_hauteurs
  iam_obs <- round(iam_obs, 0)
  
  # IAM théorique
  iam_th <- 3193.4 * log(largeur_mouillee_moyenne) + 1999.6 # La fonction log est bien équivalente à ln, vérifié par le calcul
  iam_th <- round(iam_th, 0)
  
  # Comparaison à la référence stationnelle	: IAM / IAM REF
  iam_ratio <- iam_obs/iam_th
  iam_ratio <- iam_ratio
  iam_ratio <- round(iam_ratio, 4)
  # Comparaison à la référence stationnelle	: Classe de qualité
  if(iam_ratio <= 0.2) iam_classe <- "Très mauvaise"
  if(iam_ratio > 0.2 & iam_ratio <= 0.4) iam_classe <- "Mauvaise"
  if(iam_ratio > 0.4 & iam_ratio <= 0.6) iam_classe <- "Moyenne"
  if(iam_ratio > 0.6 & iam_ratio <= 0.8) iam_classe <- "Bonne"
  if(iam_ratio > 0.8) iam_classe <- "Excellente"
  # ISCA observé
  isca_obs <- iam_isca_calcul %>% units::drop_units() %>% summarise(isca_obs = sum(isca_note)) %>% pull(isca_obs) * bonus * var_substrats * var_vitesses * var_hauteurs
  isca_obs <- round(isca_obs, 0)

  # Comment exporter dans R de la fonction plusieurs tableaux/figures ?
  operation_modifiee <-
    operation %>% 
    mutate(tpiam_sub_nb = var_substrats) %>% 
    mutate(tpiam_hau_nb = var_hauteurs) %>% 
    mutate(tpiam_vit_nb = var_vitesses) %>% 
    mutate(tpiam_poles_nb = var_max_reelle) %>% 
    mutate(tpiam_diversite_shannon = indice_diversite) %>% 
    mutate(tpiam_regularite = indice_regularite) %>% 
    mutate(tpiam_noteisca_observee = isca_obs) %>% 
    mutate(tpiam_noteiam_observee = iam_obs) %>% 
    mutate(tpiam_noteiam_theorique = iam_th) %>% 
    mutate(tpiam_noteiam_ratio = iam_ratio) %>% 
    mutate(tpiam_classe_etat = iam_classe)
  
  #### Représentation ####
  # Figuré hâchuré
  
  
  #### Export ####
  if(export == TRUE){
    
    ##### Excel #####
    wb_workbook() %>% 
      wb_add_worksheet("Résultats") %>% 
      wb_add_data(x = operation_modifiee %>% st_drop_geometry(), na.strings = "") %>% 
      wb_freeze_pane(first_active_col = 4, first_active_row = 2) %>% 
      wb_set_col_widths(cols = 1:20, widths = "auto") %>%
      wb_add_worksheet("Substrats") %>% 
      wb_add_data(x = prop_substrats %>% rename(Valeur = sub_tphab_valeur_principale), na.strings = "") %>% 
      wb_set_col_widths(cols = 1:20, widths = "auto") %>%
      wb_add_worksheet("Vitesses") %>% 
      wb_add_data(x = prop_vitesses %>% rename(Valeur = vit_tphab_valeur_principale), na.strings = "") %>% 
      wb_set_col_widths(cols = 1:20, widths = "auto") %>%
      wb_add_worksheet("Hauteurs") %>% 
      wb_add_data(x = prop_hauteurs %>% rename(Valeur = hau_tphab_valeur_principale), na.strings = "") %>% 
      wb_set_col_widths(cols = 1:20, widths = "auto") %>%
      wb_add_worksheet("Poles") %>% 
      wb_add_data(x = poles_surfaces %>% separate(code, c("Substrat", "Vitesse", "Hauteur"), " - "), na.strings = "") %>% 
      wb_freeze_pane(first_row = T) %>% 
      wb_set_col_widths(cols = 1:20, widths = "auto") %>%
      wb_save(glue("{today()}_{operation_modifiee$tpiam_coderhj}_{operation_modifiee$tpiam_op_id}_Résultats_traitement_IAM.xlsx"), overwrite = T)
    
    ##### SIG #####
    data_v2 %>% 
      select(tphab_type, tphab_valeur_principale, surface, geom) %>% 
      units::drop_units() %>% 
      group_split(tphab_type) %>% 
      # map(~SIG.export(glue("{today()}_{operation_modifiee$tpiam_coderhj}_{operation_modifiee$tpiam_op_id}_Résultats_traitement_IAM_{unique(.$tphab_type)}"), shp = F, kml = F, excel = F))
      map(~st_write(., dsn = glue("{today()}_{operation_modifiee$tpiam_coderhj}_{operation_modifiee$tpiam_op_id}_Résultats_traitement_IAM_{str_to_lower(unique(.$tphab_type))}s.geojson"), driver='GeoJSON', overwrite = TRUE, append = FALSE))
    poles_v3 %>% 
      units::drop_units() %>% 
      separate(code, c("Substrat", "Vitesse", "Hauteur"), " - ") %>% 
      st_write(., dsn = glue("{today()}_{operation_modifiee$tpiam_coderhj}_{operation_modifiee$tpiam_op_id}_Résultats_traitement_IAM_poles.geojson"), driver='GeoJSON', overwrite = TRUE, append = FALSE)
    
    ##### png #####
    warning("Mise en forme et export des cartes au format png à développer")
  }
  
  #### Sortie ####
  return(operation_modifiee)
  
} # Fin de la fonction
