#' Conversion de palette QGIS vers palette R
#'
#' Cette fonction permet de convertir des palettes QGIS au format .sld vers une palette de couleur au format R
#' @name SIG.style.qgis
#' @param data Palette à convertir, au format xml
#' @param format Format de sortie : \code{palette} (par défaut) ou \code{dataframe}
#' @keywords SIG
#' @import tidyverse
#' @import xml2
#' @export
#' @examples
#' SIG.style.qgis(palette)
#' tbl(dbD, dbplyr::in_schema("public", "layer_styles")) %>% filter(f_table_name == "topographie_habitats") %>% collect() %>% SIG.style.qgis(.$stylesld)

SIG.style.qgis <- function(
    data, 
    format = c("palette", "dataframe")
) {
  
  #### Évaluation des choix ####
  format <- match.arg(format)
  
  #### Nettoyage & reformatage ####
  ##### Extraction #####
  data_parse <- 
    data %>% 
    read_xml()
  
  data_parse_rules <-
    data_parse %>% 
    xml_find_all("//se:Rule")
  
  style_labels <-
    data_parse_rules %>% 
    xml_child("se:Name") %>% 
    as.character() %>% 
    as_tibble() %>% 
    mutate(value = str_replace(value, "<se:Name>", "")) %>% 
    mutate(value = str_replace(value, "</se:Name>", "")) %>% 
    rename(description = value)
  
  style_colors <-
    data_parse_rules %>% 
    xml_child("se:PolygonSymbolizer") %>% 
    xml_child("se:Fill") %>% 
    xml_child("se:SvgParameter") %>% 
    as.character() %>% 
    as_tibble() %>% 
    mutate(value = str_replace(value, "<se:SvgParameter name=\"fill\">", "")) %>% 
    mutate(value = str_replace(value, "</se:SvgParameter>", "")) %>% 
    rename(html_color = value)
  
  ##### Vérification #####
  style_labels_n <- nrow(style_labels)
  style_colors_n <- nrow(style_colors)
  if(style_labels_n != style_colors_n) stop(glue("Nombre d'étiquettes ({style_labels_n}) différent du nombre de couleurs ({style_colors_n})"))
  
  ##### Regroupement #####
  palette_df <-
    style_colors %>% 
    bind_cols(style_labels)
  
  ##### Transformation en une palette #####
  palette <- setNames(palette_df$html_color, palette_df$description)
  
  #### Sortie ####
  if(format == "palette") return(palette)
  if(format == "dataframe") return(palette_df)
}
