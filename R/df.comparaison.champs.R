#' Comparaison de noms de champs
#'
#' Cette fonction permet de comparer les champs présents dans deux dataframes (noms et datatypes)
#' @name df.comparaison.champs
#' @param data_1 Jeu de données 1 à comparer
#' @param data_2 Jeu de données 2 à comparer
#' @keywords Données
#' @export
#' @examples
#' df.comparaison.champs(data_1, data_2)
#' data_1 %>% df.comparaison.champs(data_2) %>% view()
#' data_1 %>% df.comparaison.champs(data_2) %>% print(n = Inf)

df.comparaison.champs <- function(data_1, data_2){

### Extraction des informations du premier objet ###
df_1 <-
  data_1 %>% 
  map(class) %>% 
  as_tibble() %>% 
  pivot_longer(everything(), names_to = "df1_nom_colonne", values_to = "df1_type_colonne")

### Extraction des informations du deuxième objet ###
df_2 <-
  data_2 %>% 
  map(class) %>% 
  as_tibble() %>% 
  pivot_longer(everything(), names_to = "df2_nom_colonne", values_to = "df2_type_colonne") %>% 
  mutate(df2 = df2_nom_colonne)

### Regroupement ###
comparaison <-
  df_1 %>% 
  full_join(df_2, 
            by = c("df1_nom_colonne" = "df2")
  ) %>% 
  distinct()

return(comparaison)

}