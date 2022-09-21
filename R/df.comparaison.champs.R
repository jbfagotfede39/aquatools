#' Comparaison de noms de champs
#'
#' Cette fonction permet de comparer les champs présents dans deux dataframes
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

comparaison <-
  names(data_1) %>% 
  as_tibble() %>% 
  rename(df1 = value) %>% 
  left_join(
    names(data_2) %>% 
      as_tibble() %>% 
      rename(df1 = value) %>% 
      mutate(df2 = df1),
    by = c("df1" = "df1")
  )

return(comparaison)

}