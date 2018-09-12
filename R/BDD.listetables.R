#' Chargement des tables et champs de nas-sig-data
#'
#' Cette fonction permet de charger les noms des tables et des champs de la base de donnéesnas-sig-data
#' 
#' @import dbplyr
#' @import dplyr
#' @import keyring
#' @import RPostgreSQL
#' @export
#' @examples
#' BDD.listetables()
#' BDD.listetables() %>% View()

##### TODO LIST #####
#
#####################

BDD.listetables <- function(
  )
{

## Ouverture de la connexion ##
if(exists("dbD") == FALSE){
  dbD <- BDD.ouverture("Data")
}
  
## Récupération et nettoyage des données ##
data <-
  RPostgreSQL::dbGetQuery(dbD, "SELECT * FROM information_schema.columns") %>% 
  filter(table_schema != "pg_catalog" & table_schema != "information_schema" & table_schema != "public") %>%
  distinct(table_schema, table_name, ordinal_position, column_name, data_type) %>%
  arrange(table_name, ordinal_position) %>%
  separate(table_name, c("tbllst_theme", "tbllst_table"), sep = "_", remove = F) %>%
  separate(column_name, c("tbllst_table_court", "tbllst_champs"), sep = "_", remove = F) %>% 
  rename(tbllst_table_schema = table_schema) %>% 
  rename(tbllst_table_name = table_name) %>% 
  rename(tbllst_column_name = column_name) %>% 
  rename(tbllst_ordinal_position = ordinal_position) %>% 
  rename(tbllst_data_type = data_type)

## Retour des données ##
return(data)

## Fermeture de la BDD ##
RPostgreSQL::dbDisconnect(dbD)
  
} # Fin de la fonction
