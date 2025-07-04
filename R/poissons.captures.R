#' Extraction des données de captures de poissons pour une opération
#'
#' Récupère les données de captures de poissons d'une opération
#' @name poissons.captures
#' @keywords donnees
#' @param station CodeRHJ de la station
#' @param date Date de l'opération
#' @param operation Code de l'opération
#' @param codeCapture Affichage du codecapture - \code{FALSE} (par défault) 
#' @param codePlacette Affichage du codeplacette - \code{FALSE} (par défault) 
#' @param codeOperation Affichage du codeoperation - \code{FALSE} (par défault) 
#' @param observations Affichage de la colonne observations - \code{FALSE} (par défault) 
#' @param profondeur_capture Affichage de la colonne profondeurcapture - \code{FALSE} (par défault) 
#' @import DBI 
#' @import glue
#' @import tidyverse
#' @export
#' @examples
#' poissons.captures()
#' poissons.captures("SOR10-2", "2015-05-19")
#' poissons.captures("SOR10-2", "2015-05-19", codePlacette=T)
#' poissons.captures(operation = 319)

poissons.captures <- function(  
  station = "",
  date = "",
  operation = "",
  codeCapture = FALSE,
  codePlacette = FALSE,
  codeOperation = FALSE,
  observations = FALSE,
  profondeur_capture = FALSE)
{

## Ouverture de la BDD ##
dbP <- BDD.ouverture(Type = "Poissons")

#### Test de cohérence ####
n_apostrophes_station <- str_count(station, "'")
if(n_apostrophes_station != 0) stop(glue("Attention : présence d'apostrophes dans le nom de station ({station}), qui seront mal gérés ensuite dans la requête SQL"))

##### Récupération des données #####
#### Construction de la requête ####
### Tronc commun ###
requete <- glue("
  with captures_2 as (
    select c.*, op.datedebut from captures c, operations op where c.codeoperation = op.codeoperation),
  captures_3 as (
    select c2.*, inv.codestation from captures_2 c2, inventaires inv where c2.codeinventaire = inv.codeinventaire),
  captures_4 as (
    select * from captures_3 c3, stations st where c3.codestation = st.codestation)
    select * from captures_4
  ")

if(nchar(operation) != 0){cond_operation <- glue("codeoperation = {operation}")}
if(nchar(operation) == 0){cond_operation <- ""}
n_cond_op <- nchar(cond_operation)

if(nchar(station) != 0){cond_station <- glue("nom = '{station}'")}
if(nchar(station) == 0){cond_station <- ""}
n_cond_st <- nchar(cond_station)

if(nchar(date) != 0){cond_date <- glue("datedebut = '{date}'")}
if(nchar(date) == 0){cond_date <- ""}
n_cond_da <- nchar(cond_date)

if(n_cond_op == 0 & n_cond_st == 0 & n_cond_da == 0){requete_filtree <- requete}
if(!(n_cond_op == 0 & n_cond_st == 0 & n_cond_da == 0)){requete_filtree <- glue("{requete} WHERE ")}
if(n_cond_op != 0 & n_cond_st == 0 & n_cond_da == 0){requete_filtree <- glue("{requete_filtree}{cond_operation}")}
if(n_cond_op == 0 & n_cond_st != 0 & n_cond_da == 0){requete_filtree <- glue("{requete_filtree}{cond_station}")}
if(n_cond_op == 0 & n_cond_st == 0 & n_cond_da != 0){requete_filtree <- glue("{requete_filtree}{cond_date}")}
if(n_cond_op != 0 & n_cond_st != 0 & n_cond_da == 0){requete_filtree <- glue("{requete_filtree}{cond_operation} AND {cond_station}")}
if(n_cond_op == 0 & n_cond_st != 0 & n_cond_da != 0){requete_filtree <- glue("{requete_filtree}{cond_station} AND {cond_date}")}
if(n_cond_op != 0 & n_cond_st == 0 & n_cond_da != 0){requete_filtree <- glue("{requete_filtree}{cond_operation} AND {cond_date}")}
if(n_cond_op != 0 & n_cond_st != 0 & n_cond_da != 0){requete_filtree <- glue("{requete_filtree}{cond_operation} AND {cond_station} AND {cond_date}")}

requete_filtree <- glue("{requete_filtree};")

#### Exécution de la requête ####
captures <- dbGetQuery(dbP, requete_filtree)
if(nrow(captures) == 0){captures <- dbGetQuery(dbP, glue("{requete} LIMIT 1"));captures <- captures[0,]} # Afin de préserver le datatype datedebut en date si absence de lignes retournées

## Fermeture de la BDD ##
DBI::dbDisconnect(dbP)

##### Simplification #####
colnames(captures)[9] <- "observations_cap" # car présence de plusieurs colonnes observations
captures_v2 <- 
  captures %>% 
  dplyr::select(codecapture, codeplacette, codeoperation, nom, datedebut, numerodepassage, codeespece, tailleminimum, taillemaximum, nombre, poids, observations_cap, profondeurcapture) %>% 
  rename(observations = observations_cap) %>% # car présence de plusieurs colonnes observations
  {if(codeCapture == F) select(., -codecapture) else .} %>% 
  {if(codePlacette == F) select(., -codeplacette) else .} %>% 
  {if(codeOperation == F) select(., -codeoperation) else .} %>% 
  {if(observations == F) select(., -observations) else .} %>% 
  {if(profondeur_capture == F) select(., -profondeurcapture) else .} %>% 
  arrange(codeespece, desc(taillemaximum))

#### Tri ####
captures_v3 <- 
  captures_v2 %>%
  arrange(numerodepassage, codeespece)

##### Calcul d'une colonne taille (avec la taille moyenne pour les lots) #####
captures_v4 <- 
  captures_v3 %>% 
  rowwise() %>% # groupement par ligne
  mutate(taillemoy = as.double(mean(c(tailleminimum,taillemaximum)))) %>% # Afin de calculer la taille moyenne pour les lots
  ungroup() %>% # Afin d'enlever le groupement par ligne lié à rowwise
  mutate(taillemoy = case_when(.$nombre == 1 ~ .$taillemaximum,
                               .$nombre != 1 ~ .$taillemoy)) # Afin de compléter les tailles pour les poissons individuels

##### Calcul d'une colonne poids (avec le poids moyen pour les lots) #####
captures_v5 <- 
  captures_v4 %>% 
  rowwise() %>% # groupement par ligne
  mutate(poids_moy = round(poids / nombre, 1)) %>% # Afin de calculer la taille moyenne pour les lots
  ungroup() %>% # Afin d'enlever le groupement par ligne lié à rowwise
  mutate(poids_moy = case_when(.$nombre == 1 ~ .$poids,
                               .$nombre != 1 ~ .$poids_moy)) # Afin de compléter les tailles pour les poissons individuels

##### Nettoyage #####
###### Nettoyage des 0 ######
if(dim(captures_v5)[1] != 0) captures_v5[captures_v5 == 0] <- NA

###### Nettoyage des cellules vides pas NA ######
captures_v6 <-
  captures_v5 %>% 
  {if(observations == T) mutate(., observations = ifelse(observations == "", NA, observations)) else .}

#### Vérification ####
if(dim(captures_v6)[1] == 0) warning(glue("Aucune capture correspondante dans l'opération {operation}"))

return(captures_v6)
} # Fin de la fonction
