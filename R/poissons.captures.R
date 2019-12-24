#' Extraction des données de captures de poissons pour un inventaire
#'
#' Récupère les données de captures de poissons d'un inventaire
#' @name poissons.captures
#' @keywords donnees
#' @param station CodeRDT de la station
#' @param date Date de l'opération
#' @param codeCapture Affichage du codecapture - \code{FALSE} (par défault) 
#' @param codePlacette Affichage du codeplacette - \code{FALSE} (par défault) 
#' @param codeOperation Affichage du codeoperation - \code{FALSE} (par défault) 
#' @import dplyr 
#' @import RSQLite 
#' @import DBI 
#' @import lubridate
#' @export
#' @examples
#' poissons.captures()
#' poissons.captures("SOR10-2", "2015-05-19")
#' poissons.captures("SOR10-2", "2015-05-19", codePlacette=T)

poissons.captures <- function(  
  station="",
  date="",
  codeCapture = FALSE,
  codePlacette = FALSE,
  codeOperation = FALSE)
{

## Ouverture de la BDD ##
dbP <- BDD.ouverture(Type = "Poissons")

##### Récupération des données #####
Captures <- tbl(dbP,"captures") %>% collect(n = Inf)
Inventaires <- tbl(dbP,"inventaires") %>% collect(n = Inf)
Stations <- tbl(dbP,"stations") %>% collect(n = Inf)

## Fermeture de la BDD ##
DBI::dbDisconnect(dbP)

##### Synthèse des données #####
Captures <- merge(Captures, Inventaires, by = c("codeinventaire"))
Captures <- merge(Captures, Stations, by = c("codestation"))

##### Transformation des formats de dates
Captures$datedebut <- ymd(Captures$datedebut)

##### Filtrage #####
if(nchar(station) != 0 & nchar(date) != 0){
Captures <-
  Captures %>% 
  filter(nom == station, datedebut == date) %>%
  arrange(numerodepassage, codeespece)}

if(nchar(station) != 0 & nchar(date) == 0){
  Captures <-
    Captures %>% 
    filter(nom == station) %>%
    arrange(numerodepassage, codeespece)}

if(nchar(station) == 0 & nchar(date) != 0){
  Captures <-
    Captures %>% 
    filter(datedebut == date) %>%
    arrange(numerodepassage, codeespece)}

# On garde captures identiques si les deux paramètres sont vides pour avoir toute la base de données

##### Simplification #####
if(codeCapture == FALSE & codePlacette == FALSE & codeOperation == FALSE){
Captures <- 
  Captures %>%
   select(nom, datedebut, numerodepassage, codeespece, tailleminimum, taillemaximum, nombre, poids)}

if(codeCapture == FALSE & codePlacette == FALSE & codeOperation == TRUE){
  Captures <- 
    Captures %>%
    select(codeoperation, nom, datedebut, numerodepassage, codeespece, tailleminimum, taillemaximum, nombre, poids)}

if(codeCapture == FALSE & codePlacette == TRUE & codeOperation == FALSE){
  Captures <- 
    Captures %>%
    select(codeplacette, nom, datedebut, numerodepassage, codeespece, tailleminimum, taillemaximum, nombre, poids)}

if(codeCapture == FALSE & codePlacette == TRUE & codeOperation == TRUE){
  Captures <- 
    Captures %>%
    select(codeplacette, codeoperation, nom, datedebut, numerodepassage, codeespece, tailleminimum, taillemaximum, nombre, poids)}

if(codeCapture == TRUE & codePlacette == FALSE & codeOperation == FALSE){
  Captures <- 
    Captures %>%
    select(codecapture, nom, datedebut, numerodepassage, codeespece, tailleminimum, taillemaximum, nombre, poids)}

if(codeCapture == TRUE & codePlacette == FALSE & codeOperation == TRUE){
  Captures <- 
    Captures %>%
    select(codecapture, codeoperation, nom, datedebut, numerodepassage, codeespece, tailleminimum, taillemaximum, nombre, poids)}

if(codeCapture == TRUE & codePlacette == TRUE & codeOperation == FALSE){
  Captures <- 
    Captures %>%
    select(codecapture, codeplacette, nom, datedebut, numerodepassage, codeespece, tailleminimum, taillemaximum, nombre, poids)}

if(codeCapture == TRUE & codePlacette == TRUE & codeOperation == TRUE){
  Captures <- 
    Captures %>%
    select(codecapture, codeplacette, codeoperation, nom, datedebut, numerodepassage, codeespece, tailleminimum, taillemaximum, nombre, poids)}

##### Calcul d'une colonne taille (avec la taille moyenne pour les lots) #####
Captures <- 
Captures %>% 
  rowwise() %>% # groupement par ligne
  mutate(taillemoy = as.double(mean(c(tailleminimum,taillemaximum)))) %>% # Afin de calculer la taille moyenne pour les lots
  ungroup() %>% # Afin d'enlever le groupement par ligne lié à rowwise
  mutate(taillemoy = case_when(.$nombre == 1 ~ .$taillemaximum,
                               .$nombre != 1 ~ .$taillemoy)) # Afin de compléter les tailles pour les poissons individuels

##### Nettoyage des 0 #####
if(dim(Captures)[1] != 0) Captures[Captures == 0] <- NA
if(dim(Captures)[1] == 0) warning("Aucune capture correspondante")

return(Captures)
} # Fin de la fonction