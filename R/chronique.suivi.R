#' Extraction suivi terrain chroniques
#'
#' Cette fonction permet d'extraire des données de suivi de terrain
#' 
#' @name chronique.suivi
#' @param x Variable dont on cherche le suivi (MO, opérateur, Station, Date, Capteur)
#' @param Type Type de donnée de suivi
#' @import lubridate
#' @import tidyverse
#' @export
#' @examples
#' chronique.suivi("CG39", Type = "MO")
#' chronique.suivi("JB-Stéphane", Type = "Opérateur")
#' chronique.suivi("DRO14-2", Type = "Station")
#' chronique.suivi("2015-10-23", Type = "Date")
#' chronique.suivi("9759803", Type = "Capteur")

##### TODO LIST #####
# Pour l'instant requête globale, mais transformer pour faire requêtes spécifiques à la demande
#####################

chronique.suivi <- function(x = "ORA2-7", 
                          Type = c("MO", "Opérateur", "Station", "Date", "Capteur")
                          )
{

  ## Évaluation des choix
  Type <- match.arg(Type)
  
  ## Connexion à la BDD ##
  dbD <- BDD.ouverture("Data")
  
  ## Chargement des données ##
  SuiviTerrain <- tbl(dbD, in_schema("fd_production", "chroniques_suiviterrain")) %>% collect(n = Inf) %>% arrange(chsvi_coderhj)
  #dbDisconnect(dbD)
  
  ## Formatage ##
  SuiviTerrain$chsvi_date <- ymd(SuiviTerrain$chsvi_date)
  
  ## x en tant que tel
  if(Type == "MO") {
    Vue <-
  SuiviTerrain %>% 
    filter(chsvi_mo == x) %>% 
    #filter(chsvi_operateurs == x) %>% 
    #filter(chsvi_coderhj == x) %>% 
    #filter(chsvi_date == x) %>% 
    #filter(chsvi_capteur == x) %>% 
    arrange(desc(chsvi_date))
  }
  
  if(Type == "Opérateur") {
    Vue <-
    SuiviTerrain %>% 
    #filter(chsvi_mo == x) %>% 
    filter(chsvi_operateurs == x) %>% 
    #filter(chsvi_coderhj == x) %>% 
    #filter(chsvi_date == x) %>% 
    #filter(chsvi_capteur == x) %>% 
    arrange(desc(chsvi_date))
  }
  
  if(Type == "Station") {
    Vue <-
    SuiviTerrain %>% 
    #filter(chsvi_mo == x) %>% 
    #filter(chsvi_operateurs == x) %>% 
    filter(chsvi_coderhj == x) %>% 
    #filter(chsvi_date == x) %>% 
    #filter(chsvi_capteur == x) %>% 
    arrange(desc(chsvi_date))
  }
  
  if(Type == "Date") {
    Vue <-
    SuiviTerrain %>% 
    #filter(chsvi_mo == x) %>% 
    #filter(chsvi_operateurs == x) %>% 
    #filter(chsvi_coderhj == x) %>% 
    filter(chsvi_date == x) %>% 
    #filter(chsvi_capteur == x) %>% 
    arrange(desc(chsvi_date))
  }
  
  if(Type == "Capteur") {
    Vue <-
    SuiviTerrain %>% 
    #filter(chsvi_mo == x) %>% 
    #filter(chsvi_operateurs == x) %>% 
    #filter(chsvi_coderhj == x) %>% 
    #filter(chsvi_date == x) %>% 
    filter(chsvi_capteur == x) %>% 
    arrange(desc(chsvi_date))
  }
  
  ## Affichage des résultats ##
  return(Vue)
}