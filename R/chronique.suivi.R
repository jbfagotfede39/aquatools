#' Extraction suivi terrain chroniques
#'
#' Cette fonction permet d'extraire des données de suivi de terrain
#' 
#' @param Type Type de donnée de suivi
#' @import lubridate
#' @import tidyverse
#' @export
#' @examples
#' chronique.suivi("CG39", Type = "MO")
#' chronique.suivi("JB-Stéphane", Type = "Operateurs")
#' chronique.suivi("DRO14-2", Type = "Station")
#' chronique.suivi("2015-10-23", Type = "Date")
#' chronique.suivi("9759803", Type = "Sonde")

##### TODO LIST #####
# Pour l'instant requête globale, mais transformer pour faire requêtes spécifiques à la demande
#####################

chronique.suivi <- function(x = "ORA2-7", 
                          Type = c("MO", "Operateurs", "Station", "Date", "Sonde")
                          )
{

  ## Évaluation des choix
  Type <- match.arg(Type)
  
  ## Connexion à la BDD ##
  if(exists("dbC") == FALSE){
    dbC <- BDD.ouverture(Type = "Chroniques")
    assign("dbC", dbC, envir = .GlobalEnv)
  }
  
  ## Chargement des données ##
  SuiviTerrain <- tbl(db,"SuiviTerrain") %>% collect()
  
  ## Formatage ##
  SuiviTerrain$Date <- ymd(SuiviTerrain$Date)
  
  ## x en tant que tel
  if(Type == "MO") {
    Vue <-
  SuiviTerrain %>% 
    filter(MO == x) %>% 
    #filter(Operateurs == x) %>% 
    #filter(CodeRDT == x) %>% 
    #filter(Date == x) %>% 
    #filter(Capteur == x) %>% 
    arrange(desc(Date))
  }
  
  if(Type == "Operateurs") {
    Vue <-
    SuiviTerrain %>% 
    #filter(MO == x) %>% 
    filter(Operateurs == x) %>% 
    #filter(CodeRDT == x) %>% 
    #filter(Date == x) %>% 
    #filter(Capteur == x) %>% 
    arrange(desc(Date))
  }
  
  if(Type == "Station") {
    Vue <-
    SuiviTerrain %>% 
    #filter(MO == x) %>% 
    #filter(Operateurs == x) %>% 
    filter(CodeRDT == x) %>% 
    #filter(Date == x) %>% 
    #filter(Capteur == x) %>% 
    arrange(desc(Date))
  }
  
  if(Type == "Date") {
    Vue <-
    SuiviTerrain %>% 
    #filter(MO == x) %>% 
    #filter(Operateurs == x) %>% 
    #filter(CodeRDT == x) %>% 
    filter(Date == x) %>% 
    #filter(Capteur == x) %>% 
    arrange(desc(Date))
  }
  
  if(Type == "Sonde") {
    Vue <-
    SuiviTerrain %>% 
    #filter(MO == x) %>% 
    #filter(Operateurs == x) %>% 
    #filter(CodeRDT == x) %>% 
    #filter(Date == x) %>% 
    filter(Capteur == x) %>% 
    arrange(desc(Date))
  }
  
  ## Affichage des résultats ##
  return(Vue)
}