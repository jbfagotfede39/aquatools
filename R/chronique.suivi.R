#' Extraction suivi terrain chroniques
#'
#' Cette fonction permet d'extraire des données de suivi de terrain
#' 
#' @param Type Type de donnée de suivi
#' @keywords 
#' @import DBI dplyr
#' @export
#' @examples
#' chronique.suivi("CG39", Type = "MO")
#' chronique.suivi("JB-Stéphane", Type = "Opérateurs")
#' chronique.suivi("DRO14-2", Type = "Station")
#' chronique.suivi("2015-10-23", Type = "Date")
#' chronique.suivi("9759803", Type = "Sonde")

##### TODO LIST #####
# 
#####################

chronique.suivi <- function(x = "ORA2-7", 
                          Type = c("MO", "Opérateurs", "Station", "Date", "Sonde")
                          )
{

  ## Évaluation des choix
  Type <- match.arg(Type)
  
  ## Connexion à la BDD ##
  db <- BDD.ouverture(Type = "Chroniques")
  
  ## Chargement des données ##
  SuiviTerrain <- dbReadTable(db, "SuiviTerrain")
  
  ## x en tant que telle
  if(Type == "MO") 
    Vue <-
  SuiviTerrain %>% 
    filter(MO == x) %>% 
    #filter(Opérateurs == x) %>% 
    #filter(CodeRDT == x) %>% 
    #filter(Date == x) %>% 
    #filter(Capteur == x) %>% 
    arrange(desc(Date))
  
  if(Type == "Opérateurs")
    Vue <-
    SuiviTerrain %>% 
    #filter(MO == x) %>% 
    filter(Opérateurs == x) %>% 
    #filter(CodeRDT == x) %>% 
    #filter(Date == x) %>% 
    #filter(Capteur == x) %>% 
    arrange(desc(Date))
  
  if(Type == "Station") 
    Vue <-
    SuiviTerrain %>% 
    #filter(MO == x) %>% 
    #filter(Opérateurs == x) %>% 
    filter(CodeRDT == x) %>% 
    #filter(Date == x) %>% 
    #filter(Capteur == x) %>% 
    arrange(desc(Date))
  
  if(Type == "Date") 
    Vue <-
    SuiviTerrain %>% 
    #filter(MO == x) %>% 
    #filter(Opérateurs == x) %>% 
    #filter(CodeRDT == x) %>% 
    filter(Date == x) %>% 
    #filter(Capteur == x) %>% 
    arrange(desc(Date))
  
  if(Type == "Sonde") 
    Vue <-
    SuiviTerrain %>% 
    #filter(MO == x) %>% 
    #filter(Opérateurs == x) %>% 
    #filter(CodeRDT == x) %>% 
    #filter(Date == x) %>% 
    filter(Capteur == x) %>% 
    arrange(desc(Date))
  
  ## Affichage des résultats ##
  return(Vue)
}