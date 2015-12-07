#' Extraction suivi terrain thermie
#'
#' Cette fonction permet d'extraire des données de suivi de terrain
#' 
#' @param Type Type de base de données
#' @keywords 
#' @import aquatools dplyr
#' @export
#' @examples
#' thermie.suivi("Station", "ORA2-7")

##### TODO LIST #####
# 
#####################

thermie.suivi <- function(x, 
                           Type = c("MO", "Opérateurs", "Station", "Date", "Sonde"), recherche = "ORA2-7")
{

  ## Évaluation des choix
  Type <- match.arg(Type)
  
  ## Connexion à la BDD ##
  db <- BDD.ouverture(Type = "Thermie")
  
  ## Chargement des données ##
  SuiviTerrain <- dbReadTable(db, "SuiviTerrain")
  
  ## Recherche en tant que telle
  if(Type == "MO") 
  SuiviTerrain %>% 
    filter(MO == recherche) %>% 
    #filter(Opérateurs == recherche) %>% 
    #filter(CodeRDT == recherche) %>% 
    #filter(Date == recherche) %>% 
    #filter(Capteur == recherche) %>% 
    arrange(desc(Date))
  
  if(Type == "Opérateurs") 
    SuiviTerrain %>% 
    #filter(MO == recherche) %>% 
    filter(Opérateurs == recherche) %>% 
    #filter(CodeRDT == recherche) %>% 
    #filter(Date == recherche) %>% 
    #filter(Capteur == recherche) %>% 
    arrange(desc(Date))
  
  if(Type == "Station") 
    SuiviTerrain %>% 
    #filter(MO == recherche) %>% 
    #filter(Opérateurs == recherche) %>% 
    filter(CodeRDT == recherche) %>% 
    #filter(Date == recherche) %>% 
    #filter(Capteur == recherche) %>% 
    arrange(desc(Date))
  
  if(Type == "Date") 
    SuiviTerrain %>% 
    #filter(MO == recherche) %>% 
    #filter(Opérateurs == recherche) %>% 
    #filter(CodeRDT == recherche) %>% 
    filter(Date == recherche) %>% 
    #filter(Capteur == recherche) %>% 
    arrange(desc(Date))
  
  if(Type == "Sonde") 
    SuiviTerrain %>% 
    #filter(MO == recherche) %>% 
    #filter(Opérateurs == recherche) %>% 
    #filter(CodeRDT == recherche) %>% 
    #filter(Date == recherche) %>% 
    filter(Capteur == recherche) %>% 
    arrange(desc(Date))