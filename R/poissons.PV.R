#' Exportation des PV
#'
#' Cette fonction permet d'exporter les données de PV de gestion piscicole
#' 
#' @param ecosysteme Code de l'écosystème
#' @keywords poissons
#' @import dplyr DBI RSQLite lubridate
#' @export
#' @examples
#' poissons.PV()
#' poissons.PV("ROU")

##### TODO LIST #####

#####################

#library(aquatools);library(dplyr);library(lubridate);library(RSQLite);ecosysteme="ROU"

poissons.PV <- function(
  ecosysteme="")
{
  
  ## Ouverture de la BDD ##
  db <- BDD.ouverture(Type = "Poissons")
  #dbListTables(db)
  
  ## Récupération des données ##
  #Captures <- dbReadTable(db, "Captures")
  #Operations <- dbReadTable(db, "Operations") %>% select(Codeoperation, AvisExpertCourt, AvisExpert)
  #IPR <- dbReadTable(db, "IPRs")
  #Inventaires <- dbReadTable(db, "Inventaires")
  #Stations <- dbReadTable(db, "Stations")
  Ecosystemes <- dbReadTable(db, "Ecosystemes")
  pv_lots <- dbReadTable(db, "pv_lots")
  pv_pvs <- dbReadTable(db, "pv_pvs")
  
  ## Synthèse des données ##
  pv_lots <- left_join(pv_lots, Ecosystemes, by = c("codeecosysteme" = "Codeecosysteme")) #
  pv_lots <- left_join(pv_lots, pv_pvs, by = "numero_pv") #
  
  ## Format de dates ##
  pv_lots$date_pv <- ymd_hms(pv_lots$date_pv)
  pv_lots$date_pv <- format(pv_lots$date_pv, "%Y-%m-%d")
  
  ## Simplification ##
  # Travail sur un seul écosystème
  if(nchar(ecosysteme) != 0){
    pv_lots <- 
      pv_lots %>%
      filter(codeRDT == ecosysteme) %>% 
      rename(Date = date_pv) %>% 
      arrange(desc(Date))}
  
  # Travail sur l'ensemble des écosystèmes
  if(nchar(ecosysteme) == 0){
    pv_lots <- 
      pv_lots %>%
      #filter(codeRDT == ecosysteme) %>% 
      rename(Date = date_pv) %>% 
      arrange(desc(Date))}

  
  return(pv_lots)
  
} # Fin de la fonction