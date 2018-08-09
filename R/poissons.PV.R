#' Exportation des PV
#'
#' Cette fonction permet d'exporter les données de PV de gestion piscicole
#' 
#' @param ecosysteme Code de l'écosystème
#' @keywords poissons
#' @import dplyr 
#' @import DBI 
#' @import RSQLite
#' @import lubridate
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
  if(exists("dbP") == FALSE){
    dbP <- BDD.ouverture(Type = "Poissons")
    assign("dbP", dbP, envir = .GlobalEnv)
  }
  #dbListTables(db)
  
  ## Récupération des données ##
  Ecosystemes <- tbl(dbP,"ecosystemes") %>% collect(n = Inf)
  pv_lots <- tbl(dbP,"pv_lots") %>% collect(n = Inf)
  pv_pvs <- tbl(dbP,"pv_pvs") %>% collect(n = Inf)
  
  ## Synthèse des données ##
  pv_lots <- left_join(pv_lots, Ecosystemes, by = c("codeecosysteme" = "Codeecosysteme")) #
  pv_lots <- left_join(pv_lots, pv_pvs, by = "numero_pv") #
  
  ## Format de dates ##
  pv_lots$date_pv <- ymd(pv_lots$date_pv)
  
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