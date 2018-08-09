#' Exportation des avis à dire d'expert
#'
#' @param ListeOperations Dataframe contenant un colonne "Station" avec le code de la station (RHJ) et une colonne "Date"
#' @keywords poissons
#' @import dplyr RSQLite lubridate
#' @export
#' @examples
#' poissons.expertise(data.frame(Station = "SOR10-2", Date = "2012-11-03"))

##### TODO LIST #####

#####################

poissons.expertise <- function(
  ListeOperations = data.frame(Station = character(0), Date = character(0)))
{
  
  ## Ouverture de la BDD ##
  if(exists("dbP") == FALSE){
    dbP <- BDD.ouverture(Type = "Poissons")
    assign("dbP", dbP, envir = .GlobalEnv)
  }
  
  ## Récupération des données ##
  Operations <- tbl(dbP,"operations") %>% collect(n = Inf)
  Inventaires <- tbl(dbP,"inventaires") %>% collect(n = Inf)
  Stations <- tbl(dbP,"stations") %>% collect(n = Inf)
  
  ## Synthèse des données ##
  Operations <- merge(Operations, Inventaires, by = c("CodeInventaire"))
  Operations <- merge(Operations, Stations, by = c("CodeStation"))
  
  ## Format de dates ##
  Operations$DateDebut.x <- ymd_hms(Operations$DateDebut.x)
  Operations$DateDebut.x <- format(Operations$DateDebut.x, "%Y-%m-%d")
  
  ## Simplification ##
  Operations <- 
    Operations %>%
    rename(Station = Nom, Date = DateDebut.x, Etat = AvisExpertCourt.x, Details = AvisExpert.x) %>% 
    filter(Station %in% ListeOperations$Station & Date %in% ListeOperations$Date) %>% 
    select(Station, Date, Etat, Details) %>% 
    arrange(Station)
  
  # Test si plusieurs résultats #
  
  if(dim(Operations)[1] == 0) 
    stop("Attention : pas d'opération dans la base de données")
  
  if(dim(Operations)[1] > 1) 
    warning("Attention : plusieurs opérations dans la base de données")
  
  ## Sortie ##
  return(Operations)
  
} # Fin de la fonction