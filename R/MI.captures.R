#' Extraction des données des captures de MI
#'
#' Récupère les données de captures d'une opération de suivi MI
#' @param listeOperations Dataframe contenant un colonne "Station" avec le code de la station (RHJ) et une colonne "Date"
#' @keywords donnees
#' @import dplyr RSQLite DBI lubridate
#' @export
#' @examples
#' MI.captures()
#' MI.captures(listeOperations)
#' MI.captures(data.frame(Station = "BFE1-8", Date = ymd("2014-05-20")))
#' MI.captures(data.frame(Station = "06084000", Date = ymd("2014-05-20")))

MI.captures <- function(
  listeOperations = data.frame(Station = character(0), Date = character(0)))
{
  ##### Connexion à la BDD #####
  ## Connexion à la BDD ##
  if(exists("dbMI") == FALSE){
    dbP <- BDD.ouverture(Type = "Macroinvertébrés")
    assign("dbMI", dbP, envir = .GlobalEnv)
  }
  
  ##### Récupération des données #####
  Operations <- tbl(dbMI,"Operations") %>% collect(n = Inf)
  Prelevements <- tbl(dbMI,"Prelevements") %>% collect(n = Inf)
  Captures <- tbl(dbMI,"Captures") %>% collect(n = Inf)
  
  ##### Synthèse des données #####
  Prelevements <- left_join(Prelevements, Operations, by = c("OperationID"))

  CapturesTemp1 <- Captures %>% filter(!is.na(PrelevementID)) %>% select(-OperationID) %>% left_join(Prelevements, by = c("PrelevementID"))
  CapturesTemp2 <- Captures %>% filter(is.na(PrelevementID)) %>% left_join(Operations, by = "OperationID")
  Captures <- 
    CapturesTemp1 %>% 
    bind_rows(CapturesTemp2) %>% 
    mutate(PhaseDCE = ifelse(VolumeAbondance == "Phase A", "A", PhaseDCE)) %>% 
    mutate(PhaseDCE = ifelse(VolumeAbondance == "Phase B", "B", PhaseDCE)) %>% 
    mutate(PhaseDCE = ifelse(VolumeAbondance == "Phase C", "C", PhaseDCE))
  
  ##### Transformation des formats de dates
  Captures$Date <- ymd(Captures$Date)
  
  ##### Filtrage #####
  if(dim(listeOperations)[1] != 0){
  Captures <-
    Captures %>% 
    filter(CodeRHJ %in% listeOperations$Station, Date %in% listeOperations$Date) %>% 
    arrange(NumEchCommun)
  }
  
  if(dim(listeOperations)[1] == 0){
    Captures <-
      Captures %>% 
      arrange(NumEchCommun)
  }
  
  if(dim(Captures)[1] == 0){
  Captures <-
    Captures %>% 
    filter(CodeSIE %in% listeOperations$Station, Date %in% listeOperations$Date) %>%
    arrange(NumEchCommun)
  }

  return(Captures)
  
} # Fin de la fonction