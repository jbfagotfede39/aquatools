#' Extraction des données des captures de MI
#'
#' Récupère les données de captures d'une opération de suivi MI
#' @keywords donnees
#' @import dplyr RSQLite DBI lubridate
#' @export
#' @examples
#' MI.captures("VAL37-4", "2013-07-22")

MI.captures <- function(
  station="VAL37-4",
  date="2013-07-22")
{
  
  #library("RSQLite");library("dplyr");library(lubridate)
  
  ##### Connexion à la BDD #####
  ## Connexion à la BDD ##
  db <- BDD.ouverture("Macroinvertébrés")
  #BDD.sauvegarde("Macroinvertébrés")
  
  ##### Récupération des données #####
  Operations <- tbl(db,"Operations") %>% collect(n = Inf)
  Prelevements <- tbl(db,"Prelevements") %>% collect(n = Inf)
  Captures <- tbl(db,"Captures") %>% collect(n = Inf)
  
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
  date <- ymd(date)
  
  ##### Filtrage #####
  Captures <-
    Captures %>% 
    filter(CodeRDT == station, Date == date) %>%
    arrange(NumEchCommun)
  
  if(dim(Captures)[1] == 0){
  Captures <-
    Captures %>% 
    filter(CodeSIE == station, Date == date) %>%
    arrange(NumEchCommun)
  }

  return(Captures)
  
} # Fin de la fonction