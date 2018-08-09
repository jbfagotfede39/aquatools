#' Exportation des résultats bruts de pêche
#'
#' Cette fonction permet d'exporter les résultats bruts et élaborés de pêche au format excel
#' 
#' @param station Code de la station
#' @param date Date de la pêche
#' @keywords poissons
#' @import dplyr RSQLite xlsx lubridate
#' @export
#' @examples
#' poissons.brut("SOR10-2", "2015-05-19")

poissons.brut <- function(
  station="AIN18-4",
  date="2011-09-07")
{
  
  ## Ouverture de la BDD ##
  if(exists("dbP") == FALSE){
    dbP <- BDD.ouverture(Type = "Poissons")
    assign("dbP", dbP, envir = .GlobalEnv)
  }
  
  ## Récupération des données ##
  Captures <- tbl(dbP,"captures") %>% collect(n = Inf)
  Inventaires <- tbl(dbP,"inventaires") %>% collect(n = Inf)
  Stations <- tbl(dbP,"stations") %>% collect(n = Inf)
  
  ## Synthèse des données ##
  Captures <- merge(Captures, Inventaires, by = c("codeinventaire"))
  Captures <- merge(Captures, Stations, by = c("codestation"))
  
  ## Format de dates ##
  Captures$datedebut <- ymd(Captures$datedebut)
  
  ## Simplification ##
  Captures <- 
    Captures %>%
    select(nom, datedebut, numerodepassage, codeespece, tailleminimum, taillemaximum, nombre, poids) %>% 
    filter(nom == station, datedebut == date) %>% 
    rename(Station = nom, Date = datedebut, Passage = numerodepassage, Espece = codeespece) %>% 
    arrange(Passage, Espece, nombre)
  Captures[Captures == 0] <- ""
  
  ##### Sorties des résultats traités au format Excel #####
  Resultats <- poissons.resultats.BDD() # Avec Aquatools
  Resultats$datedebut.x <- ymd(Resultats$datedebut.x)
  
  ## Résultats bruts
  Bruts <-
    Resultats %>%
    filter(nom == station, datedebut.x == date) %>%
    select(nom, datedebut.x, codeespece, n_sommecapturepassage1, n_sommecapturepassage2, n_sommecapturepassage3, nombretotalcaptures, densitenumeriquebrute, biomassetotalecapturee, densiteponderalebrute) %>%
    arrange(codeespece)
  
  Bruts$densitenumeriquebrute <- round(Bruts$densitenumeriquebrute,1)
  Bruts$densiteponderalebrute <- round(Bruts$densiteponderalebrute,1)
  
  temporaire <-
    Bruts %>% 
    summarise(codeespece = n(),
              n_sommecapturepassage1 = sum(n_sommecapturepassage1),
              n_sommecapturepassage2 = sum(n_sommecapturepassage2),
              n_sommecapturepassage3 = sum(n_sommecapturepassage3),
              nombretotalcaptures = sum(nombretotalcaptures),
              densitenumeriquebrute = sum(densitenumeriquebrute),
              biomassetotalecapturee = sum(biomassetotalecapturee),
              densiteponderalebrute = sum(densiteponderalebrute)
    )
  temporaire$nom <- "TOTAL"
  Bruts <- merge(Bruts, temporaire, all=T)
  
  Bruts <- Bruts %>% 
    select(nom, datedebut.x, codeespece, n_sommecapturepassage1, n_sommecapturepassage2, n_sommecapturepassage3, nombretotalcaptures, densitenumeriquebrute, biomassetotalecapturee, densiteponderalebrute) %>%
    dplyr::rename(Date = datedebut.x)
    colnames(Bruts) <- c("Station", "Date","Espèce","P1","P2","P3","Nb total","Ind/10a", "Biomasse (g)", "g/ha")
  
  ## Résultats élaborés
  Elabores <-
    Resultats %>%
    filter(nom == station, datedebut.x == date) %>%
    select(nom, datedebut.x, codeespece, n_sommecapturepassage1, n_sommecapturepassage2, n_sommecapturepassage3, estimationeffectifnumerique, densitenumeriqueestimee, intervalleconfiancedensitenum, estimationeffectifponderal, densiteponderaleestimee, intervalleconfiancedensitepond, coteabondancenumerique, coteabondanceponderale) %>%
    arrange(codeespece)
  
  Elabores$densitenumeriqueestimee <- round(Elabores$densitenumeriqueestimee,1)
  Elabores$intervalleconfiancedensitenum <- round(Elabores$intervalleconfiancedensitenum,1)
  Elabores$densiteponderaleestimee <- round(Elabores$densiteponderaleestimee,1)
  Elabores$intervalleconfiancedensitepond <- round(Elabores$intervalleconfiancedensitepond,1)
  
  temporaire <-
    Elabores %>% 
    summarise(codeespece = n(),
              n_sommecapturepassage1 = sum(n_sommecapturepassage1),
              n_sommecapturepassage2 = sum(n_sommecapturepassage2),
              n_sommecapturepassage3 = sum(n_sommecapturepassage3),
              estimationeffectifnumerique = sum(estimationeffectifnumerique),
              densitenumeriqueestimee = sum(densitenumeriqueestimee),
              estimationeffectifponderal = sum(estimationeffectifponderal),
              densiteponderaleestimee = sum(densiteponderaleestimee)
              )
  temporaire$nom <- "TOTAL"
  Elabores <- merge(Elabores, temporaire, all=T)
  
  # Remise en ordre des colonnes et renommage #
  
  Elabores <- Elabores %>% 
  select(nom, datedebut.x, codeespece, n_sommecapturepassage1, n_sommecapturepassage2, n_sommecapturepassage3, estimationeffectifnumerique, densitenumeriqueestimee, intervalleconfiancedensitenum, estimationeffectifponderal, densiteponderaleestimee, intervalleconfiancedensitepond, coteabondancenumerique, coteabondanceponderale) %>% 
  dplyr::rename(Date = datedebut.x)
  colnames(Elabores)<-c("Station", "Date","Espèce","P1","P2","P3","Effectif estimé","Ind/10a","IC Ind/10a","Biomasse estimée (g)","g/ha","IC g/ha", "CAN", "CAP")
  
  ###### Écriture des fichiers ######
  ## Captures ##
  write.xlsx(x = Captures, file = paste0(station, "_", date, "_captures.xlsx"),
             sheetName = paste0(station, " ", date), row.names = F)
  
  ## Résultats calculés ##
  SortieResultats <- createWorkbook()
  feuillebruts <- createSheet(wb=SortieResultats, sheetName="Bruts")
  feuillecalcules <- createSheet(wb=SortieResultats, sheetName="Calculés")
  addDataFrame(x=Bruts, sheet=feuillebruts, row.names=FALSE)
  addDataFrame(x=Elabores, sheet=feuillecalcules, row.names=FALSE)
  saveWorkbook(SortieResultats, paste0(station, "_", date, "_résultats.xlsx"))

} # Fin de la fonction