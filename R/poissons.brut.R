#' Exportation des résultats bruts de pêche
#'
#' Cette fonction permet d'exporter les résultats bruts et élaborés de pêche au format excel
#' 
#' @param station Code de la station
#' @param date Date de la pêche
#' @keywords poissons
#' @import dplyr DBI RSQLite xlsx lubridate
#' @export
#' @examples
#' poissons.brut("SOR10-2", "2015-05-19")

poissons.brut <- function(
  station="SAI2-3",
  date="2014-09-10")
{
  
  ## Connexion à la BDD ##
  drv <- dbDriver("SQLite")
  #db <- dbConnect(drv, dbname="/Users/imac27/hubiC/Données/Poissons/Base poisson FD/MaxiFish_V3/multifish - fede39.sqlite")
  
  if (file.exists("/Users/imac27/hubiC/Données/Poissons/Base poisson FD/MaxiFish_V3/multifish - fede39.sqlite") == T) db <- dbConnect(drv, dbname="/Users/imac27/hubiC/Données/Poissons/Base poisson FD/MaxiFish_V3/multifish - fede39.sqlite")
  if (file.exists("/Users/jean-baptistefagot_FD39/hubiC/Données/Poissons/Base poisson FD/MaxiFish_V3/multifish - fede39.sqlite") == T) db <- dbConnect(drv, dbname="/Users/jean-baptistefagot_FD39/hubiC/Données/Poissons/Base poisson FD/MaxiFish_V3/multifish - fede39.sqlite")
  
  ## Récupération des données ##
  Captures <- dbReadTable(db, "Captures")
  #Operations <- dbReadTable(db, "Operations")
  Inventaires <- dbReadTable(db, "Inventaires")
  Stations <- dbReadTable(db, "Stations")
  
  ## Synthèse des données ##
  Captures <- merge(Captures, Inventaires, by = c("CodeInventaire"))
  Captures <- merge(Captures, Stations, by = c("CodeStation"))
  
  ## Format de dates ##
  Captures$DateDebut <- ymd_hms(Captures$DateDebut)
  Captures$DateDebut <- format(Captures$DateDebut, "%Y-%m-%d")
  
  ## Simplification ##
  Captures <- 
    Captures %>%
    select(Nom, DateDebut, NumeroDePassage, Codeespece, TailleMinimum, TailleMaximum, Nombre, Poids) %>% 
    filter(Nom == station, DateDebut == date) %>% 
    rename(Station = Nom, Date = DateDebut, Passage = NumeroDePassage, Espèce = Codeespece) %>% 
    arrange(Passage, Espèce, Nombre)
  Captures[Captures == 0] <- ""
  
  ##### Sorties des résultats traités au format Excel #####
  Resultats <- poissons.resultats.BDD() # Avec Aquatools
  Resultats$DateDebut.x <- ymd_hms(Resultats$DateDebut.x)
  Resultats$DateDebut.x <- format(Resultats$DateDebut.x, "%Y-%m-%d")
  
  ## Résultats bruts
  Bruts <-
    Resultats %>%
    filter(Nom == station, DateDebut.x == date) %>%
    select(Nom, DateDebut.x, Codeespece, N_SommeCapturePassage1, N_SommeCapturePassage2, N_SommeCapturePassage3, NombreTotalCaptures, DensiteNumeriqueBrute, BiomasseTotaleCapturee, DensitePonderaleBrute) %>%
    arrange(Codeespece)
  
  Bruts$DensiteNumeriqueBrute <- round(Bruts$DensiteNumeriqueBrute,1)
  Bruts$DensitePonderaleBrute <- round(Bruts$DensitePonderaleBrute,1)
  
  temporaire <-
    Bruts %>% 
    summarise(Codeespece = n(),
              N_SommeCapturePassage1 = sum(N_SommeCapturePassage1),
              N_SommeCapturePassage2 = sum(N_SommeCapturePassage2),
              N_SommeCapturePassage3 = sum(N_SommeCapturePassage3),
              NombreTotalCaptures = sum(NombreTotalCaptures),
              DensiteNumeriqueBrute = sum(DensiteNumeriqueBrute),
              BiomasseTotaleCapturee = sum(BiomasseTotaleCapturee),
              DensitePonderaleBrute = sum(DensitePonderaleBrute)
    )
  temporaire$Nom <- "TOTAL"
  Bruts <- merge(Bruts, temporaire, all=T)
  
  Bruts <- Bruts %>% 
    select(Nom, DateDebut.x, Codeespece, N_SommeCapturePassage1, N_SommeCapturePassage2, N_SommeCapturePassage3, NombreTotalCaptures, DensiteNumeriqueBrute, BiomasseTotaleCapturee, DensitePonderaleBrute) %>%
    dplyr::rename(Date = DateDebut.x)
    colnames(Bruts) <- c("Station", "Date","Espèce","P1","P2","P3","Nb total","Ind/10a", "Biomasse (g)", "g/ha")
  
  ## Résultats élaborés
  Elabores <-
    Resultats %>%
    filter(Nom == station, DateDebut.x == date) %>%
    select(Nom, DateDebut.x, Codeespece, N_SommeCapturePassage1, N_SommeCapturePassage2, N_SommeCapturePassage3, estimationeffectifNumerique, DensiteNumeriqueestimee, IntervalleConfianceDensiteNum, estimationeffectifPonderal, DensitePonderaleestimee, IntervalleConfianceDensitePond, CoteabondanceNumerique, CoteabondancePonderale) %>%
    arrange(Codeespece)
  
  Elabores$DensiteNumeriqueestimee <- round(Elabores$DensiteNumeriqueestimee,1)
  Elabores$IntervalleConfianceDensiteNum <- round(Elabores$IntervalleConfianceDensiteNum,1)
  Elabores$DensitePonderaleestimee <- round(Elabores$DensitePonderaleestimee,1)
  Elabores$IntervalleConfianceDensitePond <- round(Elabores$IntervalleConfianceDensitePond,1)
  
  temporaire <-
    Elabores %>% 
    summarise(Codeespece = n(),
              N_SommeCapturePassage1 = sum(N_SommeCapturePassage1),
              N_SommeCapturePassage2 = sum(N_SommeCapturePassage2),
              N_SommeCapturePassage3 = sum(N_SommeCapturePassage3),
              estimationeffectifNumerique = sum(estimationeffectifNumerique),
              DensiteNumeriqueestimee = sum(DensiteNumeriqueestimee),
              estimationeffectifPonderal = sum(estimationeffectifPonderal),
              DensitePonderaleestimee = sum(DensitePonderaleestimee)
              )
  temporaire$Nom <- "TOTAL"
  Elabores <- merge(Elabores, temporaire, all=T)
  
  # Remise en ordre des colonnes et renommage #
  
  Elabores <- Elabores %>% 
  select(Nom, DateDebut.x, Codeespece, N_SommeCapturePassage1, N_SommeCapturePassage2, N_SommeCapturePassage3, estimationeffectifNumerique, DensiteNumeriqueestimee, IntervalleConfianceDensiteNum, estimationeffectifPonderal, DensitePonderaleestimee, IntervalleConfianceDensitePond, CoteabondanceNumerique, CoteabondancePonderale) %>% 
  dplyr::rename(Date = DateDebut.x)
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