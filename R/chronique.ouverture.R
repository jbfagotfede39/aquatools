#' Ouverture de fichiers de chroniques
#'
#' Cette fonction permet d'ouvrir de manière semi-automatisée des fichiers de chroniques
#' @name chronique.ouverture
#' @param Type Type de données en entrée (Mesures, Suivis, Stations, Commentaires, Capteurs)
#' @param typemesure Défini le type de données (Thermie, Piézométrie, etc.)
#' @param Localisation Localisation relative du fichier (à partir de /NAS-DATA/)
#' @param feuille Feuille où lire les données dans le cas de l'ouverture d'un fichier excel : \code{1} (par défaut)
#' @param skipvalue Nombre de lignes à sauter en début de fichier (1 par défaut pour les mesures)
#' @param nbcolonnes Nombre de colonnes concernées
#' @param typefichier Type de fichier : \code{.csv} (par défaut), \code{excel}, \code{.ods} ou \code{Interne}
#' @param typedate Format des dates pour les mesures (ymd par défaut, dmy, mdy, dmy_hms, dmy_hm, mdy_hms, mdy_hm ou ymd_hms)
#' @param typecapteur Type de capteur, pour les piézomètres ou les capteurs O2 (\code{Non précisé} par défaut)
#' @param formatmacmasalmo Format d'entrée nécessaire à MacmaSalmo (Heure puis date puis valeur) : \code{FALSE} (par défault)
#' @keywords chronique
#' @import glue
#' @import readODS
#' @import testit
#' @import tidyverse
#' @export
#' @examples
#' DataToAdd <- chronique.ouverture("Mesures", "Thermie", "/NAS-DATA/Chroniques/Saisie_JB/2018-12-17_Suivi_Ain_amont_FJPPMA_été_2018/10880567.txt")
#' DataToAdd <- chronique.ouverture("Stations", "Thermie", "/NAS-DATA/Chroniques/Saisie_JB/2019-01-11_PDPG/Stations sondesPDPG.xlsx")
#' chronique.ouverture("Suivi", "Thermie", "/NAS-DATA/Chroniques/Saisie_JB/2019-01-18_PDPG/Saisie_cahier_terrain_PDPG_V2.xlsx")

chronique.ouverture <- function(
  Type = c("Mesures", "Suivis", "Stations", "Commentaires", "Capteurs"),
  typemesure = c("Thermie", "Thermie barométrique", "Thermie piézométrique", "Barométrie", "Piézométrie", "Piézométrie brute", "Piézométrie compensée", "Piézométrie calée", "Piézométrie NGF", "Oxygénation", "Hydrologie", "Pluviométrie", "Télétransmission"),
  Localisation = NA_character_,
  feuille = 1,
  skipvalue = 9,
  nbcolonnes = 2,
  typefichier = c(".csv", "excel", ".ods", "Interne"),
  typedate = c("ymd", "dmy", "mdy", "dmy_hms", "dmy_hm", "mdy_hms", "mdy_hm", "ymd_hms"),
  typecapteur = c("Non précisé", "Hobo", "Diver", "VuSitu", "miniDOTindividuel", "miniDOTregroupe", "EDF", "RuggedTROLL", "Aquaread"),
  formatmacmasalmo = F
)
{

#### Évaluation des choix ####
Type <- match.arg(Type)
typemesure <- match.arg(typemesure)
typefichier <- match.arg(typefichier)
typedate <- match.arg(typedate)
typecapteur <- match.arg(typecapteur)

#### Localisation du fichier ####
if(any(class(url) == "character")) Localisation <- adresse.switch(Localisation)

#### Mesures ####
if(Type == "Mesures"){
  if(typemesure == "Thermie"){
    if(nbcolonnes == 2){
      if(typefichier == "excel"){dataaimporter <- read_excel(Localisation, sheet = feuille, skip = skipvalue, col_names = F)}
      if(typefichier == ".csv"){dataaimporter <- read_delim(Localisation, skip = skipvalue, delim=";", col_types = "cc", col_names = F)}
      if(typefichier == ".ods"){dataaimporter <- read_ods(Localisation, sheet = feuille, skip = skipvalue, col_types = "cc", col_names = F)}
      names(dataaimporter)[1] <- c('DateHeure')
      names(dataaimporter)[2] <- c('Valeur')
    }
    
    if(nbcolonnes == 3){
      if(typefichier == "excel"){dataaimporter <- read_excel(Localisation, sheet = feuille, skip = skipvalue, col_names = F)}
      if(typefichier == ".csv"){dataaimporter <- read_delim(Localisation, skip = skipvalue, delim=";", col_types = "ctc", col_names = F)}
      if(typefichier == ".ods"){dataaimporter <- read_ods(Localisation, sheet = feuille, skip = skipvalue, col_types = "ctc", col_names = F)}
    }
    
    if(nbcolonnes == 4){
      if(typefichier == "excel"){dataaimporter <- read_excel(Localisation, sheet = feuille, skip = skipvalue, col_names = F)}
      if(typefichier == "excel"){data_a_tester <- read_excel(Localisation, sheet = feuille, skip = skipvalue, col_names = T)}
      if(typefichier == ".csv"){dataaimporter <- read_delim(Localisation, skip = skipvalue, delim=";", col_types = "ctcc", col_names = F)}
      if(typefichier == ".csv"){data_a_tester <- read_delim(Localisation, skip = skipvalue, delim=";", col_types = "ctcc", col_names = T)}
      if(names(data_a_tester)[4] %in% c("chmes_validation")) names(dataaimporter)[4] <- c('chmes_validation')
      if(names(data_a_tester)[4] %in% c("chmes_mode_acquisition")) names(dataaimporter)[4] <- c('chmes_mode_acquisition')
    }
    
    if(nbcolonnes == 5){
      if(typefichier == "excel"){dataaimporter <- read_excel(Localisation, sheet = feuille, skip = skipvalue, col_names = F)}
      if(typefichier == "excel"){data_a_tester <- read_excel(Localisation, sheet = feuille, skip = skipvalue-1, col_names = T)}
      if(typefichier == ".csv"){dataaimporter <- read_delim(Localisation, skip = skipvalue, delim=";", col_types = "ctccc", col_names = F)}
      if(typefichier == ".csv"){data_a_tester <- read_delim(Localisation, skip = skipvalue-1, delim=";", col_types = "ctccc", col_names = T)}
      if(names(data_a_tester)[4] %in% c("chmes_validation")) names(dataaimporter)[4] <- c('chmes_validation')
      if(names(data_a_tester)[4] %in% c("chmes_mode_acquisition")) names(dataaimporter)[4] <- c('chmes_mode_acquisition')
      if(names(data_a_tester)[5] %in% c("chmes_validation")) names(dataaimporter)[5] <- c('chmes_validation')
      if(names(data_a_tester)[5] %in% c("chmes_mode_acquisition")) names(dataaimporter)[5] <- c('chmes_mode_acquisition')
      if(names(data_a_tester)[5] %in% c("chmes_mode_acquisition ")) names(dataaimporter)[5] <- c('chmes_mode_acquisition')
    }
    
    if(nbcolonnes == 6){
      if(typefichier == ".csv"){dataaimporter <- read_delim(Localisation, skip = skipvalue, delim=";", col_types = "ctcccc", col_names = F)}
    }
    
if(exists("dataaimporter") == FALSE) stop("Scénario d'importation à développer")

if(formatmacmasalmo == F){
  if(nbcolonnes == 3 | nbcolonnes == 4 | nbcolonnes == 5 | nbcolonnes == 6){names(dataaimporter)[1] <- c('Date')}
  if(nbcolonnes == 3 | nbcolonnes == 4 | nbcolonnes == 5 | nbcolonnes == 6){names(dataaimporter)[2] <- c('Heure')}
  if(nbcolonnes == 3 | nbcolonnes == 4 | nbcolonnes == 5 | nbcolonnes == 6){names(dataaimporter)[3] <- c('Valeur')}
  # if(nbcolonnes == 4 | nbcolonnes == 5 | nbcolonnes == 6){names(dataaimporter)[4] <- c('asup1')}
  # if(nbcolonnes == 5 | nbcolonnes == 6){names(dataaimporter)[5] <- c('asup2')}
  if(!(names(dataaimporter)[4] %in% c("chmes_validation", "chmes_mode_acquisition")) & (nbcolonnes == 4 | nbcolonnes == 5 | nbcolonnes == 6)){names(dataaimporter)[4] <- c('asup1')}
  if(!(names(dataaimporter)[5] %in% c("chmes_validation", "chmes_mode_acquisition")) & (nbcolonnes == 4 | nbcolonnes == 5 | nbcolonnes == 6)){names(dataaimporter)[5] <- c('asup2')}
  if(nbcolonnes == 6){names(dataaimporter)[6] <- c('asup3')}
  }

if(formatmacmasalmo == T){
  if(nbcolonnes == 3 | nbcolonnes == 4 | nbcolonnes == 5 | nbcolonnes == 6){names(dataaimporter)[1] <- c('Heure')}
  if(nbcolonnes == 3 | nbcolonnes == 4 | nbcolonnes == 5 | nbcolonnes == 6){names(dataaimporter)[2] <- c('Date')}
  if(nbcolonnes == 3 | nbcolonnes == 4 | nbcolonnes == 5 | nbcolonnes == 6){names(dataaimporter)[3] <- c('Valeur')}
  }

## Nettoyage ##
if(testit::has_warning(ymd(dataaimporter$Date)) == FALSE & typedate == "ymd") dataaimporter$Date <- ymd(dataaimporter$Date)
if(testit::has_warning(dmy(dataaimporter$Date)) == FALSE & typedate == "dmy") dataaimporter$Date <- dmy(dataaimporter$Date)
if(testit::has_warning(mdy(dataaimporter$Date)) == FALSE & typedate == "mdy") dataaimporter$Date <- mdy(dataaimporter$Date)
if(testit::has_warning(ymd_hms(dataaimporter$DateHeure)) == FALSE & typedate == "ymd_hms"){
  dataaimporter <-
    dataaimporter %>% 
    mutate(DateHeure = ymd_hms(DateHeure)) %>% 
    mutate(Date = format(DateHeure, format="%Y-%m-%d")) %>% 
    mutate(Heure = format(DateHeure, format="%H:%M:%S"))
}
if(testit::has_warning(dmy_hm(dataaimporter$DateHeure)) == FALSE & typedate == "dmy_hm"){
  dataaimporter <-
    dataaimporter %>% 
    mutate(DateHeure = dmy_hm(DateHeure)) %>% 
    mutate(Date = format(DateHeure, format="%Y-%m-%d")) %>% 
    mutate(Heure = format(DateHeure, format="%H:%M:%S"))
}
if(testit::has_warning(dmy_hms(dataaimporter$DateHeure)) == FALSE & typedate == "dmy_hms"){
  dataaimporter <-
    dataaimporter %>% 
    mutate(DateHeure = dmy_hms(DateHeure)) %>% 
    mutate(Date = format(DateHeure, format="%Y-%m-%d")) %>% 
    mutate(Heure = format(DateHeure, format="%H:%M:%S"))
}
if(testit::has_warning(mdy_hms(dataaimporter$DateHeure)) == FALSE & typedate == "mdy_hms"){
  dataaimporter <-
    dataaimporter %>% 
    mutate(DateHeure = mdy_hms(DateHeure)) %>% 
    mutate(Date = format(DateHeure, format="%Y-%m-%d")) %>% 
    mutate(Heure = format(DateHeure, format="%H:%M:%S"))
}
if(testit::has_warning(mdy_hm(dataaimporter$DateHeure)) == FALSE & typedate == "mdy_hm"){
  dataaimporter <-
    dataaimporter %>% 
    mutate(DateHeure = mdy_hm(DateHeure)) %>% 
    mutate(Date = format(DateHeure, format="%Y-%m-%d")) %>% 
    mutate(Heure = format(DateHeure, format="%H:%M:%S"))
}

# Cas où les heures sont au format 1899-12-31 00:00:00 (POSIXct avec str ou dttm en tibble) :
if(inherits(dataaimporter$Heure, "POSIXct")){ #dataaimporter[[2]] car avec macma on peut avoir un ordre de champs différents
  dataaimporter <-
    dataaimporter %>% 
    mutate(Heure = format(Heure, format="%H:%M:%S"))
}

dataaimporter <-
  dataaimporter %>% 
  {if ("chmes_validation" %in% names(.)) rename(., "validation" = "chmes_validation") else .} %>%
  {if ("chmes_mode_acquisition" %in% names(.)) rename(., "mode_acquisition" = "chmes_mode_acquisition") else .} %>%
  {if ("validation" %in% names(.) & "mode_acquisition" %in% names(.)) dplyr::select(., Date, Heure, Valeur, validation, mode_acquisition) else .} %>%
  {if ("validation" %in% names(.) & !("mode_acquisition" %in% names(.))) dplyr::select(., Date, Heure, Valeur, validation) else .} %>%
  {if ("mode_acquisition" %in% names(.) & !("validation" %in% names(.))) dplyr::select(., Date, Heure, Valeur, mode_acquisition) else .} %>%
  {if (!("validation" %in% names(.) & "mode_acquisition" %in% names(.))) dplyr::select(., Date, Heure, Valeur) else .} %>%
  # dplyr::select(Date, Heure, Valeur) %>% 
  mutate(Date = format(Date, format="%Y-%m-%d")) %>% 
  mutate(Date = ymd(Date)) %>% 
  mutate(Heure = as.character(Heure)) %>% 
  mutate(Valeur = str_replace(Valeur, " °C", "")) %>% 
  mutate(Valeur = str_replace(Valeur, "°C", "")) %>% 
  mutate(Valeur = as.numeric(sub(",", ".", Valeur))) %>% 
  mutate(Valeur = round(Valeur,3)) %>% 
  filter(is.na(Valeur) != T) %>% 
  mutate(typemesure = "Thermie") %>% 
  mutate(unite = "°C")
  }
  
if(typemesure == "Piézométrie"){
  if(typecapteur == "Non précisé"){
  typecapteur = readline(prompt = "Type de capteur piézométrique : 1 (Hobo) ou 2 (Diver) ou 3 (VuSitu) ou 4 (RuggedTROLL) : ")
  if (!(typecapteur == 1 | typecapteur == 2 | typecapteur == 3 | typecapteur == 4)) {stop("Valeur non disponible")}
  if (typecapteur == 1) {typecapteur <- "Hobo"}
  if (typecapteur == 2) {typecapteur <- "Diver"}
  if (typecapteur == 3) {typecapteur <- "VuSitu"}
  if (typecapteur == 4) {typecapteur <- "RuggedTROLL"}
  }
  typedonnee = readline(prompt = "Type de mesure piézométrique 1 (Baro) ou 2 (Piézo) : ")
  typedonnee <- ifelse(typedonnee == 1, "Baro", "Piézo")
  
  if(typecapteur == "Diver"){
    dataaimporter <- 
      read_csv2(Localisation, skip = 54, col_names = c("Date","Piézométrie", "Thermie")) %>% 
      filter(Piézométrie != 4133.6) %>% # Car fin de chronique remplie avec cette valeur
      filter(Thermie != 193.35) %>% # Car fin de chronique remplie avec cette valeur
      mutate(Date = ymd_hms(Date)) %>%
      mutate(Heure = format(Date, format="%H:%M:%S")) %>% 
      mutate(Datefine = Date) %>% 
      mutate(Date = ymd(format(Date, format="%Y-%m-%d")))
  }
  if(typecapteur == "Hobo"){
    dataaimporter <- 
      read_csv2(Localisation, skip = 2, col_names = c("Date","Heure","Piézométrie", "Thermie")) %>% 
      {if(typedate == "dmy") mutate(., Date = as.character(format(dmy(Date), format="%Y-%m-%d"))) else .} %>% 
      mutate(Date = ymd(Date)) %>% 
      dplyr::select(Date, Heure, Piézométrie, Thermie)
      
  }
  if(typecapteur == "VuSitu"){
    dataaimporter <- 
      read_csv2(Localisation, skip = 1, col_names = c("Date","Pression","Thermie", "Piézométrie"), col_types = "cccc") %>%
      mutate(Thermie = as.numeric(sub(",", ".", Thermie))) %>% 
      mutate(Piézométrie = as.numeric(sub(",", ".", Piézométrie))) %>% 
      mutate(Date = ymd_hms(Date)) %>%
      mutate(Heure = format(Date, format="%H:%M:%S")) %>% 
      mutate(Datefine = Date) %>% 
      mutate(Date = ymd(format(Date, format="%Y-%m-%d"))) %>% 
      dplyr::select(Date, Heure, Piézométrie, Thermie)
  }
  if(typecapteur == "RuggedTROLL"){
    dataaimporter <- 
      read_csv2(Localisation, skip = 70, col_names = c("Date", "Heure", "Seconde", "Thermie", "Profondeur", "Piézométrie"), col_types = "cccccc") %>%
      mutate(Thermie = as.numeric(sub(",", ".", Thermie))) %>% 
      mutate(Piézométrie = as.numeric(sub(",", ".", Piézométrie))) %>% 
      mutate(Date = ymd(Date)) %>%
      dplyr::select(Date, Heure, Piézométrie, Thermie)
  }
  
  dataaimporter <- 
    dataaimporter %>% 
    filter(!is.na(Thermie)) %>% 
    mutate(Heure = as.character(Heure)) %>% 
    tidyr::gather(typemesure, Valeur, Piézométrie:Thermie) %>% 
    mutate(unite = ifelse(typemesure == "Thermie", "°C", NA_character_)) %>% 
    mutate(unite = ifelse(typemesure == "Piézométrie" & (typecapteur == "Hobo" | typecapteur == "RuggedTROLL"), "kPa", unite)) %>% 
    mutate(unite = ifelse(typemesure == "Piézométrie" & (typecapteur == "Diver" | typecapteur == "VuSitu"), "cm H2O", unite)) %>% 
    mutate(typemesure = ifelse(typemesure == "Thermie" & typedonnee == "Baro", "Thermie barométrique", typemesure)) %>% 
    mutate(typemesure = ifelse(typemesure == "Thermie" & typedonnee == "Piézo", "Thermie piézométrique", typemesure)) %>% 
    mutate(typemesure = ifelse(typemesure == "Piézométrie" & typedonnee == "Baro", "Barométrie", typemesure)) %>% 
    mutate(typemesure = ifelse(typemesure == "Piézométrie" & typedonnee == "Piézo", "Piézométrie brute", typemesure))
}
  
  if(typemesure == "Télétransmission"){
    if(typecapteur == "Non précisé"){
    ## Définition du point de suivi :
    station <- stringr::str_extract_all(Localisation, pattern = "[:alnum:]+__", simplify = FALSE)[[1]][1] %>% str_replace("__", "")
    if(nchar(station) == 1){station <- stringr::str_extract_all(Localisation, pattern = "[:alnum:]+-[:alnum:]+__", simplify = FALSE)[[1]][1] %>% str_replace("__", "")}
    
    Contexte <- readChar(adresse.switch(Localisation), file.info(adresse.switch(Localisation))$size)
    debutData <- str_split(Contexte, pattern = "Record n", simplify = FALSE)[[1]][1] %>% str_count(pattern = "\n")+1
    nData <- str_split(Contexte, pattern = "-----------", simplify = FALSE)[[1]][1] %>% str_count(pattern = "\n")-debutData
    nColonnes <- str_split(Contexte, pattern = "Record n", simplify = FALSE)[[1]][2]
    nColonnes <- str_split(nColonnes, pattern = "\n", simplify = FALSE)[[1]][1] %>% str_count(pattern = ",")+1
    emetteur <- stringr::str_extract_all(Contexte, pattern = "s/n:[0-9]+", simplify = FALSE)[[1]][1] %>% str_replace("^s/n:", "")
    capteur <- stringr::str_extract_all(Contexte, pattern = "s/n:[0-9]+", simplify = FALSE)[[1]][2] %>% str_replace("^s/n:", "")
    dateCompleteTeletransmission <- dmy_hms(str_split(Contexte, pattern = "\n", simplify = FALSE)[[1]][6], locale = "en_US.UTF-8")
    dateTeletransmission <- format(dateCompleteTeletransmission, format="%Y-%m-%d")
    heureTeletransmission <- format(dateCompleteTeletransmission, format="%H:%M:%S")
    valeurTempTeletransmission <- str_split(Contexte, pattern = "\n", simplify = FALSE)[[1]][debutData + nData + 3]
    valeurTempTeletransmission <- str_split(valeurTempTeletransmission, pattern = "- Temperature:  ", simplify = FALSE)[[1]][2]
    valeurTempTeletransmission <- as.numeric(str_split(valeurTempTeletransmission, pattern = " \xb0C\r", simplify = FALSE)[[1]][1])
    
    ## Importation des données ##
    dataaimporter <- 
      read_delim(Localisation, skip = debutData, delim=",", col_types = str_dup("c", nColonnes), col_names = F, n_max = nData) %>% 
      rename(Date = "X2") %>% 
      filter(!is.na(Date)) %>% 
      mutate(Date = dmy(Date)) %>% 
      mutate(Date = as.character(format(Date, format="%Y-%m-%d"))) #%>% 
      #mutate(.[[3]] = as.numeric( sub(",", ".", .[[3]])))
    
    ## Travail des données de VOU ##
    if(station == "VOUmercantine" | station == "VOUsurchauffant" | station == "AIN22-2" | station == "AIN37-2"){
      dataaimporterPartie1 <-
        dataaimporter %>%
        dplyr::select(2:3,5) %>%
        rename(Heure = 2, Valeur = 3) %>%
        mutate(Capteur = emetteur) %>%
        mutate(Valeur = as.numeric( sub(",", ".", Valeur))) %>% 
        mutate(typemesure = "Barométrie") %>%
        mutate(unite = "mBar")

      dataaimporterPartie2 <-
        dataaimporter %>%
        dplyr::select(2,3,8) %>%
        rename(Heure = 2, Valeur = 3) %>%
        mutate(Capteur = capteur) %>%
        mutate(Valeur = as.numeric( sub(",", ".", Valeur))) %>% 
        mutate(typemesure = "Thermie") %>%
        mutate(unite = "°C")
    }
    
    ## Travail des données de l'Ain à Ney ##
      if(station == "AIN22-2"){
        # Piézo brute
        dataaimporterPartie3 <-
          dataaimporter %>%
          dplyr::select(2,3,7) %>%
          rename(Heure = 2, Valeur = 3) %>%
          mutate(Capteur = capteur) %>%
          mutate(Valeur = as.numeric( sub(",", ".", Valeur))) %>% 
          mutate(typemesure = "Piézométrie brute") %>%
          mutate(unite = "cmH2O")
        
        # Piézo nette
        dataaimporterPartie4 <-
          dataaimporter %>%
          dplyr::select(2,3,9) %>%
          rename(Heure = 2, Valeur = 3) %>%
          mutate(Capteur = capteur) %>%
          mutate(Valeur = as.numeric( sub(",", ".", Valeur))) %>% 
          mutate(typemesure = "Piézométrie compensée") %>%
          mutate(unite = "cmH2O")
        
        dataaimporterPartie2 <-
          dataaimporterPartie2 %>%
          bind_rows(dataaimporterPartie3) %>%
          bind_rows(dataaimporterPartie4)
        
      }
      
    ## Travail des données de l'Ain à Marigny ##
    if(station == "AIN37-2"){
      capteurQualite <- stringr::str_extract_all(Contexte, pattern = "s/n:[0-9]+", simplify = FALSE)[[1]][3] %>% str_replace("^s/n:", "")
      
      # O2 concentration
      dataaimporterPartie3 <-
        dataaimporter %>%
        dplyr::select(2,3,7) %>%
        rename(Heure = 2, Valeur = 3) %>%
        mutate(Capteur = capteur) %>%
        mutate(Valeur = as.numeric( sub(",", ".", Valeur))) %>% 
        mutate(typemesure = "Oxygénation") %>%
        mutate(unite = "mg/L")
      
      # O2 saturation
      dataaimporterPartie4 <-
        dataaimporter %>%
        dplyr::select(2,3,9) %>%
        rename(Heure = 2, Valeur = 3) %>%
        mutate(Capteur = capteur) %>%
        mutate(Valeur = as.numeric( sub(",", ".", Valeur))) %>% 
        mutate(typemesure = "Oxygénation") %>%
        mutate(unite = "%")
      
      # O2 pression
      dataaimporterPartie5 <-
        dataaimporter %>%
        dplyr::select(2,3,10) %>%
        rename(Heure = 2, Valeur = 3) %>%
        mutate(Capteur = capteur) %>%
        mutate(Valeur = as.numeric( sub(",", ".", Valeur))) %>% 
        mutate(typemesure = "Oxygénation") %>%
        mutate(unite = "torr")
      
      
      #### Baromètre et capteur O2 traités -> reste sonde qualité à faire au moins pour la piézo brute + compensée
      
      
      # Piézo brute
      if(ncol(dataaimporter) > 24){ # Sinon il n'y a pas les données issues de l'Aquatroll (pas assez d'énergie par exemple)
      # dataaimporterPartie6 <-
      #   dataaimporter %>%
      #   dplyr::select(2,3,28) %>%
      #   rename(Heure = 2, Valeur = 3) %>%
      #   mutate(Capteur = capteur) %>%
      #   mutate(Valeur = as.numeric( sub(",", ".", Valeur))) %>% 
      #   mutate(typemesure = "Piézométrie brute") %>%
      #   mutate(unite = "kPa")
      
      # Piézo nette
      dataaimporterPartie7 <-
        dataaimporter %>%
        # dplyr::select(2,3,31) %>%
        dplyr::select(2,3,30) %>%
        rename(Heure = 2, Valeur = 3) %>%
        mutate(Capteur = capteur) %>%
        mutate(Valeur = as.numeric( sub(",", ".", Valeur))) %>% 
        mutate(typemesure = "Piézométrie compensée") %>%
        mutate(unite = "cmH2O")
      }
      
      dataaimporterPartie2 <-
        dataaimporterPartie2 %>%
        bind_rows(dataaimporterPartie3) %>%
        bind_rows(dataaimporterPartie4) %>%
        bind_rows(dataaimporterPartie5) %>%
        # {if(ncol(dataaimporter) > 24) bind_rows(., dataaimporterPartie6) else .} %>%
        {if(ncol(dataaimporter) > 24) bind_rows(., dataaimporterPartie7) else .} # %>%  Nutriments, conductivité, turbidité à ajouter ensuite
        #bind_rows(dataaimporterPartie8)
      
    }
      
      ## Regroupement des parties de données ##
      dataaimporter <-
        dataaimporterPartie1 %>%
        bind_rows(dataaimporterPartie2) %>%
        add_row(Date = dateTeletransmission, Heure = heureTeletransmission, Valeur = valeurTempTeletransmission, Capteur = emetteur, typemesure = "Thermie barométrique", unite = "°C") %>% 
        #rowwise() %>% 
        #mutate(Heure = ifelse(nchar(Heure == 5, paste0(Heure, ":00")))) %>% 
        mutate(Valeur = as.numeric(sub(",", ".", Valeur))) %>%
        mutate(Valeur = round(Valeur,3)) %>% 
        mutate(coderhj = station)
    
    }
    
    if(typecapteur == "EDF"){
        dataaimporter <- 
          read_csv(Localisation, skip = 1) %>% 
          rename(complet = !!names(.[1])) %>% 
          filter(!grepl("FIN", complet)) %>% 
          mutate(ligne = row_number())
        
        dataaimporter <-
          dataaimporter %>% 
          filter(grepl("SID", complet)) %>% 
          full_join(tibble(ligne = seq(1, max(dataaimporter$ligne), by = 1)), by = "ligne") %>%
          arrange(ligne) %>%
          fill(complet) %>% 
          rename(localisation = complet) %>% 
          right_join(dataaimporter, by = "ligne") %>% 
          dplyr::select(-ligne) %>% 
          filter(!grepl("SID", complet)) %>% 
          tidyr::separate(localisation, c("code1", "codemo", "coderhj", "typemesure", "unite", "code3"), sep = ';') %>% 
          mutate(complet = str_sub(complet, 1, -4)) %>% # Suppression du dernier ; en fin de ligne, car pose pb ensuite
          tidyr::separate(complet, c("code11", "DateHeure", "valeur"), sep = ';') %>%
          mutate(DateHeure = dmy_hm(DateHeure)) %>% 
          mutate(Date = ymd(format(DateHeure, format="%Y-%m-%d"))) %>% 
          mutate(Heure = format(DateHeure, format="%H:%M:%S")) %>% 
          dplyr::select(Date, Heure, coderhj, typemesure, valeur)
        
        dataaimporter <- 
          dataaimporter %>% 
          filter(!is.na(valeur)) %>% 
          mutate(Heure = as.character(Heure)) %>% 
          mutate(typemesure = case_when(typemesure == "PL" ~ "Pluviométrie",
                                        typemesure == "DE" ~ "Hydrologie",
                                        typemesure == "TA" ~ "Thermie barométrique",
                                        typemesure == "TE" ~ "Thermie",
                                        typemesure == "CD" ~ "Conductivité",
                                        typemesure == "O2" ~ "Oxygénation")
                 ) %>% 
          mutate(unite = case_when(typemesure == "Pluviométrie" ~ "mm",
                                   typemesure == "Hydrologie" ~ "m3/s",
                                   typemesure == "Thermie barométrique" ~ "°C",
                                   typemesure == "Thermie" ~ "°C",
                                   typemesure == "Conductivité" ~ "µS/cm",
                                   typemesure == "Oxygénation" ~ "mg/L")
          )
    }
    
    if(typecapteur == "Aquaread"){
      ## Définition du point de suivi
      modem <- basename(Localisation) %>% stringr::str_extract(pattern = "[:alnum:]+")
      capteur <- ifelse(modem == "215640287", "21584010", NA_character_)
      station <- ifelse(modem == "215640287", "ILA", NA_character_)
      modem;capteur;station
      
      ## Importation des données
      dataaimporter <-
        read_csv(Localisation) %>% 
        rename(Heure = Time) %>%
        rename(`Piézométrie compensée` = `Level(m)`) %>%
        rename(`Thermie piézométrique` = `Water Temperature(C)`) %>%
        rename(`Piézométrie brute` = `Water Pressure(mB)`) %>%
        rename(`Thermie barométrique` = `Air Temperature(C)`) %>%
        rename(`Barométrie` = `Air Pressure(mB)`) %>%
        select(Date, Heure, `Piézométrie compensée`, `Thermie piézométrique`, `Piézométrie brute`, `Thermie barométrique`, `Barométrie`) %>% 
        pivot_longer(-c(Date, Heure), names_to = "typemesure", values_to = "Valeur") %>% 
        mutate(Date = dmy(Date)) %>% 
        mutate(Capteur = capteur) %>%
        mutate(Valeur = as.numeric( sub(",", ".", Valeur))) %>% 
        mutate(Valeur = ifelse(typemesure == "Piézométrie compensée", Valeur*100, Valeur)) %>% 
        mutate(unite = case_when(typemesure == "Piézométrie compensée" ~ "cm H2O",
                                 typemesure == "Piézométrie brute" ~ "mBar",
                                 typemesure == "Thermie piézométrique" ~ "°C",
                                 typemesure == "Thermie barométrique" ~ "°C",
                                 typemesure == "Barométrie" ~ "mBar")
        )
    }
      
  }
  
  if(typemesure == "Oxygénation"){
    if(typecapteur == "Non précisé"){
    typecapteur = readline(prompt = "Type de capteur oxygène : 1 (Hobo) ou 2 (miniDOTindividuel) ou 3 (miniDOTregroupe) : ")
    if (!(typecapteur == 1 | typecapteur == 2)) {stop("Valeur non disponible")}
    if (typecapteur == 1) {typecapteur <- "Hobo"}
    if (typecapteur == 2) {typecapteur <- "miniDOTindividuel"}
    if (typecapteur == 3) {typecapteur <- "miniDOTregroupe"}
    }
    
    if(typecapteur == "miniDOTindividuel" | typecapteur == "miniDOTregroupe"){
      if (typecapteur == "miniDOTindividuel") dataaimporter <- read_csv(Localisation, skip = 3, col_names = c("Time","Tension","Thermie", "Concentration", "Saturation"), col_types = "ddddd")
      if (typecapteur == "miniDOTregroupe") dataaimporter <- read_csv(Localisation, skip = 9, col_names = c("Time", "Time2", "Time3", "Tension", "Thermie", "Concentration", "Saturation", "Autre"), col_types = "dccddddd")
      dataaimporter <- 
        dataaimporter %>% 
        mutate(Time = as_datetime(Time)) %>% 
        mutate(Date = ymd(format(Time, format="%Y-%m-%d"))) %>% 
        mutate(Heure = format(Time, format="%H:%M:%S")) %>% 
        dplyr::select(Date, Heure, Concentration, Thermie, Saturation)

      if(typecapteur == "miniDOTindividuel") dataaimporter <- dataaimporter %>% dplyr::select(-Saturation) %>% PC.saturationO2()
    }
    
    dataaimporter <- 
      dataaimporter %>% 
      filter(!is.na(Thermie)) %>% 
      mutate(Heure = as.character(Heure)) %>% 
      tidyr::gather(typemesure, Valeur, Concentration:Saturation) %>% 
      mutate(unite = ifelse(typemesure == "Thermie", "°C", NA_character_)) %>% 
      mutate(unite = ifelse(typemesure == "Concentration", "mg/L", unite)) %>% 
      mutate(unite = ifelse(typemesure == "Saturation", "%", unite)) %>% 
      mutate(typemesure = ifelse(grepl("Concentration|Saturation", typemesure), "Oxygénation", typemesure))
  }
  
  if(typemesure == "Pluviométrie"){
  }
  
  if(typemesure == "Hydrologie"){ 

    # Format récupéré depuis l'API Hub'Eau en .csv :
    if(typefichier == "Interne"){
      dataaimporter <- Localisation
      
      dataaimporter <-
        dataaimporter %>% 
        # rename_all(~sub("<", "", .x)) %>%
        # rename_all(~sub(">", "", .x)) %>%
        mutate(DateHeure = ymd_hms(date_obs)) %>% 
        mutate(Date = format(DateHeure, format="%Y-%m-%d")) %>% 
        mutate(Heure = format(DateHeure, format="%H:%M:%S")) %>% 
        mutate(coderhj = code_site) %>%
        mutate(capteur = "Sonde DREAL") %>%
        mutate(Valeur = resultat_obs/10000) %>% # Les données en sortie de l'API sont en L/s, donc on convertit en m3/s. Il faut toutefois diviser par 10000 au lieu de 1000 car les données en sortie sont au format 903.0 et le .0 est mangé par read_csv2 et devient 9030.
        # mutate(unite = "L/s") %>%
        mutate(unite = "m3/s") %>%
        mutate(typemesure = glue("Hydrologie - {grandeur_hydro}")) %>% 
        mutate(validation = libelle_qualification_obs) %>% 
        mutate(mode_acquisition = libelle_methode_obs) %>% 
        filter(is.na(Valeur) != T) %>% 
        select(coderhj, capteur, Date, Heure, Valeur, unite, typemesure, validation, mode_acquisition)
    } # Fin d'importation d'un format récupéré depuis l'API Hub'Eau en .csv :
    
    # Format exporté/téléchargé de manière classique via un navigateur depuis hydroportail en .csv :
    if(typefichier == ".csv"){
    dataaimporter <- read_csv2(Localisation, show_col_types = FALSE) %>% filter(row_number() > 1)
    
    dataaimporter <-
      dataaimporter %>% 
      rename_all(~sub("<", "", .x)) %>%
      rename_all(~sub(">", "", .x)) %>%
      mutate(DateHeure = ymd_hms(DtObsElaborHydro)) %>% 
      mutate(Date = format(DateHeure, format="%Y-%m-%d")) %>% 
      mutate(Heure = format(DateHeure, format="%H:%M:%S")) %>% 
      mutate(coderhj = CdSiteHydro) %>%
      mutate(capteur = "Sonde DREAL") %>%
      mutate(Valeur = as.numeric(sub(",", ".", ResObsElaborHydro))) %>% 
      mutate(unite = "L/s") %>%
      mutate(typemesure = glue("Hydrologie - {TypDeGrdSerieObsElaborHydro}")) %>% 
      mutate(validation = QualifObsElaborHydro) %>% 
      mutate(mode_acquisition = MethObsElaborHydro) %>% 
      filter(is.na(Valeur) != T) %>% 
      select(coderhj, capteur, Date, Heure, Valeur, unite, typemesure, validation, mode_acquisition)
    } # Fin d'importation d'un format exporté/téléchargé de manière classique via un navigateur depuis hydroportail en .csv
  } # Fin d'importation de données d'hydrologie

## Tri ##
dataaimporter <- 
  dataaimporter %>% 
  arrange(Date, Heure)
    
## Transformation des champs ##
dataaimporter <- 
  dataaimporter %>% 
  rename_all(list(~stringi::stri_trans_general(., "latin-ascii"))) %>% # Pour remplacer les caractères accentués par les mêmes sans accents
  rename_all(list(~paste0("chmes_",.))) %>% 
  rename_all(list(~gsub("[[:punct:]]", "_", .))) %>% 
  rename_all(list(~tolower(.)))
}

#### Suivis ####
if(Type == "Suivis"){
SuiviTerrain <- 
  structure(list(id = numeric(0), chsvi_mo = logical(0), chsvi_coderhj = character(0), 
                 chsvi_typesuivi = character(0), chsvi_operateurs = logical(0), 
                 chsvi_date = logical(0), chsvi_heure = logical(0), chsvi_capteur = logical(0), 
                 chsvi_valeur = logical(0), chsvi_unite = character(0), chsvi_profondeur = logical(0), chsvi_action = logical(0), 
                 chsvi_fonctionnement = logical(0), chsvi_qualite = character(0), 
                 chsvi_actionafaire = logical(0), chsvi_remarques = logical(0), 
                 `_modif_utilisateur` = logical(0), `_modif_type` = logical(0), 
                 `_modif_date` = logical(0)), class = c("tbl_df", "tbl", "data.frame"
                 ), row.names = c(NA, 0L))

dataaimporter <- read_excel(Localisation, sheet = feuille)

## Renommage des champs ##
dataaimporter <- 
  dataaimporter %>% 
  rename_at(vars(matches("Valeur")), list(~str_replace(., "Valeur", "chsvi_valeur"))) %>%
  rename_at(vars(matches("Valeur manuelle")), list(~str_replace(., "Valeur manuelle", "chsvi_valeur"))) %>%
  rename_at(vars(matches("Profondeur")), list(~str_replace(., "Profondeur", "chsvi_profondeur"))) %>%
  rename_at(vars(matches("Température manuelle")), list(~str_replace(., "Température manuelle", "chsvi_valeur"))) %>%
  rename_at(vars(matches("Tmanuelle")), list(~str_replace(., "Tmanuelle", "chsvi_valeur"))) %>%
  rename_at(vars(matches("MO")), list(~str_replace(., "MO", "chsvi_mo"))) %>%
  rename_at(vars(matches("Maître d'ouvrage")), list(~str_replace(., "Maître d'ouvrage", "chsvi_mo"))) %>%
  rename_at(vars(ends_with("Operateurs")), list(~str_replace(., "Operateurs", "chsvi_operateurs"))) %>%
  rename_at(vars(ends_with("Opérateur")), list(~str_replace(., "Opérateur", "chsvi_operateurs"))) %>%
  rename_at(vars(ends_with("Opérateurs")), list(~str_replace(., "Opérateurs", "chsvi_operateurs"))) %>%
  rename_at(vars(matches("CodeRDT")), list(~str_replace(., "CodeRDT", "chsvi_coderhj"))) %>%
  rename_at(vars(matches("Station")), list(~str_replace(., "Station", "chsvi_coderhj"))) %>%
  rename_at(vars(matches("TypeSuivi")), list(~str_replace(., "TypeSuivi", "chsvi_typesuivi"))) %>%
  rename_at(vars(matches("Date")), list(~str_replace(., "Date", "chsvi_date"))) %>%
  rename_at(vars(matches("Heure")), list(~str_replace(., "Heure", "chsvi_heure"))) %>%
  rename_at(vars(matches("Capteur")), list(~str_replace(., "Capteur", "chsvi_capteur"))) %>%
  rename_at(vars(matches("Numéro sonde")), list(~str_replace(., "Numéro sonde", "chsvi_capteur"))) %>%
  rename_at(vars(matches("Unité")), list(~str_replace(., "Unité", "chsvi_unite"))) %>%
  rename_at(vars(matches("Unite")), list(~str_replace(., "Unite", "chsvi_unite"))) %>%
  rename_at(vars(matches("Action")), list(~str_replace(., "Action", "chsvi_action"))) %>%
  rename_at(vars(matches("Fonctionnement")), list(~str_replace(., "Fonctionnement", "chsvi_fonctionnement"))) %>%
  rename_at(vars(matches("Qualité")), list(~str_replace(., "Qualité", "chsvi_qualite"))) %>%
  rename_at(vars(matches("Qualite")), list(~str_replace(., "Qualite", "chsvi_qualite"))) %>%
  rename_at(vars(starts_with("Remarque")), list(~str_replace(., "Remarque", "chsvi_remarques"))) %>%
  rename_at(vars(matches("AFaire")), list(~str_replace(., "AFaire", "chsvi_actionafaire"))) %>%
  rename_at(vars(matches("chsvi_action à réaliser lors de la prochaine visite")), list(~str_replace(., "chsvi_action à réaliser lors de la prochaine visite", "chsvi_actionafaire"))) %>%
  rename_at(vars(matches("ToDo")), list(~str_replace(., "ToDo", "chsvi_actionafaire"))) %>%
  rename_at(vars(matches("A faire Printemps 2019")), list(~str_replace(., "A faire Printemps 2019", "chsvi_actionafaire"))) %>%
  dplyr::select(-contains("SuiviTerrainID")) %>% 
  dplyr::select(-contains("saisie")) %>% 
  mutate('_modif_utilisateur' = NA) %>% 
  mutate('_modif_type' = NA) %>% 
  mutate('_modif_date' = NA)
  
## Complément des données ##
dataaimporter <- 
  dataaimporter %>% 
  rowwise() %>% 
  mutate(chsvi_typesuivi = ifelse("chsvi_typesuivi" %in% names(.) & typemesure == "Thermie", chsvi_typesuivi, "Thermie")) %>% 
  mutate(chsvi_unite = ifelse("chsvi_unite" %in% names(.) & typemesure == "Thermie", chsvi_unite, "°C")) %>% 
  mutate(chsvi_qualite = ifelse("chsvi_qualite" %in% names(.), chsvi_qualite, as.character(NA))) %>% 
  mutate(chsvi_profondeur = ifelse("chsvi_profondeur" %in% names(.), chsvi_profondeur, as.character(NA))) %>% 
  mutate(chsvi_actionafaire = ifelse("chsvi_actionafaire" %in% names(.), chsvi_actionafaire, as.character(NA))) %>% 
  ungroup() %>% 
  mutate(id = NA_integer_)

if(typemesure != "Thermie") stop(paste0("Complément des données non développés pour le type de mesures ",typemesure))
  
## Formatage des données ##
# Réalisé dans BDD.format

## Filtrage des données ##
dataaimporter <- 
  dataaimporter %>% 
  filter(!is.na(chsvi_date))
  
## Modification de l'ordre des champs ##
dataaimporter <- 
  dataaimporter %>% 
  {if (!("Modification" %in% names(dataaimporter))) dplyr::select(., match(colnames(SuiviTerrain), names(.))) else .} %>%
  {if ("Modification" %in% names(dataaimporter)) dplyr::select(., match(colnames(SuiviTerrain), names(.)), contains("Modification")) else .} # Afin de conserver temporairement une hypothétique colonne Modification qui permet de repérer les lignes modifiées dans le cas de nettoyages massifs
}

#### Stations ####
if(Type == "Stations"){

## Chargement des données ##
dataaimporter <- read_excel(Localisation, sheet = feuille)

## Transformation ##
dataaimporter <- 
  dataaimporter %>% 
  rename_at(vars(contains("CodeRDT")), list(~str_replace(., "CodeRDT", "coderhj"))) %>%
  rename_at(vars(contains("X")), list(~str_replace(., "X", "coord_x"))) %>%
  rename_at(vars(contains("Y")), list(~str_replace(., "Y", "coord_y"))) %>%
  rename_at(vars(contains("LIIE")), list(~str_replace(., "LIIE", ""))) %>%
  rename_at(vars(contains("TypeCoord")), list(~str_replace(., "TypeCoord", "coord_type"))) %>%
  rename_at(vars(contains("CommuneINSEE")), list(~str_replace(., "CommuneINSEE", "commune"))) %>%
  rename_at(vars(contains("Ecosystème")), list(~str_replace(., "Ecosystème", "milieu"))) %>%
  rename_at(vars(contains("Code hydro")), list(~str_replace(., "Code hydro", "milieucodehydro"))) %>%
  rename_at(vars(contains("ReseauThermie")), list(~str_replace(., "ReseauThermie", "reseauthermietype"))) %>%
  rename_at(vars(contains("Thermie")), list(~str_replace(., "Thermie", "suivithermie"))) %>%
  rename_at(vars(contains("Piezo")), list(~str_replace(., "Piezo", "suivipiezo"))) %>%
  rename_at(vars(contains("Hydro")), list(~str_replace(., "Hydro", "suivihydro"))) %>%
  rename_at(vars(contains("Type")), list(~str_replace(., "Type", "suivihydro"))) %>%
  rename_at(vars(contains("O2")), list(~str_replace(., "O2", "suivio2"))) %>%
  rename_at(vars(contains("Pluvio")), list(~str_replace(., "Pluvio", "suivipluvio"))) %>%
  rename_at(vars(contains("CodCONT")), list(~str_replace(., "CodCONT", "codecontextepdpg"))) %>%
  rename_at(vars(contains("Sous_bassi")), list(~str_replace(., "Sous_bassi", "sousbassin"))) %>%
  rename_at(vars(contains("DistSource")), list(~str_replace(., "DistSource", "distancesource"))) %>%
  rename_at(vars(contains("Tmm30j")), list(~str_replace(., "Tmm30j", "temperaturemax"))) %>%
  rename_at(vars(contains("Sectionmou")), list(~str_replace(., "Sectionmou", "sectionmouillee"))) %>%
  rename_at(vars(contains("LLitMin")), list(~str_replace(., "LLitMin", "largeurlitmineur"))) %>%
  rename_at(vars(matches("LLitEti")), list(~str_replace(., "LLitEti", "largeurlitetiage"))) %>%
  rename_at(vars(starts_with("NTT")), list(~str_replace(., "NTT", "typetheorique"))) %>%
  rename_at(vars(matches("0NGFéchelle")), list(~str_replace(., "0NGFéchelle", "zcapteur"))) %>%
  rename_at(vars(contains("ProfSondeE")), list(~str_replace(., "ProfSondeE", "profsonde"))) %>%
  rename_at(vars(contains("SurfaceBV")), list(~str_replace(., "SurfaceBV", "surfacebassinversant"))) %>%
  rename_at(vars(contains("Remarque")), list(~str_replace(., "Remarque", "remarques"))) %>%
  rename_all(list(~str_to_lower(.))) %>% 
  rename_all(list(~str_replace(., "[[:punct:]]", "_"))) %>% 
  rename_all(list(~str_replace(., "chsta_", ""))) %>% # car parfois déjà présent devant certains noms de colonnes dans excel
  rename_all(list(~str_c("chsta_",.))) %>% 
  mutate(chsta_coord_x = ifelse(is.na(chsta_coord_x) & "chsta_coord_xl93" %in% names(.), chsta_coord_xl93, chsta_coord_x)) %>% 
  mutate(chsta_coord_y = ifelse(is.na(chsta_coord_y) & "chsta_coord_yl93" %in% names(.), chsta_coord_yl93, chsta_coord_y)) %>% 
  mutate(chsta_coord_x = ifelse(is.na(chsta_coord_x) & "chsta_coord_xliie" %in% names(.), chsta_coord_xliie, chsta_coord_x)) %>% 
  mutate(chsta_coord_y = ifelse(is.na(chsta_coord_y) & "chsta_coord_yliie" %in% names(.), chsta_coord_yliie, chsta_coord_y)) %>% 
  dplyr::select(-(matches("chsta_stationid"))) %>% 
  dplyr::select(-(matches("chsta_coord_xliie"))) %>% 
  dplyr::select(-(matches("chsta_coord_yliie"))) %>% 
  dplyr::select(-(matches("chsta_coord_xl93"))) %>% 
  dplyr::select(-(matches("chsta_coord_yl93"))) %>% 
  dplyr::select(-(matches("chsta_coord_xamont"))) %>% 
  dplyr::select(-(matches("chsta_coord_yaval"))) %>% 
  mutate(chsta_codesie = ifelse(!is.na(chsta_codesie) & nchar(chsta_codesie) == 7, glue("0{chsta_codesie}"), chsta_codesie)) %>% 
  rowwise() %>% 
  mutate(chsta_distancesource_confluencedrainprincipal = ifelse("chsta_distancesource_confluencedrainprincipal" %in% names(.), chsta_distancesource_confluencedrainprincipal, NA)) %>% 
  mutate(chsta_milieucodehydro = ifelse("chsta_milieucodehydro" %in% names(.), chsta_milieucodehydro, NA)) %>% 
  mutate(chsta_bassin = ifelse("chsta_bassin" %in% names(.), chsta_bassin, NA)) %>% 
  mutate(chsta_sousbassin = ifelse("chsta_sousbassin" %in% names(.), chsta_sousbassin, NA)) %>% 
  mutate(chsta_commune = ifelse("chsta_commune" %in% names(.), chsta_commune, NA)) %>% 
  mutate(chsta_departement = ifelse("chsta_departement" %in% names(.), chsta_departement, NA)) %>% 
  mutate(chsta_pays = ifelse("chsta_pays" %in% names(.), chsta_pays, NA)) %>% 
  mutate(chsta_transmission = ifelse("chsta_transmission" %in% names(.), chsta_transmission, NA)) %>% 
  mutate(chsta_transmission = ifelse(is.na(chsta_transmission), "Non", chsta_transmission)) %>% 
  mutate(chsta_fonctionnement = ifelse("chsta_fonctionnement" %in% names(.), chsta_fonctionnement, NA)) %>% 
  mutate(chsta_suivithermie = ifelse("chsta_suivithermie" %in% names(.), chsta_suivithermie, NA)) %>% 
  mutate(chsta_codemo = ifelse("chsta_codemo" %in% names(.), chsta_codemo, NA)) %>% 
  mutate(chsta_codesie = ifelse("chsta_codesie" %in% names(.), chsta_codesie, NA)) %>% 
  mutate(chsta_mo = ifelse("chsta_mo" %in% names(.), chsta_mo, NA)) %>% 
  mutate(chsta_coord_type = ifelse("chsta_coord_type" %in% names(.), chsta_coord_type, NA)) %>% 
  mutate(chsta_reseauthermietype = ifelse("chsta_reseauthermietype" %in% names(.), chsta_reseauthermietype, NA)) %>% 
  mutate(chsta_suivipiezo = ifelse("chsta_suivipiezo" %in% names(.), chsta_suivipiezo, NA)) %>% 
  mutate(chsta_suivio2 = ifelse("chsta_suivio2" %in% names(.), chsta_suivio2, NA)) %>% 
  mutate(chsta_suivipluvio = ifelse("chsta_suivipluvio" %in% names(.), chsta_suivipluvio, NA)) %>% 
  mutate(chsta_typetheorique = ifelse("chsta_typetheorique" %in% names(.), chsta_typetheorique, NA)) %>% 
  mutate(chsta_altitude = ifelse("chsta_altitude" %in% names(.), chsta_altitude, NA)) %>% 
  mutate(chsta_codecontextepdpg = ifelse("chsta_codecontextepdpg" %in% names(.), chsta_codecontextepdpg, NA)) %>% 
  mutate(chsta_sprep = ifelse("chsta_sprep" %in% names(.), chsta_sprep, NA)) %>% 
  mutate(chsta_distancesource = ifelse("chsta_distancesource" %in% names(.), chsta_distancesource, NA)) %>% 
  mutate(chsta_temperaturemax = ifelse("chsta_temperaturemax" %in% names(.), chsta_temperaturemax, NA)) %>% 
  mutate(chsta_sectionmouillee = ifelse("chsta_sectionmouillee" %in% names(.), chsta_sectionmouillee, NA)) %>% 
  mutate(chsta_durete = ifelse("chsta_durete" %in% names(.), chsta_durete, NA)) %>% 
  mutate(chsta_largeurlitmineur = ifelse("chsta_largeurlitmineur" %in% names(.), chsta_largeurlitmineur, NA)) %>% 
  mutate(chsta_largeurlitetiage = ifelse("chsta_largeurlitetiage" %in% names(.), chsta_largeurlitetiage, NA)) %>% 
  mutate(chsta_pente = ifelse("chsta_pente" %in% names(.), chsta_pente, NA)) %>% 
  mutate(chsta_carteign = ifelse("chsta_carteign" %in% names(.), chsta_carteign, NA)) %>% 
  mutate(chsta_rive = ifelse("chsta_rive" %in% names(.), chsta_rive, NA)) %>% 
  mutate(chsta_ancrage = ifelse("chsta_ancrage" %in% names(.), chsta_ancrage, NA)) %>% 
  mutate(chsta_acces = ifelse("chsta_acces" %in% names(.), chsta_acces, NA)) %>% 
  mutate(chsta_detailsloc = ifelse("chsta_detailsloc" %in% names(.), chsta_detailsloc, NA)) %>% 
  mutate(chsta_description = ifelse("chsta_description" %in% names(.), chsta_description, NA)) %>% 
  mutate(chsta_remarques = ifelse("chsta_remarques" %in% names(.), chsta_remarques, NA)) %>% 
  mutate(chsta_ordretournee = ifelse("chsta_ordretournee" %in% names(.), chsta_ordretournee, NA)) %>% 
  mutate(chsta_impacts = ifelse("chsta_impacts" %in% names(.), chsta_impacts, NA)) %>% 
  mutate(chsta_profsonde = ifelse("chsta_profsonde" %in% names(.), chsta_profsonde, NA)) %>% 
  mutate(chsta_substrats = ifelse("chsta_substrats" %in% names(.), chsta_substrats, NA)) %>% 
  mutate(chsta_distberge = ifelse("chsta_distberge" %in% names(.), chsta_distberge, NA)) %>% 
  mutate(chsta_ombrage = ifelse("chsta_ombrage" %in% names(.), chsta_ombrage, NA)) %>% 
  mutate(chsta_facies = ifelse("chsta_facies" %in% names(.), chsta_facies, NA)) %>% 
  mutate(chsta_numphoto = ifelse("chsta_numphoto" %in% names(.), chsta_numphoto, NA)) %>% 
  mutate(chsta_zbouchon = ifelse("chsta_zbouchon" %in% names(.), chsta_zbouchon, NA)) %>% 
  mutate(chsta_typez = ifelse("chsta_typez" %in% names(.), chsta_typez, NA)) %>% 
  mutate(chsta_hcapteurbouchon = ifelse("chsta_hcapteurbouchon" %in% names(.), chsta_hcapteurbouchon, NA)) %>% 
  mutate(chsta_url = ifelse("chsta_url" %in% names(.), chsta_url, NA)) %>% 
  mutate(chsta_module = ifelse("chsta_module" %in% names(.), round(as.numeric(chsta_module),3), NA)) %>% 
  mutate(chsta_qmna5 = ifelse("chsta_qmna5" %in% names(.), chsta_qmna5, NA)) %>% 
  mutate(chsta_q2 = ifelse("chsta_q2" %in% names(.), chsta_q2, NA)) %>% 
  mutate(chsta_q5 = ifelse("chsta_q5" %in% names(.), chsta_q5, NA)) %>% 
  mutate(chsta_q10 = ifelse("chsta_q10" %in% names(.), chsta_q10, NA)) %>% 
  mutate(chsta_q20 = ifelse("chsta_q20" %in% names(.), chsta_q20, NA)) %>% 
  mutate(chsta_q30 = ifelse("chsta_q30" %in% names(.), chsta_q30, NA)) %>% 
  mutate(chsta_q50 = ifelse("chsta_q50" %in% names(.), chsta_q50, NA)) %>% 
  mutate(chsta_q100 = ifelse("chsta_q100" %in% names(.), chsta_q100, NA)) %>% 
  mutate(chsta_q300 = ifelse("chsta_q300" %in% names(.), chsta_q300, NA)) %>% 
  mutate(chsta_surfacebassinversant = ifelse("chsta_surfacebassinversant" %in% names(.), chsta_surfacebassinversant, NA)) %>% 
  ungroup() %>% 
  # dplyr::select(-(matches("chsta_afaire"))) %>%
  # dplyr::select(-(matches("chsta_numcarte"))) %>%
  # dplyr::select(-(matches("chsta_projection"))) %>%
  # dplyr::select(-(matches("chsta_type"))) %>%
  # dplyr::select(-(matches("chsta_profondeur"))) %>%
  dplyr::select(chsta_coderhj,
         chsta_codemo,
         chsta_codesie,
         chsta_mo,
         chsta_milieu,
         chsta_milieucodehydro,
         chsta_bassin,
         chsta_sousbassin,
         chsta_commune,
         chsta_departement,
         chsta_pays,
         chsta_coord_x,
         chsta_coord_y,
         chsta_coord_type,
         chsta_fonctionnement,
         chsta_transmission,
         chsta_suivithermie,
         chsta_reseauthermietype,
         chsta_suivipiezo,
         chsta_suivihydro,
         chsta_suivio2,
         chsta_suivipluvio,
         chsta_typetheorique,
         chsta_altitude,
         chsta_codecontextepdpg,
         chsta_sprep,
         chsta_distancesource,
         chsta_distancesource_confluencedrainprincipal,
         chsta_temperaturemax,
         chsta_sectionmouillee,
         chsta_durete,
         chsta_largeurlitmineur,
         chsta_largeurlitetiage,
         chsta_pente,
         chsta_surfacebassinversant,
         chsta_carteign,
         chsta_rive,
         chsta_ancrage,
         chsta_acces,
         chsta_detailsloc,
         chsta_description,
         chsta_url,
         chsta_remarques,
         chsta_ordretournee,
         chsta_impacts,
         chsta_profsonde,
         chsta_substrats,
         chsta_distberge,
         chsta_ombrage,
         chsta_facies,
         chsta_numphoto,
         chsta_zcapteur,
         chsta_zbouchon,
         chsta_typez,
         chsta_hcapteurbouchon,
         chsta_module,
         chsta_qmna5,
         chsta_q2,
         chsta_q5,
         chsta_q10,
         chsta_q20,
         chsta_q30,
         chsta_q50,
         chsta_q100,
         chsta_q300
         ) %>% 
  mutate(id = NA) %>% 
  mutate('_modif_utilisateur' = NA) %>% 
  mutate('_modif_type' = NA) %>% 
  mutate('_modif_date' = NA) %>% 
  dplyr::select(id, everything(), '_modif_utilisateur', '_modif_type', '_modif_date')

## Test ##
if(dataaimporter %>% filter(is.na(chsta_coord_x)) %>% nrow() > 0) stop("Présence de stations sans coordonnées")
}


#### Commentaires ####
if(Type == "Commentaires"){
  ## Chargement des données ##
  Commentaires <-
    structure(
      list(
        id = 2:3,
        chres_coderhj = c("ORA2-7", "ANG0-7"),
        chres_typemesure = c("Thermie", "Thermie"),
        chres_anneebiol = c(2018L,
                            2014L),
        chres_commentaire = c(
          "Test commentaire",
          "La chronique de température fait état d'une amplitude totale annuelle moyenne, avec une température médiane assez faible. Les variations intra-journalières sont assez réduites, mais augmentent légèrement à partir du printemps et durant la période estivale. Les pics de chaleur restent toutefois à des valeurs peu importantes."
        ),
        `_modif_utilisateur` = c("jb", "jb"),
        `_modif_type` = c("I",
                          "I"),
        `_modif_date` = structure(
          c(1571991458.03569, 1574241840.5954),
          class = c("POSIXct", "POSIXt"),
          tzone = ""
        )
      ),
      row.names = 1:2,
      class = c("tbl_df",
                "tbl", "data.frame")
    )
  
  dataaimporter <- read_excel(Localisation, sheet = feuille)
  if(all(names(dataaimporter) %in% names(Commentaires)) == FALSE) stop("Le fichier source de commentaires contient des noms de colonne à corriger")
}


#### Capteurs ####
if(Type == "Capteurs"){
  ## Chargement des données ##
  Capteurs <-
    structure(list(chcap_proprietaire = character(0), chcap_typecapteur = character(0), 
                   chcap_modelecapteur = character(0), chcap_numerocapteur = character(0), 
                   chcap_etat = character(0), chcap_projet = character(0), chcap_originecapteur = character(0), 
                   chcap_datedebut = character(0), chcap_datefin = character(0), 
                   chcap_remarques = character(0)), row.names = integer(0), class = c("tbl_df", 
                                                                                      "tbl", "data.frame"))
  
  dataaimporter <- read_excel(Localisation, sheet = feuille)
  if(all(names(dataaimporter) %in% names(Capteurs)) == FALSE) stop("Le fichier source des capteurs contient des noms de colonne à corriger")
}


#### Sortie des résultats ####
return(dataaimporter)
  
} # Fin de la fonction
