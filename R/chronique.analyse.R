#' Analyse de chroniques
#'
#' Cette fonction permet d'analyser des chroniques de mesures (température, niveaux, etc.)
#' @name chronique.analyse
#' @param data Data.frame contenant a minima une colonne chmes_date, une colonne chmes_heure et une colonne chmes_valeur
#' @param typemesure Défini le type de données et modifie l'analyse en fonction
#' @param pasdetemps Indique le pas de temps entre chaque mesure : 1 pour une heure (1 par défaut)
#' @param seuils Seuils de valeurs (25,22,19,15,4 par défaut) dont il faut tester les dépassements. L'ordre de sortie correspond à l'ordre de cette liste.
#' @param seuilexcesdefaut Seuil en-deça duquel les dépassements sont testés par défaut, sinon ils sont testés par excès (valeur de 12 par défaut).
#' @param degresjours Calcul des degrés/jours (\code{FALSE} par défault)
#' @keywords chronique
#' @import RcppRoll
#' @import tidyverse
#' @export
#' @examples
#' chronique.analyse(data, "Thermie")

chronique.analyse <- function(
  data = data,
  typemesure = c("Thermie", "Thermie barométrique", "Thermie piézométrique", "Barométrie", "Piézométrie", "Piézométrie brute", "Piézométrie compensée", "Piézométrie calée", "Piézométrie NGF", "Oxygénation", "Hydrologie", "Pluviométrie"),
  pasdetemps = 1,
  seuils = c(25,22,19,15,4),
  seuilexcesdefaut = 12,
  degresjours = F
  )
{
  
  ##### -------------- A FAIRE -------------- #####
  # Ajout d'un interrupteur de traitement de thermie/piézo/O2 -> classes d'amplitudes différentes en fct de thermie/piézo/O2 notamment -> chercher valeurs de référence dans biblio)
  # Il faudra faire une fonction commune (entre chronique.figure, chronique.agregation et chronique.analyse) pour créer un contexte propre de chronique
  # -------------- A FAIRE -------------- #

  ##### Mise au format des données #####
  
  ## Transformation du format des dates
  data <-
    data %>% 
    mutate(chmes_date = ymd(chmes_date)) %>% 
    mutate(Time = ymd_hms(paste(chmes_date, chmes_heure, sep = "_"))) %>% 
    filter(is.na(chmes_valeur) == F) %>% 
    arrange(Time)

  ##### Contexte de la chronique #####
  # chmes_coderhj
  Contexte <- 
    data %>% 
    distinct(chmes_coderhj)
  if(dim(Contexte)[1] == 0) stop("Aucune donnée dans la chronique à analyser")
  if(dim(Contexte)[1] > 1) stop("Différentes stations dans la chronique à analyser")
  
  Contexte <-
    data %>%
    summarise(
      DateMin = min(chmes_date),
      DateMax = max(chmes_date),
    ) %>%
    mutate(AnneeFinaleDebut = ifelse(month(DateMin) == 10 | month(DateMin) == 11 | month(DateMin) == 12, year(DateMin)+1, year(DateMin))) %>% 
    mutate(AnneeFinaleFin = ifelse(month(DateMax) >= 1 & month(DateMax) <= 9, year(DateMax), year(DateMax)+1)) %>% 
    mutate(Annee = ifelse(AnneeFinaleDebut == AnneeFinaleFin, AnneeFinaleFin, NA_integer_)) %>% 
    select(Annee) %>% 
    bind_cols(Contexte)
  
  if(is.na(Contexte$Annee)){stop("Calcul de l'année incomplet")}
  
  # chmes_typemesure
  if(testit::has_error(data %>% 
                         distinct(chmes_typemesure) %>% 
                         bind_cols(Contexte)) == TRUE) stop("Plusieurs chmes_typemesure au sein du jeu de données")
  Contexte <- 
    data %>% 
    distinct(chmes_typemesure) %>% 
    bind_cols(Contexte)
  
  typemesure <- Contexte$chmes_typemesure
  
  #### Évaluation des choix ####
  typemesure <- match.arg(typemesure) # à replacer en tête de fonction si ne fonctionne pas bien ici

  ##### Agrégation complète #####
  DataTravail <- chronique.agregation(data)
  #DataTravail[[1]];DataTravail[[2]];DataTravail[[3]];DataTravail[[4]];DataTravail[[5]]
  
  ##### Valeurs remarquables instantanées #####
  ValRemarqInstant <- 
    DataTravail[[5]] %>% 
    group_by(chmes_coderhj, chmes_typemesure)
  
  ##### Valeurs remarquables journalières #####
  ### Statistiques par jour ###
  ValJours <- 
    DataTravail[[2]] %>% 
    ungroup() %>% 
    mutate(Nref = 24/pasdetemps) %>% 
    mutate(VMaxMoy7j = RcppRoll::roll_mean(VMaxJ, 7, align = "right", fill = NA)) %>% 
    mutate(VMaxMoy30j = RcppRoll::roll_mean(VMaxJ, 30, align = "right", fill = NA)) %>% 
    mutate(VMoyMoy30j = RcppRoll::roll_mean(VMoyJ, 30, align = "right", fill = NA))
  
  ### Coefficient de variation des moyennes journalières
  CVJ <- sd(ValJours$VMoyJ)/mean(ValJours$VMoyJ)
  if(is.na(CVJ) == T) stop("Présence de valeurs vides dans la chronique")
  
  ### Valeurs remarquables ###
  ValRemarqJours <-
    ValJours %>% 
    group_by(chmes_coderhj, chmes_typemesure) %>% 
    summarise(
      VMoyJMinPer = round(min(VMoyJ),1),
      DateVMoyJMinPer = chmes_date[VMoyJ == min(VMoyJ)][1], # le [1] permet d'afficher la première occurence dans le cas d'occurences multiples
      VMoyJMedPer = round(median(VMoyJ),1),
      VMoyJMoyPer = round(mean(VMoyJ),1),
      VMoyJMaxPer = round(max(VMoyJ),1),
      DateVMoyJMaxPer = chmes_date[VMoyJ == max(VMoyJ)][1], # le [1] permet d'afficher la première occurence dans le cas d'occurences multiples
      AmplitudeVMoyJPer = VMoyJMaxPer-VMoyJMinPer,
      VarVMoyJ = round(var(VMoyJ),2)
    )
  
  ##### Valeurs remarquables sur année biologique #####
  ValRemarqAB <- 
    DataTravail[[4]] %>% 
    select(chmes_coderhj, chmes_typemesure, VMinAB, DateVMinAB, VMaxAB, DateVMaxAB, Percentile10AB, Percentile25AB, Percentile50AB, Percentile75AB, Percentile90AB, Percentile90diurneAB)
  
  ##### Valeurs remarquables sur périodes mobiles #### 
  ## Calcul V Maxmoy 30 jours ## (Sens FD71)
  ValRemarqPeriodesMobiles <-
    ValJours %>% 
    group_by(chmes_coderhj, chmes_typemesure) %>% 
    filter(!is.na(VMoyMoy30j)) %>% 
    summarise(
      VMoyMoy30J = max(VMoyMoy30j)
    ) 
  
  ## Calcul V Maxmoy 30 jours ## (Sens Verneaux)
  ValRemarqPeriodesMobiles <-
    ValJours %>% 
    group_by(chmes_coderhj, chmes_typemesure) %>% 
    filter(!is.na(VMaxMoy30j)) %>% 
    summarise(
      VMaxMoy30J = max(VMaxMoy30j),
      DateDebutVMaxMoy30J = chmes_date[VMaxMoy30j == max(VMaxMoy30j)][1] -29, # le [1] permet d'afficher la première occurence dans le cas d'occurences multiples
      DateFinVMaxMoy30J = chmes_date[VMaxMoy30j == max(VMaxMoy30j)][1] # le [1] permet d'afficher la première occurence dans le cas d'occurences multiples
    ) %>% 
    left_join(ValRemarqPeriodesMobiles, by = c("chmes_coderhj", "chmes_typemesure"))
  
  ## Calcul V Maxmoy 7 jours ##
  ValRemarqPeriodesMobiles <-
    ValJours %>% 
    group_by(chmes_coderhj, chmes_typemesure) %>% 
    filter(!is.na(VMaxMoy7j)) %>% 
    summarise(
      VMaxMoy7J = max(VMaxMoy7j),
      DateDebutVMaxMoy7J = chmes_date[VMaxMoy7j == max(VMaxMoy7j)][1] -6,# le [1] permet d'afficher la première occurence dans le cas d'occurences multiples
      DateFinVMaxMoy7J = chmes_date[VMaxMoy7j == max(VMaxMoy7j)][1] # le [1] permet d'afficher la première occurence dans le cas d'occurences multiples
    ) %>% 
    left_join(ValRemarqPeriodesMobiles, by = c("chmes_coderhj", "chmes_typemesure"))
  
  ## Calcul V Moymax 24 heures ##
  ValRemarqPeriodesMobiles <-
    DataTravail[[1]] %>% 
    mutate(VMM24H = RcppRoll::roll_mean(chmes_valeur, 24, align = "right", fill = NA)) %>% 
    filter(!is.na(VMM24H)) %>%
    summarise(
      VMaxMoy24H = max(VMM24H),
      DateMoyVMaxMoy24H = paste(chmes_date, chmes_heure, sep = " ")[VMM24H == max(VMM24H)][1] # le [1] permet d'afficher la première occurence dans le cas d'occurences multiples
    ) %>%
    mutate(DateMoyVMaxMoy24H = ymd_hms(DateMoyVMaxMoy24H)) %>%  # pour arrondir la date au jour il y a le plus d'heures
    mutate(DateMoyVMaxMoy24H = ifelse(hour(DateMoyVMaxMoy24H) >= 12, format(DateMoyVMaxMoy24H, format="%Y-%m-%d"), format(DateMoyVMaxMoy24H - days(1), format="%Y-%m-%d"))) %>% 
    bind_cols(ValRemarqPeriodesMobiles)
  
  ## Arrondis ##
  ValRemarqPeriodesMobiles <-
    ValRemarqPeriodesMobiles %>% 
    mutate(VMaxMoy7J = round(VMaxMoy7J,1)) %>% 
    mutate(VMaxMoy30J = round(VMaxMoy30J,1)) %>% 
    mutate(VMaxMoy24H = round(VMaxMoy24H,1))
  
  if(ValRemarqPeriodesMobiles$VMaxMoy7J < ValRemarqPeriodesMobiles$VMaxMoy30J) warning(glue("Pb dans calcul Vmm pour {Contexte$chmes_coderhj}"))

  ## Extraction de l'année du VMM ##
  AnneeVMM <- as.data.frame(year(ValRemarqPeriodesMobiles$DateFinVMaxMoy30J))
  colnames(AnneeVMM) <- c("AnneeVMM")
  
  ##### Qualité des données #####
  Qualite <-
    data %>% 
    group_by(chmes_coderhj, chmes_typemesure) %>% 
    summarise(
      DateDPeriode = data$chmes_date[1], ## Date début période ##
      DateFPeriode = data$chmes_date[dim(data)[1]], ## Date fin période ##
      NbJ = length(unique(data$chmes_date)) ## Nb Total de Jours ##
    ) %>%
    mutate(IntervalleMax = max(data$Time) - min(data$Time)) %>% # Calcul de l'intervalle entre les dates minimale et maximale
    mutate(dureeTotale = ifelse(abs(IntervalleMax - NbJ) <= 2, "Nb tot jours cohérent", "Nb tot jours incohérent")) %>% 
    bind_cols(
      ValJours %>% 
        summarise(
          NbJpasOk = length(unique(chmes_date[NMesuresJ != Nref])),
          NbJOK = length(unique(chmes_date[NMesuresJ == Nref]))
        )
    )
  
  ##### Degrés-jours #####
  ## Degrés-jours descendant (TRF) ##
  RefValeurInf <- 12
  RefDegJours <- 800

  degresjoursTRF <-
    ValJours %>%
    mutate(TestValeurInf = ifelse(VMoyJ < RefValeurInf, 1, NA)) %>% # identification de la journée passant sous une valeur repère
    filter(!is.na(TestValeurInf)) %>%
    mutate(DegJours = cumsum(VMoyJ)) %>%
    summarise(
      DateDebutdegresjoursTRF = chmes_date[first(TestValeurInf)],
      DateFindegresjoursTRF = first(chmes_date[DegJours > RefDegJours])
    ) %>%
    mutate(DateDebutdegresjoursTRF = ymd(DateDebutdegresjoursTRF)) %>%
    mutate(DateFindegresjoursTRF = ymd(DateFindegresjoursTRF)) %>%
    mutate(NbJdegresjoursTRF = as.numeric(DateFindegresjoursTRF - DateDebutdegresjoursTRF))

  ## Degrés-jours montants (OBR) ##
  RefValeurSup <- 9
  RefDegJours <- 270

  degresjoursAutreEsp <-
    ValJours %>%
    mutate(TestValeurSup = ifelse(VMoyJ > RefValeurSup, 1, NA)) %>% # identification de la journée passant sous une valeur repère
    filter(chmes_date > paste0(year(last(chmes_date)),"-02-01")) %>% # 29 mars
    filter(!is.na(TestValeurSup)) %>%
    #View()
    mutate(DegJours = cumsum(VMoyJ)) %>%
    summarise(
      DateDebutdegresjoursAutreEsp = chmes_date[first(TestValeurSup)],
      DateFindegresjoursAutreEsp = first(chmes_date[DegJours > RefDegJours])
    ) %>%
    mutate(DateDebutdegresjoursAutreEsp = ymd(DateDebutdegresjoursAutreEsp)) %>%
    mutate(DateFindegresjoursAutreEsp = ymd(DateFindegresjoursAutreEsp)) %>%
    mutate(NbJdegresjoursAutreEsp = as.numeric(DateFindegresjoursAutreEsp - DateDebutdegresjoursAutreEsp))
  
  ##### Dépassement de valeurs seuils ####1
# Fonctionnement général : on travail partout avec Vmax, avec un renommage avant et après des champs avec leur valeur réelle + calcul < ou > pour les faibles valeurs ou fortes valeurs
seuils <- unique(seuils) # En cas de saisie de doublons en entrée

if(is.numeric(seuilexcesdefaut) == FALSE) stop("seuilexcesdefaut doit être une valeur numérique")

## Calcul des épisodes ##
for(i in 1:length(seuils)){
Seuil <- seuils[i]

  if(i == 1){
  AnalyseSeuils <-
    DataTravail[[1]] %>% 
    rowwise() %>% 
    mutate(depassementVmax = ifelse(Seuil >= seuilexcesdefaut, chmes_valeur > Seuil, NA)) %>%
    mutate(depassementVmax = ifelse(Seuil < seuilexcesdefaut, chmes_valeur < Seuil, depassementVmax)) %>%
    mutate(differenceVmax = ifelse(Seuil >= seuilexcesdefaut, chmes_valeur - Seuil, NA)) %>%
    mutate(differenceVmax = ifelse(Seuil < seuilexcesdefaut, Seuil - chmes_valeur, differenceVmax)) %>%
    ungroup() %>%
    group_by(depassementVmax) %>%
    mutate(EpisodeVmax = ifelse(depassementVmax == TRUE, 1:n(), NA)) %>% 
    ungroup() %>%
    mutate(EpisodeVmax2 = ifelse(!is.na(EpisodeVmax) & is.na(lag(EpisodeVmax)), EpisodeVmax, NA)) %>% 
    fill(EpisodeVmax2) %>% 
    mutate(EpisodeVmax2 = ifelse(is.na(EpisodeVmax), NA, EpisodeVmax2)) %>% 
    mutate(EpisodeVmax = EpisodeVmax2) %>% 
    select(-depassementVmax, -differenceVmax, -EpisodeVmax2)
  
if(Seuil >= seuilexcesdefaut) AnalyseSeuils <- AnalyseSeuils %>% rename(!!paste0('EpisodesSup',quo_name(Seuil)) := EpisodeVmax)
if(Seuil < seuilexcesdefaut) AnalyseSeuils <- AnalyseSeuils %>% rename(!!paste0('EpisodesInf',quo_name(Seuil)) := EpisodeVmax)

  }
  if(i != 1){
  AnalyseSeuilsTemp <-
    DataTravail[[1]] %>% 
    rowwise() %>% 
    mutate(depassementVmax = ifelse(Seuil >= seuilexcesdefaut, chmes_valeur > Seuil, NA)) %>%
    mutate(depassementVmax = ifelse(Seuil < seuilexcesdefaut, chmes_valeur < Seuil, depassementVmax)) %>%
    mutate(differenceVmax = ifelse(Seuil >= seuilexcesdefaut, chmes_valeur - Seuil, NA)) %>%
    mutate(differenceVmax = ifelse(Seuil < seuilexcesdefaut, Seuil - chmes_valeur, differenceVmax)) %>%
    ungroup() %>%
    group_by(depassementVmax) %>%
    mutate(EpisodeVmax = ifelse(depassementVmax == TRUE, 1:n(), NA)) %>% 
    ungroup() %>%
    mutate(EpisodeVmax2 = ifelse(!is.na(EpisodeVmax) & is.na(lag(EpisodeVmax)), EpisodeVmax, NA)) %>% 
    fill(EpisodeVmax2) %>% 
    mutate(EpisodeVmax2 = ifelse(is.na(EpisodeVmax), NA, EpisodeVmax2)) %>% 
    mutate(EpisodeVmax = EpisodeVmax2) %>% 
    select(-depassementVmax, -differenceVmax, -EpisodeVmax2) 
  
  if(Seuil >= seuilexcesdefaut) AnalyseSeuils <- AnalyseSeuilsTemp %>% rename(!!paste0('EpisodesSup',quo_name(Seuil)) := EpisodeVmax) %>% left_join(AnalyseSeuils, by = c("chmes_date", "chmes_heure", "chmes_valeur", "chmes_coderhj", "chmes_typemesure", "chmes_anneebiol"))
  if(Seuil < seuilexcesdefaut) AnalyseSeuils <- AnalyseSeuilsTemp %>% rename(!!paste0('EpisodesInf',quo_name(Seuil)) := EpisodeVmax) %>% left_join(AnalyseSeuils, by = c("chmes_date", "chmes_heure", "chmes_valeur", "chmes_coderhj", "chmes_typemesure", "chmes_anneebiol"))
  
  }
}

## Analyse des épisodes ##
ListeEpisodes <- AnalyseSeuils %>% select(contains("Episodes")) %>% names() %>% rev()

# Nb total d'heures et nb de jours où on atteint au moins une fois la valeur
for(i in 1:length(seuils)){
  #episode <- ListeEpisodes[i]
  episode <- ListeEpisodes[length(seuils) + 1 - i]
  
  if(i == 1){
Episodes <-
  AnalyseSeuils %>% 
  rename(EpisodeVmax := !!(quo(episode))) %>% 
  summarise(
    dureemaxepisodesvmax = sum(!is.na(EpisodeVmax), na.rm = T),
    nbjepisodesvmax = length(unique(chmes_date[!is.na(EpisodeVmax)]))) %>% 
  rename(!!paste0('DureeMax',quo_name(episode)) := dureemaxepisodesvmax) %>% 
  rename(!!paste0('NbJ',quo_name(episode)) := nbjepisodesvmax)
  }
  
  if(i != 1){
    Episodes <-
      AnalyseSeuils %>% 
      rename(EpisodeVmax := !!(quo(episode))) %>% 
      summarise(
        dureemaxepisodesvmax = sum(!is.na(EpisodeVmax), na.rm = T),
        nbjepisodesvmax = length(unique(chmes_date[!is.na(EpisodeVmax)]))) %>% 
      rename(!!paste0('DureeMax',quo_name(episode)) := dureemaxepisodesvmax) %>% 
      rename(!!paste0('NbJ',quo_name(episode)) := nbjepisodesvmax) %>% 
      bind_cols(Episodes)
  }
}

# Nb d'épisodes + durée et dates de l'épisode le plus long
AnalyseSeuils <-
  AnalyseSeuils %>% 
  mutate(chmes_datefine = ymd_hms(paste0(chmes_date, chmes_heure))) %>% 
  arrange(chmes_datefine)

for(i in 1:length(seuils)){
  episode <- ListeEpisodes[i]
  
  # Renommage temporaire
  AnalyseSeuilsTemp <- AnalyseSeuils %>% rename(EpisodeVmax := !!(quo(episode)))
  # Nb d'épisodes
  Episodes <- 
    Episodes %>% 
    bind_cols(nbepisodesvmax = n_distinct(AnalyseSeuilsTemp$EpisodeVmax, na.rm = T))
  
  # Durée et dates de l'épisode le plus long
  if(Episodes$nbepisodesvmax != 0){
    Episodes <- 
      Episodes %>% 
      bind_cols(AnalyseSeuilsTemp %>% filter(!is.na(EpisodeVmax)) %>% group_by(EpisodeVmax) %>% summarise(N = n()) %>% right_join(AnalyseSeuilsTemp, by = "EpisodeVmax") %>% filter(N == max(N, na.rm = T)) %>% summarise(datedebutepisodevmax = first(chmes_datefine), datefinepisodevmax = last(chmes_datefine)) %>% mutate(dureeepisodevmax = (datefinepisodevmax - datedebutepisodevmax)/(3600*24)))
    }
  if(Episodes$nbepisodesvmax == 0){
    Episodes <- 
      Episodes %>% 
      mutate(datedebutepisodevmax = NA) %>% 
      mutate(datefinepisodevmax = NA) %>% 
      mutate(dureeepisodevmax = NA)
    }
  
  # Renommage final
  Episodes <- 
    Episodes %>% 
    rename(!!paste0('Nb',quo_name(episode)) := nbepisodesvmax) %>% 
    rename(!!paste0('DateD',quo_name(episode)) := datedebutepisodevmax) %>% 
    rename(!!paste0('DateF',quo_name(episode)) := datefinepisodevmax) %>% 
    rename(!!paste0('Duree',quo_name(episode)) := dureeepisodevmax)
}

# Remise en ordre des champs des épisodes en fonction de l'ordre de saisie des seuils #
Episodesdesordonnes <- Episodes

for(i in 1:length(seuils)){
  if(i == 1){
    Episodes <- 
      Episodesdesordonnes %>% 
      select(matches(as.character(seuils[length(seuils) + 1 - i]))) # pour avoir les valeurs par ordre de l'appel
  }
  if(i != 1){
    Episodes <- 
      Episodesdesordonnes %>% 
      select(matches(as.character(seuils[length(seuils) + 1 - i]))) %>% # pour avoir les valeurs par ordre de l'appel
      bind_cols(Episodes)
  }
}

  #### Sélection des résultats ####
Complet <- 
  Qualite %>% 
  left_join(Contexte, by = c("chmes_coderhj", "chmes_typemesure")) %>% 
  bind_cols(AnneeVMM) %>% 
  left_join(ValRemarqInstant, by = c("chmes_coderhj", "chmes_typemesure")) %>% 
  left_join(ValRemarqJours, by = c("chmes_coderhj", "chmes_typemesure")) %>% 
  left_join(ValRemarqPeriodesMobiles, by = c("chmes_coderhj", "chmes_typemesure")) %>% 
  left_join(ValRemarqAB, by = c("chmes_coderhj", "chmes_typemesure")) %>% 
  bind_cols(Episodes)

if(typemesure != "Thermie"){
  Complet <- Complet
  }

if(typemesure == "Thermie" & degresjours == FALSE){
  Complet <- Complet
  }

if(typemesure == "Thermie" & degresjours == TRUE){
Complet <- 
  Complet %>% 
  left_join(degresjoursTRF, by = c("chmes_coderhj", "chmes_typemesure")) %>% 
  left_join(degresjoursAutreEsp, by = c("chmes_coderhj", "chmes_typemesure"))
}

  #### Nettoyage des résultats ####
Complet <-
  Complet %>%
  rename_at(vars(matches("chmes_coderhj")), list( ~ str_replace(., "chmes_coderhj", "Coderhj"))) %>%
  rename_at(vars(matches("chmes_typemesure")), list( ~ str_replace(., "chmes_typemesure", "Typemesure"))) %>% 
  select(Typemesure, Coderhj, Annee, AnneeVMM, everything())

  #### Sortie des résultats ####
  return(Complet)
  
} # Fin de la fonction
