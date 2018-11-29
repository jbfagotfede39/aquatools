#' Analyse de chroniques
#'
#' Cette fonction permet d'analyser des chroniques de mesures (température, niveaux, etc.)
#' @name chronique.analyse
#' @param data Data.frame contenant a minima une colonne chmes_date, une colonne chmes_heure et une colonne chmes_valeur
#' @param typemesure Défini le type de données et modifie l'analyse en fonction
#' @param pasdetemps Indique le pas de temps entre chaque mesure : 1 pour une heure (1 par défaut)
#' @param Vmin Valeur inférieure de seuil d'analyse
#' @param Vmax Valeur supérieure de seuil d'analyse
#' @param VminExt Valeur inférieure extrême de seuil d'analyse
#' @param VmaxExt Valeur supérieure extrême de seuil d'analyse
#' @keywords chronique
#' @import tidyverse
#' @import lubridate
#' @import RcppRoll
#' @export
#' @examples
#' chronique.analyse("ANG0-4", 2016, data, pasdetemps = 1)
#' chronique.analyse(as.character(Stations[i,2]), Annee, data, pasdetemps = 1)

chronique.analyse <- function(
  data = data,
  typemesure = c("Thermie", "Thermie barométrique", "Thermie piézométrique", "Barométrie", "Piézométrie", "Piézométrie brute", "Piézométrie compensée", "Oxygénation", "Hydrologie", "Pluviométrie"),
  pasdetemps = 1,
  Vmin = 4,
  Vmax = 18,
  VminExt = 0,
  VmaxExt = 24
  )
{
  
  ##### -------------- A FAIRE -------------- #####
  # Ajout d'un interrupteur de traitement de thermie/piézo/O2 -> classes d'amplitudes différentes en fct de thermie/piézo/O2 notamment -> chercher valeurs de référence dans biblio)
  # Calcul du nb et de la durée de dépassement de 4 valeurs Vmin, Vmax, VminExt, VmaxExt
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
  
  # Annee
  Contexte <- 
    data %>% 
    summarise(
      Annee = median(year(chmes_date))
    ) %>% 
    bind_cols(Contexte)
  
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
  
  ##### Valeurs instantanées remarquables #####
  ValRemarqInstant <- DataTravail[[5]]
  
  ##### Valeurs journalières remarquables #####
  ### Statistiques par jour ###
  ValJours <- 
    DataTravail[[2]] %>% 
    mutate(Nref = 24/pasdetemps) %>% 
    mutate(VMM7j = roll_mean(VMoyJ, 7, align = "right", fill = NA)) %>% 
    mutate(VMM30j = roll_mean(VMoyJ, 30, align = "right", fill = NA))
  
  ### Coefficient de variation des moyennes journalières
  CVJ <- sd(ValJours$VMoyJ)/mean(ValJours$VMoyJ)
  if(is.na(CVJ) == T) stop("Présence de valeurs vides dans la chronique")
  
  ### Valeurs remarquables ###
  ValRemarqJours <-
    ValJours %>% 
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
  
  ##### Valeurs sur périodes mobiles #### 
  ## Calcul V Moymax 7 et 30 J ##
  ValRemarqPeriodesMobiles <-
    ValJours %>% 
    filter(!is.na(VMM30j)) %>% 
    summarise(
      VMaxMoy30J = max(VMM30j),
      DateDebutVMaxMoy30J = chmes_date[VMM30j == max(VMM30j)][1] -29, # le [1] permet d'afficher la première occurence dans le cas d'occurences multiples
      DateFinVMaxMoy30J = chmes_date[VMM30j == max(VMM30j)][1] # le [1] permet d'afficher la première occurence dans le cas d'occurences multiples
    ) 
    
  ValRemarqPeriodesMobiles <-
    ValJours %>% 
    filter(!is.na(VMM7j)) %>% 
    summarise(
      VMaxMoy7J = max(VMM7j),
      DateDebutVMaxMoy7J = chmes_date[VMM7j == max(VMM7j)][1] -6,# le [1] permet d'afficher la première occurence dans le cas d'occurences multiples
      DateFinVMaxMoy7J = chmes_date[VMM7j == max(VMM7j)][1] # le [1] permet d'afficher la première occurence dans le cas d'occurences multiples
    ) %>% 
    bind_cols(ValRemarqPeriodesMobiles) %>% 
    mutate(VMaxMoy7J = round(VMaxMoy7J,1)) %>% 
    mutate(VMaxMoy30J = round(VMaxMoy30J,1))
  
  if(ValRemarqPeriodesMobiles$VMaxMoy7J < ValRemarqPeriodesMobiles$VMaxMoy30J) stop("Pb dans calcul Vmm")

  ## Extraction de l'année du VMM ##
  AnneeVMM <- year(ValRemarqPeriodesMobiles$DateFinVMaxMoy30J)
  
  ##### Degrés-jours #####
  ## Degrés-jours descendant (TRF) ##
  RefValeurInf <- 12
  RefDegJours <- 800

  DegresJoursTRF <-
    ValJours %>%
    mutate(TestValeurInf = ifelse(VMoyJ < RefValeurInf, 1, NA)) %>% # identification de la journée passant sous une valeur repère
    filter(!is.na(TestValeurInf)) %>%
    mutate(DegJours = cumsum(VMoyJ)) %>%
    summarise(
      DateDebutDegresJours = chmes_date[first(TestValeurInf)],
      DateFinDegresJours = first(chmes_date[DegJours > RefDegJours])
    ) %>%
    mutate(DateDebutDegresJours = ymd(DateDebutDegresJours)) %>%
    mutate(DateFinDegresJours = ymd(DateFinDegresJours)) %>%
    mutate(NbJDegresJours = as.numeric(DateFinDegresJours - DateDebutDegresJours))

  ## Degrés-jours montants (OBR) ##
  RefValeurSup <- 9
  RefDegJours <- 270

  DegresJoursAutreEsp <-
    ValJours %>%
    mutate(TestValeurSup = ifelse(VMoyJ > RefValeurSup, 1, NA)) %>% # identification de la journée passant sous une valeur repère
    filter(chmes_date > paste0(year(last(chmes_date)),"-02-01")) %>% # 29 mars
    filter(!is.na(TestValeurSup)) %>%
    #View()
    mutate(DegJours = cumsum(VMoyJ)) %>%
    summarise(
      DateDebutDegresJours = chmes_date[first(TestValeurSup)],
      DateFinDegresJours = first(chmes_date[DegJours > RefDegJours])
    ) %>%
    mutate(DateDebutDegresJours = ymd(DateDebutDegresJours)) %>%
    mutate(DateFinDegresJours = ymd(DateFinDegresJours)) %>%
    mutate(NbJDegresJours = as.numeric(DateFinDegresJours - DateDebutDegresJours))
  
  ##### Dépassement de valeurs seuils #### 
  # On peut calculer si chaque valeur est dans quel intervalle (normal, max-MaxExtrem, > MaxExtrem, etc.)
  # Puis on décompte la durée et le nombre d'épisode : mais comment les regrouper par ensemble
  
  
  #### À faire ####
  # Ajout de 4 colonnes avec calcul si on est dans chaque intervalle de valeurs ou non
  # filtrage en ne conservant que les lignes contenant un positif dans au moins une des colonnes
  # Plutôt filtrage différent pour chaque dénombrement de chaque modalité/intervalle
  # Tri par ordre chronologie
  # Puis établissement si même intervalle ou intervalle différent entre deux valeurs successives si valeur = à intervalle de temps fixé au départ ou si supérieur en faisant n = 0 au départ puis max(n) +1 à chaque fois
  # Puis dénombrement des modalités uniques de n pour avoir le nombre d'épisodes
  # et dénombrement du nb de lignes x intervalle fixé au départ pour avoir la durée totale de dépassement
  
  # utilisation d'une dérivée de la fonction roll ? roll_mean(VMoyJ, 7, align = "right", fill = NA)
  
  ##### Qualité des données #####
  ## Date début période ##
  DateDPeriode <- data$chmes_date[1]
  #DateDPeriode <- format(data$Date[1], format="%d-%m-%Y")
  
  ## Date fin période ##
  DateFPeriode <- data$chmes_date[dim(data)[1]]
  #DateFPeriode <- format(data$chmes_date[dim(data)[1]], format="%d-%m-%Y")

  ## Nb Total de Jours ##
  NbJ <- length(unique(data$chmes_date))
  
  ## Vérification de la cohérence entre le nombre de jours et la durée de la chronique ##
  intervalMax <- max(data$Time) - min(data$Time)
  if(abs(intervalMax - NbJ) <= 2) dureeTotale <- "Nb tot jours cohérent"
  if(abs(intervalMax - NbJ) > 2) dureeTotale <- "Nb tot jours incohérent"
  
  ## Vérification de la cohérence avec le nb de données attendues par jour ##
  
  # Totaux des jours OK et pas OK #
  NbJpasOK <- dim(ValJours %>% filter(NMesuresJ != Nref))[1]
  NbJOK <- dim(ValJours %>% filter(NMesuresJ == Nref))[1]
  
  #### Sortie des résultats ####
  if(typemesure == "Thermie") Complet <- data.frame(NbJ, NbJOK, NbJpasOK, DateDPeriode, DateFPeriode, intervalMax, dureeTotale, Contexte$chmes_coderhj, Contexte$Annee, Contexte$chmes_typemesure, AnneeVMM, ValRemarqInstant$VMinI, ValRemarqInstant$VMaxI, ValRemarqInstant$VMoyI, ValRemarqInstant$VMedI, ValRemarqInstant$VarI, ValRemarqInstant$VAmpliI, ValRemarqJours$VMoyJMinPer, ValRemarqJours$DateVMoyJMinPer, ValRemarqJours$VMoyJMaxPer, ValRemarqJours$DateVMoyJMaxPer, ValRemarqJours$VMoyJMoyPer, ValRemarqJours$VMoyJMedPer, CVJ, ValRemarqJours$AmplitudeVMoyJPer, ValRemarqPeriodesMobiles$VMaxMoy7J, ValRemarqPeriodesMobiles$DateDebutVMaxMoy7J, ValRemarqPeriodesMobiles$DateFinVMaxMoy7J, ValRemarqPeriodesMobiles$VMaxMoy30J, ValRemarqPeriodesMobiles$DateDebutVMaxMoy30J, ValRemarqPeriodesMobiles$DateFinVMaxMoy30J, DegresJoursTRF$DateDebutDegresJours, DegresJoursTRF$DateFinDegresJours, DegresJoursTRF$NbJDegresJours, DegresJoursAutreEsp$DateDebutDegresJours, DegresJoursAutreEsp$DateFinDegresJours, DegresJoursAutreEsp$NbJDegresJours) 
  if(typemesure != "Thermie") Complet <- data.frame(NbJ, NbJOK, NbJpasOK, DateDPeriode, DateFPeriode, intervalMax, dureeTotale, Contexte$chmes_coderhj, Contexte$Annee, Contexte$chmes_typemesure, AnneeVMM, ValRemarqInstant$VMinI, ValRemarqInstant$VMaxI, ValRemarqInstant$VMoyI, ValRemarqInstant$VMedI, ValRemarqInstant$VarI, ValRemarqInstant$VAmpliI, ValRemarqJours$VMoyJMinPer, ValRemarqJours$DateVMoyJMinPer, ValRemarqJours$VMoyJMaxPer, ValRemarqJours$DateVMoyJMaxPer, ValRemarqJours$VMoyJMoyPer, ValRemarqJours$VMoyJMedPer, CVJ, ValRemarqJours$AmplitudeVMoyJPer, ValRemarqPeriodesMobiles$VMaxMoy7J, ValRemarqPeriodesMobiles$DateDebutVMaxMoy7J, ValRemarqPeriodesMobiles$DateFinVMaxMoy7J, ValRemarqPeriodesMobiles$VMaxMoy30J, ValRemarqPeriodesMobiles$DateDebutVMaxMoy30J, ValRemarqPeriodesMobiles$DateFinVMaxMoy30J) 

} # Fin de la fonction