#' Analyse de chroniques
#'
#' Cette fonction permet d'analyser des chroniques de mesures (température, niveaux, etc.)
#' 
#' @param CodeRDT Nom de la station
#' @param Annee Année du suivi
#' @param data Data.frame contenant a minima une colonne Date, une colonne Heure et une colonne Valeur
#' @param Titre Titre du graphique (vide par défaut)
#' @param legendeY Défini le texte de la légende de l'axe Y (Température (°C) par défaut)
#' @param duree Si \code{Complet} (par défault), affichage de l'année complète.  Si \code{Relatif}, affichage uniquement de la période concernée.
#' @param Vmm30j Si \code{FALSE} (par défault), n'affiche pas les
#'    Vmm30j.  Si \code{TRUE}, les affiche.
#' @param Vminmax Si \code{TRUE} (par défault), affiche pas les
#'    valeurs journalières minimales et maximales.  Si \code{FALSE}, ne les affiche pas.
#' @param Ymin Valeur minimale de l'axe des Y (0 par défaut)
#' @param Ymax Valeur maximale de l'axe des Y (aucune par défaut)
#' @param pasdetemps Indique le pas de temps entre chaque mesure : 1 pour une heure (1 par défaut)
#' @keywords chronique
#' @import tidyverse
#' @import lubridate
#' @import RcppRoll
#' @export
#' @examples
#' chronique.figure(data)
#' chronique.figure(data = mesdonnees, legendeY = "Température (°C)", duree = "Complet")
#' chronique.figure(data = tableaudonnee, Titre=nom, legendeY = "Température (°C)", save=T, format=".png")

chronique.analyse <- function(
  CodeRDT="",
  Annee="",
  data = data,
  pasdetemps = 1
  )
{
  
  ##### -------------- A FAIRE -------------- #####
  # Créer plusieurs modes de sortie : Complet, mensuel, journalier, synthèse, avec un test en fin de fonction qui crache des données différentes en fonction
  # Calcul du nb et de la durée de dépassement de 4 valeurs Vmin, Vmax, VminExt, VmaxExt
  # 
  # -------------- A FAIRE -------------- #
  #data <- DataTravail
  #data <- DataToAdd
  
  ##### Mise au format des données #####
  
  ## Transformation du format des dates
  data$Date <- as.Date(data$Date,format="%Y-%m-%d")
  data <-
    data %>% 
    mutate(Time = ymd_hms(paste(Date, Heure, sep = "_"))) %>% 
    filter(is.na(Valeur) == F) %>% 
    arrange(Time)

  ##### Valeurs instantanées remarquables #####
  ValRemarqInstant <-
    data %>% 
    summarise(
      VMinI = round(min(Valeur),1),
      VMedI = round(median(Valeur),1),
      VMoyI = round(mean(Valeur),1),
      VMaxI = round(max(Valeur),1),
      VAmpliI = VMaxI-VMinI,
      VarI = round(var(Valeur),2)
    )
  
  ##### Valeurs journalières remarquables #####
  ### Statistiques par jour ###
  ValJours <- 
    data %>% 
    group_by(Date) %>% 
    summarise(
      VMinJ = min(Valeur),
      VMedJ = median(Valeur),
      VMoyJ = mean(Valeur),
      VMaxJ = max(Valeur),
      VAmpliJ = VMaxJ-VMinJ,
      VarJ = var(Valeur),
      N = n()
    ) %>% 
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
      DateVMoyJMinPer = Date[VMoyJ == min(VMoyJ)],
      VMoyJMedPer = round(median(VMoyJ),1),
      VMoyJMoyPer = round(mean(VMoyJ),1),
      VMoyJMaxPer = round(max(VMoyJ),1),
      DateVMoyJMaxPer = Date[VMoyJ == max(VMoyJ)],
      AmplitudeVMoyJPer = VMoyJMaxPer-VMoyJMinPer,
      VarVMoyJ = round(var(VMoyJ),2)
    ) 
  
  ##### Valeurs sur périodes mobiles #### 
  ## Calcul V Moymax 7 et 30 J ##
  ValRemarqPeriodesMobiles <-
    ValJours %>% 
    filter(complete.cases(.)) %>% 
    summarise(
      VMaxMoy7J = max(VMM7j, na.rm = T),
      DateDebutTMaxMoy7J = Date[VMM7j == max(VMM7j, na.rm = T)] -6,
      DateFinTMaxMoy7J = Date[VMM7j == max(VMM7j, na.rm = T)],
      VMaxMoy30J = max(VMM30j, na.rm = T),
      DateDebutTMaxMoy30J = Date[VMM30j == max(VMM30j, na.rm = T)] -29,
      DateFinTMaxMoy30J = Date[VMM30j == max(VMM30j, na.rm = T)]
    ) %>% 
    mutate(VMaxMoy7J = round(VMaxMoy7J,1)) %>% 
    mutate(VMaxMoy30J = round(VMaxMoy30J,1))
  
  if(ValRemarqPeriodesMobiles$VMaxMoy7J < ValRemarqPeriodesMobiles$VMaxMoy30J) stop("Pb dans calcul Vmm")

  ## Extraction de l'année du VMM ##
  AnneeVMM <- year(ValRemarqPeriodesMobiles$DateFinTMaxMoy30J)
  
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
  DateDPeriode <- data$Date[1]
  #DateDPeriode <- format(data$Date[1], format="%d-%m-%Y")
  
  ## Date fin période ##
  DateFPeriode <- data$Date[dim(data)[1]]
  #DateFPeriode <- format(data$Date[dim(data)[1]], format="%d-%m-%Y")

  ## Nb Total de Jours ##
  NbJ <- length(unique(data$Date))
  
  ## Vérification de la cohérence entre le nombre de jours et la durée de la chronique ##
  intervalMax <- max(data$Time) - min(data$Time)
  if(abs(intervalMax - NbJ) <= 2) dureeTotale <- "Nb tot jours cohérent"
  if(abs(intervalMax - NbJ) > 2) dureeTotale <- "Nb tot jours incohérent"
  
  ## Vérification de la cohérence avec le nb de données attendues par jour ##
  
  # Totaux des jours OK et pas OK #
  NbJpasOK <- dim(ValJours %>% filter(N != Nref))[1]
  NbJOK <- dim(ValJours %>% filter(N == Nref))[1]
  
  #### Sortie des résultats ####
  Complet <- data.frame(NbJ, NbJOK, NbJpasOK, DateDPeriode, DateFPeriode, intervalMax, dureeTotale, CodeRDT, Annee, AnneeVMM, ValRemarqInstant$VMinI, ValRemarqInstant$VMaxI, ValRemarqInstant$VMoyI, ValRemarqInstant$VMedI, ValRemarqInstant$VarI, ValRemarqInstant$VAmpliI, ValRemarqJours$VMoyJMinPer, ValRemarqJours$DateVMoyJMinPer, ValRemarqJours$VMoyJMaxPer, ValRemarqJours$DateVMoyJMaxPer, ValRemarqJours$VMoyJMoyPer, ValRemarqJours$VMoyJMedPer, CVJ, ValRemarqJours$AmplitudeVMoyJPer, ValRemarqPeriodesMobiles$VMaxMoy7J, ValRemarqPeriodesMobiles$DateDebutTMaxMoy7J, ValRemarqPeriodesMobiles$DateFinTMaxMoy7J, ValRemarqPeriodesMobiles$VMaxMoy30J, ValRemarqPeriodesMobiles$DateDebutTMaxMoy30J, ValRemarqPeriodesMobiles$DateFinTMaxMoy30J) 
  
  
} # Fin de la fonction