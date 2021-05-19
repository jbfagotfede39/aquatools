#' Calcul d'une durée de degrés-jours
#'
#' Cette fonction permet de calcul une date de démarrage et de fin de reproduction à partir de données de degrés-jours
#' @name chronique.degresjours
#' @param data Data.frame de données journalières issu de la fonction \code{chronique.agregation}
#' @param espece Espèce recherchée (vide par défaut - Si complété, va directement chercher les valeurs de référence dans \code{data(PoissonsThermieReproduction)} - Exemple : TRF, BRO)
#' @param valeurdeclenchement Température de déclenchement de la phase de reproduction (pour recherche automatique de la date de déclenchement)
#' @param typevaleurdeclenchement Si \code{Montant} (par défault), s'appuie sur une chronique printanière (valeurs croissantes : BRO, OBR, GAR, etc.). Si \code{Descendant}, s'appuie sur une chronique automnale (TRF)
#' @param nbjourssuccessifsdeclenchement Nombre de jours successifs (\code{2} par défaut) devant afficher une température moyenne supérieure ou inférieure (en fonction de typevaleurdeclenchement) à la valeurdeclenchement
#' @param datedeclenchementmanuelle Si l'opérateur souhaite forcer manuellement la date de démarrage du calcul (au format 2020-12-01)
#' @param valeurdegresjours Valeur de degrés-jours de référence
#' @keywords chronique
#' @import glue
#' @import tidyverse
#' @export
#' @examples
#' Resultats %>% chronique.degresjours()
#' Resultats %>% chronique.degresjours(valeurdeclenchement = 10, typeseuildeclenchement = "Montant", nbjourssuccessifsdeclenchement = 2, valeurdegresjours = 180)
#' Resultats %>% chronique.degresjours(valeurdegresjours = 180, datedeclenchementmanuelle = "2021-03-30")
#' test <- ValJours %>% filter(chmes_coderhj == "AIN22-2") %>% formatage.annee.biologique() %>% group_by(chmes_coderhj, chmes_anneebiol) %>% group_split() 
#' listeespecesatraiter <- c("TRF", "OBR", "BRO", "GAR", "TAN", "CCO", "HOT", "BAF")
#' argList <- list(x = test, y = listeespecesatraiter)
#' crossArg <- cross_df(argList)
#' test <- map2_dfr(crossArg$x, crossArg$y, chronique.degresjours)

##### TODO LIST #####
# Implantation dans chronique.analyse()
# Calcul des faux épisodes de déclenchement pour sortie de synthèse même dans le cas où la date est forcée
# 
#####################

chronique.degresjours <- function(
  data = data,
  espece = NA_character_,
  valeurdeclenchement = 10,
  typeseuildeclenchement = c("Montant", "Descendant"),
  nbjourssuccessifsdeclenchement = 2,
  datedeclenchementmanuelle = NA_character_,
  valeurdegresjours = 180
)
{
  
  #### Évaluation des choix ####
  typeseuildeclenchement <- match.arg(typeseuildeclenchement)
  if(!is.na(datedeclenchementmanuelle)){
    datedeclenchementmanuelle <- ymd(datedeclenchementmanuelle)
    if(month(datedeclenchementmanuelle) %in% c(1, 2, 3, 4, 5, 6, 7)) typeseuildeclenchement = "Montant"
    if(month(datedeclenchementmanuelle) %in% c(9, 10, 11, 12)) typeseuildeclenchement = "Descendant"
  }
  
  ##### Contexte de la chronique #####
  Contexte <- 
    data %>% 
    chronique.contexte()
  if(dim(data)[1] == 0) stop("Aucune donnée dans la chronique à analyser")
  if(Contexte$nstation > 1) stop("Différentes stations dans la chronique à analyser")
  if(Contexte$nannee > 1) stop("Différentes années biologiques dans la chronique à analyser")
  
  #### Recherche des données de référence ####
  if(!is.na(espece)){
    data(PoissonsThermieReproduction)
    espececoncernee <-
      PoissonsThermieReproduction %>% 
      filter(codeespece == espece)
    if(nrow(espececoncernee) == 0) stop(glue("Absence de données de référence pour l'espèce {espece}"))
    valeurdeclenchement <- espececoncernee %>% filter(grepl("Température déclenchement reproduction", parametre)) %>% summarise((mean = mean(value, na.rm = T))) %>% pull()
    typeseuildeclenchement <- espececoncernee %>% filter(grepl("Température déclenchement reproduction", parametre)) %>% dplyr::select(parametre) %>% distinct() %>% pull()
    typeseuildeclenchement <- ifelse(typeseuildeclenchement == "Température déclenchement reproduction descendante", "Descendant", "Montant")
    valeurdegresjours <- espececoncernee %>% filter(grepl("Durée d'émergence après la ponte|Durée d'incubation après la ponte", parametre)) %>% summarise((mean = mean(value, na.rm = T))) %>% pull()
  }
  
  #### Calcul des jours de déclenchement ####
  calculdeclenchement <-
    data %>%
    ungroup() %>% 
    {if (!is.na(datedeclenchementmanuelle)) filter(., chmes_date >= datedeclenchementmanuelle) else .} %>% # Filtrage général sur la période concernée
    {if (is.na(datedeclenchementmanuelle) & typeseuildeclenchement == "Montant") filter(., chmes_date >= paste0(year(last(chmes_date)),"-02-01")) else .} %>% # Filtrage général sur la période printanière
    {if (is.na(datedeclenchementmanuelle) & typeseuildeclenchement == "Descendant") filter(., chmes_date <= paste0(year(last(chmes_date)),"-02-01")) else .} %>% # Filtrage général sur la période hivernale
    {if (typeseuildeclenchement == "Montant") mutate(., TestValeurSup = ifelse(VMoyJ > valeurdeclenchement, 1, NA)) else .} %>% # identification des journées passant au-dessus d'une valeur repère
    {if (typeseuildeclenchement == "Descendant") mutate(., TestValeurSup = ifelse(VMoyJ <= valeurdeclenchement, 1, NA)) else .} %>% # identification des journées passant au-dessous d'une valeur repère
    {if (!is.na(datedeclenchementmanuelle)) mutate(., TestValeurSup = ifelse(row_number() == 1, 1, TestValeurSup)) else .} %>% # identification des journées passant au-dessous d'une valeur repère
    mutate(groupe = ifelse(!is.na(TestValeurSup), row_number(), NA)) %>% # on attribue un numéro de groupe pour chaque "saut"
    filter(!is.na(TestValeurSup)) %>% # on supprime les jours ne répondant pas à la condition de température minimale
    mutate(conditionsuccession = ifelse(lead(groupe, nbjourssuccessifsdeclenchement) - groupe == nbjourssuccessifsdeclenchement, "TRUE", "FALSE")) %>% # On établit si les jours successifs sont en nombre suffisant par rapport à la condition nbjourssuccessifsdeclenchement
    {if (!is.na(datedeclenchementmanuelle)) mutate(., conditionsuccession = ifelse(row_number() == 1, "TRUE", conditionsuccession)) else .} # On établit si les jours successifs sont en nombre suffisant par rapport à la condition nbjourssuccessifsdeclenchement
    
  #### Analyse des jours de décenchement potentiels non satisfaisants ####
  episodesdeclenchementanterieurspasassezlongs <-
    calculdeclenchement %>% 
    mutate(datesuivante = lead(chmes_date)) %>% 
    mutate(conditiondate = datesuivante - chmes_date) %>% 
    mutate(numeroepisode = ifelse(lead(conditiondate) - conditiondate == 0, NA, groupe)) %>% 
    mutate(numeroepisode = ifelse(conditiondate != 1, numeroepisode, NA)) %>% 
    filter(chmes_date <= first(chmes_date[conditionsuccession == TRUE])) %>% 
    fill(numeroepisode, .direction = "up") %>% 
    group_by(numeroepisode) %>% 
    filter(row_number()==1) %>% 
    ungroup()
  
  NbEpisodesAnterieurs <- nrow(episodesdeclenchementanterieurspasassezlongs)
  DatesEpisodesAnterieurs <- episodesdeclenchementanterieurspasassezlongs %>% distinct(chmes_date) %>% pull()
  
  #### Calcul des dates de début, de fin et durée en jours de la phase
  degresjours <-
    calculdeclenchement %>% 
    filter(chmes_date >= first(chmes_date[conditionsuccession == TRUE])) %>% 
    mutate(DegJours = cumsum(VMoyJ)) %>%
    summarise(
      DateDebutdegresjours = chmes_date[first(TestValeurSup)],
      DateFindegresjours = first(chmes_date[DegJours > valeurdegresjours])
    ) %>%
    mutate(DateDebutdegresjours = ymd(DateDebutdegresjours)) %>%
    mutate(DateFindegresjours = ymd(DateFindegresjours)) %>%
    mutate(NbJdegresjours = as.numeric(DateFindegresjours - DateDebutdegresjours))
  
  #### Regroupement des métriques ####
  Vue <- 
    degresjours %>% 
    {if (NbEpisodesAnterieurs != 0) add_column(., NbEpisodesAnterieurs = NbEpisodesAnterieurs) else .} %>% 
    {if (NbEpisodesAnterieurs == 0) add_column(., NbEpisodesAnterieurs = 0) else .} %>% 
    {if (NbEpisodesAnterieurs != 0) add_column(., DatesEpisodesAnterieurs = glue_collapse(DatesEpisodesAnterieurs, sep = ";")) else .} %>% 
    {if (NbEpisodesAnterieurs == 0) add_column(., DatesEpisodesAnterieurs = NA_character_) else .} %>% 
    {if (!is.na(datedeclenchementmanuelle)) mutate(., NbEpisodesAnterieurs = NA_integer_) else .} %>% 
    {if (!is.na(datedeclenchementmanuelle)) mutate(., DatesEpisodesAnterieurs = NA_character_) else .} %>% 
    add_column(valeurdeclenchement = valeurdeclenchement) %>% 
    add_column(typeseuildeclenchement = typeseuildeclenchement) %>% 
    add_column(nbjourssuccessifsdeclenchement = nbjourssuccessifsdeclenchement) %>% 
    add_column(datedeclenchementmanuelle = datedeclenchementmanuelle) %>% 
    add_column(valeurdegresjours = valeurdegresjours) %>% 
    add_column(espece = espece) %>% 
    add_column(station = Contexte$station) %>% 
    add_column(annee = Contexte$annee) %>% 
    dplyr::select(station, annee, espece, valeurdegresjours, typeseuildeclenchement, valeurdeclenchement, nbjourssuccessifsdeclenchement, datedeclenchementmanuelle, everything())

  #### Affichage des résultats ####
  return(Vue)
    }
