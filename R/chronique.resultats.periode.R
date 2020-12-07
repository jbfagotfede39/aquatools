#' Périodes des résultats de chroniques
#'
#' Cette fonction permet de calculer les périodes couvertes par les résultats de chroniques (champs PeriodeTotale)
#' @name chronique.resultats.periode
#' @param data Data.frame issu de la fonction chronique.resultats ou de la base de données Chroniques
#' @param typeperiode Type de période que l'on souhaite calculer : période totale (\code{Enveloppe} : 2014-2018), ou période fractionnée (\code{Detail} : 2014-2015;2017;2019-2020, par défaut)
#' @keywords chronique
#' @import glue
#' @import tidyverse
#' @export
#' @examples
#' Resultats %>% chronique.resultats.periode()
#' chronique.resultats("2017","Année biologique") %>% chronique.resultats.periode()

##### TODO LIST #####
# Vérifier à quoi ressemble le cas où il n'y a qu'une année, et faire pour que cette année seule apparaisse plutôt que 2020-2020
# Implanter cette fonction dans chronique.analyse() ou chronique.traitement() plutôt, afin que le calcul de l'enveloppe soit directement juste, notamment pour les autres FD
# Créer une fonction plus générale pour le renommage initial des champs, à ensuite également utiliser dans chronique.resultat.filtrage() + chronique.cle() + chronique.resultats.periode
# Améliorer le fonctionnement en ajoutant une symbolique (3ème version) avec les années/périodes complètes ou incomplètes (parenthèses ou accolades pour les années incomplètes ?)
#####################

chronique.resultats.periode <- function(
  data = data,
  typeperiode = c("Detail", "Enveloppe")
  )
{
  
  #### Évaluation des choix ####
  typeperiode <- match.arg(typeperiode)
  
  #### Vérification des noms de champs ####
  # Format standard sans chres_ pour utilisation autonome de l'outil
  data <-
    data %>% 
    {if("AnneeVMM" %in% colnames(.)) mutate(., paramanneevmm = AnneeVMM) else .} %>% 
    {if("chres_anneevmm" %in% colnames(.)) mutate(., paramanneevmm = chres_anneevmm) else .} %>% 
    {if("Annee" %in% colnames(.)) mutate(., paramannee = Annee) else .} %>%
    {if("chres_annee" %in% colnames(.)) mutate(., paramannee = chres_annee) else .} %>% 
    {if("DateFPeriode" %in% colnames(.)) mutate(., paramdatefperiode = DateFPeriode) else .} %>% 
    {if("chres_datefperiode" %in% colnames(.)) mutate(., paramdatefperiode = chres_datefperiode) else .} %>% 
    {if("NbJ" %in% colnames(.)) mutate(., paramnbj = NbJ) else .} %>% 
    {if("chres_nbj" %in% colnames(.)) mutate(., paramnbj = chres_nbj) else .} %>% 
    {if("Coderhj" %in% colnames(.)) mutate(., paramcoderhj = Coderhj) else .} %>% 
    {if("chres_coderhj" %in% colnames(.)) mutate(., paramcoderhj = chres_coderhj) else .} %>% 
    {if("chsta_coderhj" %in% colnames(.)) mutate(., paramcoderhj = chsta_coderhj) else .} %>% 
    {if("Typemesure" %in% colnames(.)) mutate(., paramtypemesure = Typemesure) else .} %>% 
    {if("chres_typemesure" %in% colnames(.)) mutate(., paramtypemesure = chres_typemesure) else .}

  #### Dédoublonnage et tri ####
  dataagrege <- 
    data %>% 
    {if(nrow(data) != 0) arrange(., paramcoderhj, paramtypemesure, paramannee) else .} %>%
    distinct(paramcoderhj, paramtypemesure, paramannee) %>% 
    group_by(paramcoderhj, paramtypemesure)

  
  #### Calcul ####
  if(typeperiode == "Enveloppe"){
  Vue <-
    dataagrege %>% 
    summarise(min = min(paramannee),
              max = max(paramannee)) %>% 
    mutate(PeriodeTotale = paste0(min, " - ", max)) %>% 
    select(-min,-max) %>% 
    left_join(data, by = c("paramcoderhj", "paramtypemesure")) %>% 
    ungroup()
  }
  
  if(typeperiode == "Detail"){
  Vue <-
    dataagrege %>% 
    mutate(annee = paramannee) %>% 
    complete(paramannee = min(.$paramannee):max(.$paramannee), nesting(paramcoderhj, paramtypemesure)) %>% 
    group_by(paramcoderhj, paramtypemesure) %>%
    mutate(ponctuation = ifelse(lead(annee+1) == annee, ";", "-")) %>%
    mutate(ponctuation = ifelse(is.na(lead(annee)), ";", "-")) %>%
    filter(!is.na(annee)) %>%
    filter(!(ponctuation == "-" & lag(ponctuation) == "-" & annee != min(paramannee))) %>%
    mutate(ponctuation = ifelse(annee == max(annee), NA, ponctuation)) %>%
    mutate(Vue = glue("{paramannee}{ponctuation}", .na = "")) %>%
    summarise(PeriodeFractionnee = str_c(Vue, collapse = "")) %>% # Agrégation de l'ensemble des valeurs de la colonne en une unique chaîne de caractères
    left_join(data, by = c("paramcoderhj", "paramtypemesure"))
  }
  
  #### Vérification des noms de champs ####
  # Format standard sans chres_ pour utilisation autonome de l'outil
  Vue <-
    Vue %>% 
    select(-contains("param"))
  
  #### Affichage des résultats ####
  return(Vue)
}
