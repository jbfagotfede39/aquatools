#' Filtrage des résultats de chroniques
#'
#' Cette fonction permet de filtrer les résultats de chroniques 
#' @name chronique.resultats.filtrage
#' @param data Data.frame issu de la fonction chronique.resultats ou de la base de données Chroniques
#' @param anneevmm Si \code{TRUE} (par défault), ne conserve que les résultats dont l'année de fin de vmm30j est égale à l'année biologique du résultat
#' @param datefperiode Ne conserver que les résultats dont la date de fin de période est postérieure à la date choisie ("07-15", soit le 15 juillet par défault)
#' @param nbj Nombre de journées minimales pour chaque résultat à conserver (75 par défault).
#' @keywords chronique
#' @import tidyverse
#' @export
#' @examples
#' Resultats %>% chronique.resultats.filtrage()
#' Resultats %>% chronique.resultats.filtrage(datefperiode = "08-01")

##### TODO LIST #####
# Créer une fonction plus générale pour le renommage initial des champs, à ensuite également utiliser dans chronique.resultat.filtrage() + chronique.cle() + chronique.resultats.periode
#####################

chronique.resultats.filtrage <- function(
  data = data,
  anneevmm = TRUE,
  datefperiode = "07-15",
  nbj = 75
  )
{
  
  #### Évaluation des choix ####
  #cle <- match.arg(cle)
  
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

  #### Filtrage ####
  Vue <-
    data %>% 
    {if(anneevmm == TRUE) filter(., paramannee == paramanneevmm) else .} %>% # Afin de supprimer les années avec une Vmm incohérente
    filter(paramdatefperiode > ymd(glue('{paramannee}-{datefperiode}'))) %>% # Afin de supprimer les chroniques qui ne couvrent pas la période estivale
    dplyr::filter(paramnbj > nbj) # Afin de supprimer les années avec une Vmm incohérente
  
  #### Vérification des noms de champs ####
  # Format standard sans chres_ pour utilisation autonome de l'outil
  Vue <-
    Vue %>% 
    select(-contains("param"))
  
  #### Affichage des résultats ####
  return(Vue)
}