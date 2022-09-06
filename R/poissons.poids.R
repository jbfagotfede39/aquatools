#' Calcul des poids attendus en fonction de la taille pour un jeu de captures
#'
#' Calcule, à partir d'une jeu de données de captures, les poids attendus pour des tailles données
#' @name poissons.poids
#' @param data Jeu de données de référence, issu de \code{poissons.captures}
#' @param data_a_completer Jeu de données à compléter (optionnel, si absent, sortie d'un tableau de référence), issu ou bien au format de \code{poissons.captures}
#' @param pas_taille Pas de taille (25 mm par défaut) des estimations produites. Ce paramètre est ignoré dans le cas du complément d'un jeu de données fourni via le paramètre \code{data_a_completer}.
#' @keywords donnees
#' @import broom
#' @import FSA
#' @import modelr
#' @import tidyverse
#' @export
#' @examples
#' poissons.poids(Captures)
#' poissons.poids(Captures, pas_taille = 10)
#' jeu_donnees_reference %>%
#'    filter(codeespece %in% liste_especes_traitees$codeespece) %>%
#'    group_split(codeespece) %>% 
#'    map_dfr(~ poissons.poids(data = ., data_a_completer = data_brutes))

poissons.poids <- function(  
  data = data,
  data_a_completer = NA_character_,
  pas_taille = 25)
{

#### Test et références ####
especes <-
  data %>% 
  {if("ref_espece" %in% names(.)) rename(., codeespece = ref_espece) else .} %>% 
  distinct(codeespece)
if(nrow(especes) > 1) stop(glue("Plusieurs espèces en présence alors qu'il n'en faut qu'une : {glue_collapse(especes$codeespece, ', ', last = ' et ')}"))
if(nrow(data) < 30) stop("Il y a moins de 30 captures fournies en entrée")

milieux <- 
  data %>% 
  stations.coderhj(DistSource = F, ColonneEntree = "nom", ColonneSortie = "milieu") %>% 
  distinct(milieu)
  
##### Importation et transformation des données #####
data <-
  data %>% 
  dplyr::rename(tl = taillemoy) %>% 
  dplyr::rename(wt = poids_moy) %>% 
  dplyr::mutate(logW = log10(wt), logL = log10(tl))
  
##### Modélisation ######
fit1 <- lm(logW ~ logL, data = data)
p_value <- 
  broom::tidy(fit1) %>% 
  filter(term != "(Intercept)") %>% 
  select(p.value) %>% 
  pull()

###### Estimation #####
## Amplitude des données de référence ##
limites <- range(data$tl, na.rm = T)
limites[1] <- floor(limites[1] / pas_taille) * pas_taille
limites[2] <- ceiling(limites[2] / pas_taille) * pas_taille

## Estimations à partir des données de référence ##
lens <- seq(from = limites[1], by = pas_taille, length = (limites[2]-limites[1])/pas_taille+1)

nd <- data.frame(logL = log10(lens))  # df of log(lengths)
plogW <- predict(fit1, nd, interval="prediction") # Permet d'obtenir l'enveloppe afin de deviner de futures valeurs
cf <- FSA::logbtcf(fit1, 10) # correction factor

## Création d'un tableau générique de prédiction si absence de données à compléter ##
if(is.null(nrow(data_a_completer))){
  estimations <- data.frame(lens, cf * 10^plogW) # Dataframe contenant les poids pour une longueur donnée, avec l'intervalle de confiance à 95 %
  estimations <-
    estimations %>% 
    dplyr::mutate(fit = round(fit, 1)) %>% 
    dplyr::mutate(lwr = round(lwr, 1)) %>% 
    dplyr::mutate(upr = round(upr, 1)) %>% 
    # dplyr::rename('Taille (mm)' = lens) %>% 
    # dplyr::rename('Poids estimé (g)'= fit) %>% 
    # dplyr::rename('Poids minimum à 95% (g)'= lwr) %>%
    # dplyr::rename('Poids maximum à 95% (g)'= upr)   dplyr::rename('Taille (mm)' = lens) %>% 
    dplyr::rename('taille' = lens) %>%
    dplyr::rename('poids_estime'= fit) %>% 
    dplyr::rename('poids_min_95_pour_100'= lwr) %>%
    dplyr::rename('poids_max_95_pour_100'= upr)
}

## Complément des données d'entrée à compléter si présentes ##
if(!is.null(nrow(data_a_completer))){
  resultats_partiels <- 
    data_a_completer %>% 
      filter(codeespece == especes %>% pull()) %>% # Pour ne conserver que les individus pour lesquels le poids est à estimer
      filter(is.na(poids)) %>% # Pour ne conserver que les individus pour lesquels le poids est à estimer
      filter(taillemoy >= limites[1]) %>% # Pour ne conserver que les individus dont la taille est dans l'enveloppe d'estimation
      filter(taillemoy <= limites[2]) %>% # Pour ne conserver que les individus dont la taille est dans l'enveloppe d'estimation
      mutate(logL = log10(taillemoy)) # Calcul du log de la taille
      # modelr::add_predictions(fit1) %>% # Calcul de l'estimation du log du poids - Fonctionne mais ne permet pas l'ajout de l'intervalle de confiance
  
  resultats_partiels_avec_intervalles <- predict(fit1, resultats_partiels %>% select(logL) %>% as.data.frame(), interval="prediction")
    
  estimations <-
    resultats_partiels %>% 
    bind_cols(as_tibble(resultats_partiels_avec_intervalles)) %>% # Conversion en tibble nécessaire car on part d'une matrice, et celui peut dans certains cas poser pb de renommage de champs
    mutate(lm_fit = cf * 10^fit) %>% # Rétro-calcul du poids estimé à partir de son log et d'un facteur de correction
    mutate(lm_lwr = round(cf * 10^lwr, 1)) %>% # Rétro-calcul du poids estimé à partir de son log et d'un facteur de correction
    mutate(lm_upr = round(cf * 10^upr, 1)) %>% # Rétro-calcul du poids estimé à partir de son log et d'un facteur de correction
    mutate(poids_moy = round(lm_fit, 1)) %>% # Calcul du poids individuel moyen
    mutate(poids = round(poids_moy * nombre, 1)) %>% # Calcul du poids du lot
    rename(poids_min_95_pour_100 = lm_lwr, 
           poids_max_95_pour_100 = lm_upr) %>% 
    select(nom:poids_moy, contains("_95_pour_100"))
  
}

#### Mise en forme et compléments génériques ####
estimations <-
  estimations %>% 
  mutate(p_value = p_value) %>% 
  mutate(ref_espece = especes %>% pull()) %>% 
  mutate(ref_milieux = milieux %>% pull() %>% glue_collapse(sep = ",")) %>% 
  {if(is.null(nrow(data_a_completer))) select(., contains("ref"), everything()) else .} %>% 
  {if(!is.null(nrow(data_a_completer))) select(., everything(), contains("ref")) else .}

#### Vérification de la qualité des résultats ####
## Qualité du modèle ##
valeurs_problematiques <-
  estimations %>% 
  filter(p_value > 0.05) %>% 
  distinct(p_value)
if(nrow(valeurs_problematiques) > 0) warning("Attention à vérifier les p-value car certaines sont supérieures à 0.05")

## Complétude des résultats ##
if(!is.null(nrow(data_a_completer))){
  valeurs_vides <-
    estimations %>% 
    filter(is.na(poids))
  if(nrow(valeurs_vides) == 1) warning("Attention : présence de 1 individu/lot non estimé")
  if(nrow(valeurs_vides) > 1) warning(glue("Attention : présence de {nrow(valeurs_vides)} individus/lots non estimés"))
}

#### Sortie des résultats ####

return(estimations)
} # Fin de la fonction