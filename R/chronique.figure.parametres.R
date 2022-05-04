#' Paramétrisation des vues de chroniques
#'
#' Cette fonction permet de sélectionner automatiquement les paramètres de légende des représentations graphiques de chroniques à partir des données issues de chronique.contexte
#' @name chronique.figure.parametres
#' @param data Data.frame issu de la fonction chronique.contexte()
#' @param typefigure Type de figure qui est à paramétriser : \code{valeurs} (par défault), \code{cumul} ou \code{vmm30j}
#' @keywords chronique
#' @import glue
#' @export
#' @examples
#' chronique.mesures("DRO6-8") %>% chronique.contexte() %>% chronique.figure.parametres()
#' 
#' parametres <- chronique.mesures("DRO6-8") %>% chronique.contexte() %>% chronique.figure.parametres()
#' unite <- parametres$unite
#' legendeY <- parametres$legendeY
#' legendeTitre <- parametres$legendeTitre
#' typemesureTitreSortie <- parametres$typemesureTitreSortie

chronique.figure.parametres <- function(
  data = data,
  typefigure = c("valeurs", "cumul", "vmm30j")
)
{
  
  #### Évaluation des choix ####
  typefigure <- match.arg(typefigure)
  
  #### Contexte des données ####
  contextereference <- structure(list(nstation = integer(0), nmo = integer(0), ntypemesure = integer(0), 
                                      nunite = integer(0), nannee = integer(0), nmilieu = integer(0), 
                                      station = structure(character(0), class = c("glue", "character"
                                      )), mo = structure(character(0), class = c("glue", "character"
                                      )), typemesure = structure(character(0), class = c("glue", 
                                                                                         "character")), unite = structure(character(0), class = c("glue", 
                                                                                                                                                  "character")), annee = structure(character(0), class = c("glue", 
                                                                                                                                                                                                           "character")), milieu = structure(character(0), class = c("glue", 
                                                                                                                                                                                                                                                                     "character"))), row.names = integer(0), class = c("tbl_df", 
                                                                                                                                                                                                                                                                                                                       "tbl", "data.frame"))

  #### Tests ####
  if(any(names(data) == names(contextereference)) == FALSE) stop("Les données d'entrée ne sont pas au format de sortie de chronique.contexte")
  if(data$ntypemesure != 1) stop("Plusieurs chmes_typemesure au sein du jeu de données")

  #### Création des objets vides pour tests ultérieurs ####
  legendeY <- NA_character_
  legendeTitre <- NA_character_
  typemesureTitreSortie <- NA_character_
  classes <- NA_character_
  palette <- NA_character_
  
  #### Ajustement des paramètres en fonction du typemesure ####
  typemesure <- data$typemesure
  if(is.na(typemesure)) stop("Pas de typemesure de défini")
  
  if(typemesure == "Thermie" | typemesure == "Thermie barométrique" | typemesure == "Thermie piézométrique"){
    typemesureTitreSortie <- "_thermie_"
    unite <- "°C"
    legendeTitre <- "Températures :"
    classes <- c(-15, seq(18, 23, by = 1), 45)
    palette <- c("< 18"="blue",
                 "[18,19)"="green",
                 "[19,20)"="yellow",
                 "[20,21)"="orange",
                 "[21,22)"="red",
                 "[22,23)"="purple",
                 "> 23"="black"
    )
    
    if(typefigure == "valeurs"){
      legendeY <- glue("Température ({unite})")

    }
    if(typefigure == "vmm30j"){
      legendeY <- glue("Tmm30j ({unite})")
    }
    if(typefigure == "cumul"){
      legendeY <- "Somme des degrés-jours"
      if(data$nannee == 1 & data$nstation == 1) legendeTitre <- "Année biologique"
      if(data$nannee != 1 & data$nstation == 1) legendeTitre <- "Années biologiques"
      if(data$nannee != 1 & data$nstation != 1) legendeTitre <- "Couples station-année"
      classes <- NA_character_
      palette <- NA_character_
    }
  }
  
  if(typemesure == "Barométrie"){
    if(typefigure == "valeurs"){
      # unite <- quote(cm~H2O)
      unite <- quote(cm~H[2]*O) # À voir à l'usage si ça fonctionne
      legendeY = bquote("Pression" ~ "atmosphérique" ~ "(" ~ .(unite) ~ ")")
      legendeTitre <- "Barométrie :"
      typemesureTitreSortie <- "_barométrie_"
    }
  }
  
  if(typemesure == "Piézométrie" | typemesure == "Piézométrie brute" | typemesure == "Piézométrie compensée" | typemesure == "Piézométrie calée"){
    if(typefigure == "valeurs"){
      unite <- "cm"
      legendeY <- glue("Hauteur d'eau ({unite})")
      legendeTitre <- "Piézométrie :"
      typemesureTitreSortie <- "_piézométrie_"
    }
  }
  
  if(typemesure == "Piézométrie NGF"){
    if(typefigure == "valeurs"){
      unite <- "m NGF"
      legendeY <- glue("Hauteur d'eau ({unite})")
      legendeTitre <- "Piézométrie :"
      typemesureTitreSortie <- "_piézométrie_"
    }
  }
  
  if(typemesure == "Oxygénation"){
    if(typefigure == "valeurs"){
      unite <- quote(mg~O[2]/L)
      legendeY = bquote("Oxygène" ~ "dissous" ~ "(" ~ .(unite) ~ ")")
      legendeTitre <- "Oxygénation :"
      typemesureTitreSortie <- "_oxygénation_"
      classes <- seq(0, 180, by = 20)
      palette <- c("< 20"="black",
                   # "[0,20)"="black",
                   "[20,40)"="red",
                   "[40,60)"="orange",
                   "[60,80)"="yellow",
                   "[80,100)"="green",
                   "[100,120)"="blue",
                   "[120,140)"="yellow",
                   "[140,160)"="orange",
                   "[160,180)"="red",
                   "> 180"="black"
      )
    }
  }
  
  if(typemesure == "Hydrologie"){
    typemesureTitreSortie <- "_hydrologie_"
    unite <- quote(m^3/s)
    legendeTitre <- "Hydrologie :"
    if(typefigure == "valeurs"){
      legendeY = bquote("Débit" ~ "(" ~ .(unite) ~ ")")
    }
    if(typefigure == "cumul"){
      stop("Légendes à modifier")
      legendeY = bquote("Débit" ~ "(" ~ .(unite) ~ ")")
    }
  }
  
  if(typemesure == "Pluviométrie"){
    typemesureTitreSortie <- "_pluviométrie_"
    unite <- quote(L/m^2)
    legendeTitre <- "Pluviométrie :"

    if(typefigure == "valeurs"){
      legendeY = bquote("Précipitations" ~ "(" ~ .(unite) ~ ")")
    }
    if(typefigure == "cumul"){
      legendeY = bquote("Précipitations" ~ "cumulées" ~ "(" ~ .(unite) ~ ")")
    }
  }
  
  #### Tests ####
  if(any(is.na(as.character(unite)))) stop("Paramètres d'unité de figure à créer")
  if(any(is.na(as.character(legendeY)))) stop("Paramètres de titres de figure à créer")
  if(is.na(legendeTitre)) stop("Paramètres de titres de figure à créer")
  if(is.na(typemesureTitreSortie)) stop("Paramètres de titres de figure à créer")
  
  #### Regroupement des valeurs ####
  parametres <- NULL
  parametres$unite <- unite
  parametres$legendeY <- legendeY
  parametres$legendeTitre <- legendeTitre
  parametres$typemesureTitreSortie <- typemesureTitreSortie
  parametres$classes <- classes
  parametres$palette <- palette
  
  #### Affichage des résultats ####
  return(parametres)

} # Fin de la fonction
