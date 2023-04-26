#' Renommage des champs de données physico-chimiques issues de Hub'eau
#'
#' Renommage des champs de données physico-chimiques issues de Hub'eau vers le format de la base de données interne
#' @name PC.renommage
#' @param data Jeu de données issu de Hub'eau
#' @keywords donnees
#' @import tidyverse
#' @export
#' @examples
#' PC.renommage(data)

PC.renommage <- function(data)
{

  ## Liste de référence ##
  renommage <- c(pcmes_codesie = 'code_station',
                 pcmes_coderhj = 'libelle_station',
                 pcmes_date = 'date_prelevement',
                 pcmes_heure = 'heure_prelevement',
                 pcmes_parametresandre = 'code_parametre',
                 pcmes_parametrenom = 'libelle_parametre',
                 pcmes_valeur = 'resultat',
                 pcmes_unitenom = 'symbole_unite',
                 pcmes_unitesandre = 'code_unite',
                 pcmes_supportnom = 'libelle_support',
                 pcmes_supportsandre = 'code_support',
                 pcmes_qualificationnom = 'libelle_qualification',
                 pcmes_qualificationsandre = 'code_qualification',
                 pcmes_laboratoirenom = 'nom_laboratoire',
                 pcmes_laboratoiresiret = 'code_laboratoire',
                 pcmes_fractionnom = 'libelle_fraction',
                 pcmes_fractionsandre = 'code_fraction',
                 pcmes_coderemarque = 'code_remarque',
                 pcmes_codeinsitu = 'code_insitu',
                 pcmes_limitequantification = 'limite_quantification',
                 pcmes_limitedetection = 'limite_detection',
                 pcmes_methodenom = 'nom_methode_analyse',
                 pcmes_methodesandre = 'code_methode_analyse',
                 pcmes_producteurnom = 'nom_producteur_analyse',
                 pcmes_producteursandre = 'code_producteur_analyse'
  )
  
  data <-
    data %>% 
    rename(any_of(renommage))

##### Renvoi des données #####
return(data)

}