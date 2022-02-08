#' Ajout des taxons et codes SANDRE
#'
#' Ajout des codes SANDRE et du nom du taxon de rang inférieur (espèce -> genre -> etc.) à partir d'un tableau issu de MI.systematique
#' @name MI.SANDRE
#' @keywords donnees
#' @import tidyverse
#' @export
#' @examples
#' MI.SANDRE(data)

MI.SANDRE <- function(data)
{
  # Création des champs à partir des espèces #
  data$taxon_libelle <- ifelse(is.na(data$sysesp_ranglibelle), NA_character_, data$sysesp_ranglibelle)
  data$taxon_sandre <- ifelse(is.na(data$sysesp_rangsandre), NA, data$sysesp_rangsandre)
  
  # Application pour les GENRES #
  data$taxon_libelle <- ifelse(is.na(data$taxon_libelle), # Si taxon vide
                            ifelse(is.na(data$sysgen_ranglibelle), NA, data$sysgen_ranglibelle), # on regarde si genre vide, si vide alors NA, sinon genre ajouté
                            data$taxon_libelle) # Si taxon pas vide, alors on remet taxon
  data$taxon_sandre <- ifelse(is.na(data$taxon_sandre), # Si taxon_sandre vide
                                 ifelse(is.na(data$sysgen_rangsandre), NA, data$sysgen_rangsandre), # on regarde si sysgen_rangsandre vide, si vide alors NA, sinon sysgen_rangsandre ajouté
                                 data$taxon_sandre) # Si taxon_sandre pas vide, alors on remet taxon_sandre

  # Application pour les SOUS-FAMILLES #  
  data$taxon_libelle <- ifelse(is.na(data$taxon_libelle), # Si taxon vide
                            ifelse(is.na(data$sysssfam_ranglibelle), NA, data$sysssfam_ranglibelle), # on regarde si sous-famille vide, si vide alors NA, sinon sous-famille ajouté
                            data$taxon_libelle) # Si taxon pas vide, alors on remet taxon
  data$taxon_sandre <- ifelse(is.na(data$taxon_sandre), # Si taxon_sandre vide
                                 ifelse(is.na(data$sysssfam_rangsandre), NA, data$sysssfam_rangsandre), # on regarde si sysssfam_rangsandre vide, si vide alors NA, sinon sysssfam_rangsandre ajouté
                                 data$taxon_sandre) # Si taxon_sandre pas vide, alors on remet taxon_sandre

  # Application pour les FAMILLES #
  data$taxon_libelle <- ifelse(is.na(data$taxon_libelle), # Si taxon vide
                            ifelse(is.na(data$sysfam_ranglibelle), NA, data$sysfam_ranglibelle), # on regarde si famille vide, si vide alors NA, sinon famille ajouté
                            data$taxon_libelle) # Si taxon pas vide, alors on remet taxon
  data$taxon_sandre <- ifelse(is.na(data$taxon_sandre), # Si taxon_sandre vide
                                 ifelse(is.na(data$sysfam_rangsandre), NA, data$sysfam_rangsandre), # on regarde si sysfam_rangsandre vide, si vide alors NA, sinon sysfam_rangsandre ajouté
                                 data$taxon_sandre) # Si taxon_sandre pas vide, alors on remet taxon_sandre
  
  # Application pour les ORDRES #
  data$taxon_libelle <- ifelse(is.na(data$taxon_libelle), # Si taxon vide
                            ifelse(is.na(data$sysord_ranglibelle), NA, data$sysord_ranglibelle), # on regarde si ordre vide, si vide alors NA, sinon ordre ajouté
                            data$taxon_libelle) # Si taxon pas vide, alors on remet taxon
  data$taxon_sandre <- ifelse(is.na(data$taxon_sandre), # Si taxon_sandre vide
                                 ifelse(is.na(data$sysord_rangsandre), NA, data$sysord_rangsandre), # on regarde si sysord_rangsandre vide, si vide alors NA, sinon sysord_rangsandre ajouté
                                 data$taxon_sandre) # Si taxon_sandre pas vide, alors on remet taxon_sandre
  
  return(data)
  
} # Fin de la fonction