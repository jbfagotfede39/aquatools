#' Ajout des taxons et codes SANDRE
#'
#' Ajout des codes SANDRE et du nom du taxon de rang inférieur (espèce -> genre -> etc.) à partir d'un tableau issu de MI.systematique
#' @keywords donnees
#' @import dplyr
#' @export
#' @examples
#' MI.SANDRE(data)

MI.SANDRE <- function(data)
{
  # Création des champs à partir des espèces #
  data$Taxon <- ifelse(is.na(data$Espece), NA, data$Espece)
  data$CodeSANDRE <- ifelse(is.na(data$EspeceCodeSandre), NA, data$EspeceCodeSandre)
  
  # Application pour les GENRES #
  data$Taxon <- ifelse(is.na(data$Taxon), # Si taxon vide
                            ifelse(is.na(data$Genre), NA, data$Genre), # on regarde si genre vide, si vide alors NA, sinon genre ajouté
                            data$Taxon) # Si taxon pas vide, alors on remet taxon
  data$CodeSANDRE <- ifelse(is.na(data$CodeSANDRE), # Si CodeSANDRE vide
                                 ifelse(is.na(data$GenreCodeSandre), NA, data$GenreCodeSandre), # on regarde si GenreCodeSandre vide, si vide alors NA, sinon GenreCodeSandre ajouté
                                 data$CodeSANDRE) # Si CodeSANDRE pas vide, alors on remet CodeSANDRE

  # Application pour les SOUS-FAMILLES #  
  data$Taxon <- ifelse(is.na(data$Taxon), # Si taxon vide
                            ifelse(is.na(data$SousFamille), NA, data$SousFamille), # on regarde si sous-famille vide, si vide alors NA, sinon sous-famille ajouté
                            data$Taxon) # Si taxon pas vide, alors on remet taxon
  data$CodeSANDRE <- ifelse(is.na(data$CodeSANDRE), # Si CodeSANDRE vide
                                 ifelse(is.na(data$SousFamilleCodeSandre), NA, data$SousFamilleCodeSandre), # on regarde si SousFamilleCodeSandre vide, si vide alors NA, sinon SousFamilleCodeSandre ajouté
                                 data$CodeSANDRE) # Si CodeSANDRE pas vide, alors on remet CodeSANDRE

  # Application pour les FAMILLES #
  data$Taxon <- ifelse(is.na(data$Taxon), # Si taxon vide
                            ifelse(is.na(data$Famille), NA, data$Famille), # on regarde si famille vide, si vide alors NA, sinon famille ajouté
                            data$Taxon) # Si taxon pas vide, alors on remet taxon
  data$CodeSANDRE <- ifelse(is.na(data$CodeSANDRE), # Si CodeSANDRE vide
                                 ifelse(is.na(data$FamilleCodeSandre), NA, data$FamilleCodeSandre), # on regarde si FamilleCodeSandre vide, si vide alors NA, sinon FamilleCodeSandre ajouté
                                 data$CodeSANDRE) # Si CodeSANDRE pas vide, alors on remet CodeSANDRE
  
  # Application pour les ORDRES #
  data$Taxon <- ifelse(is.na(data$Taxon), # Si taxon vide
                            ifelse(is.na(data$Ordre), NA, data$Ordre), # on regarde si ordre vide, si vide alors NA, sinon ordre ajouté
                            data$Taxon) # Si taxon pas vide, alors on remet taxon
  data$CodeSANDRE <- ifelse(is.na(data$CodeSANDRE), # Si CodeSANDRE vide
                                 ifelse(is.na(data$OrdreCodeSandre), NA, data$OrdreCodeSandre), # on regarde si OrdreCodeSandre vide, si vide alors NA, sinon OrdreCodeSandre ajouté
                                 data$CodeSANDRE) # Si CodeSANDRE pas vide, alors on remet CodeSANDRE
  
  return(data)
  
} # Fin de la fonction