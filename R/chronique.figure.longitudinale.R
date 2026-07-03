#' Profils en long de chroniques
#'
#' Cette fonction permet de représenter sous forme de profil longitudinal des données de chroniques
#' @name chronique.figure.longitudinale
#' @param data Dataframe issu de la fonction chronique.resultats ou de la base de données Chroniques, avec jointure de \code{chsta_milieu} et de \code{chsta_distancesource}
#' @param titre Titre du graphique (vide par défaut)
#' @param origine_donnees Éventuelle source des données à afficher sur la figure (vide par défaut)
#' @param format Définit le format d'enregistrement (par défaut .png)
#' @param save Si \code{FALSE} (par défault), n'enregistre pas les figures. Si \code{TRUE}, les enregistre.
#' @param projet Nom du projet
#' @keywords chronique
#' @import glue
#' @import RColorBrewer
#' @import tidyverse
#' @export
#' @examples
#' Resultats %>% chronique.figure.longitudinale()
#' Resultats %>% chronique.resultats.filtrage() %>% chronique.figure.longitudinale()
#' Resultats %>% left_join(Stations %>% select(chsta_coderhj, chsta_milieu, chsta_distancesource, chsta_distancesource_confluencedrainprincipal), by = c("chres_coderhj" = "chsta_coderhj")) %>% chronique.resultats.filtrage() %>% chronique.figure.longitudinale()

chronique.figure.longitudinale <- function(
  data = data,
  titre = "",
  origine_donnees = "",
  save = FALSE,
  projet = NA_character_,
  format = ".png"
)
{
  
  #### Évaluation des choix ####
  #Recherche <- match.arg(Recherche)
  
  #### Renommage des variables ####
  # Stations
  data <-
    data %>% 
    chronique.variables.renommage(formatentree = "Vide", formatsortie = "chres") |> 
    mutate(chsta_distancesource = as.numeric(chsta_distancesource))
  
  #### Contexte des données ####
  contexte <- chronique.contexte(data)
  
  #### Tests ####
  if(contexte$ntypemesure != 1) stop("Plusieurs chmes_typemesure au sein du jeu de données")
  if(contexte$nstation == 0 | contexte$nannee == 0) stop("Aucune donnée dans la chronique à analyser")
  if(contexte$nstation <= 2) stop(glue("Seulement {contexte$nstation} station(s) au sein du jeu de données"))
  if("chsta_milieu" %in% names(data) == FALSE) stop("Pas de champs chsta_milieu dans le jeu de données en entrée")
  if("chsta_distancesource" %in% names(data) == FALSE) stop("Pas de champs chsta_distancesource dans le jeu de données en entrée")
  if(contexte$nmilieu != 1 & ("chsta_distancesource_confluencedrainprincipal" %in% names(data) == FALSE)) stop("Pas de champs chsta_distancesource_confluencedrainprincipal dans le jeu de données en entrée")
  
  #### Calcul des distances à la source homogènes ####
  if(contexte$nmilieu != 1){
    # ecosystemeppal <- tcltk::tk_select.list(sort(unlist(strsplit(contexte$milieu, ";"))), multiple = F, title = "Écosystème principal")
    stop("Commande à remplacer pour supprimer la dépendance à tcltk::tk_select.list")
  }
  
  data <-
    data %>% 
    {if(contexte$nmilieu == 1) mutate(., distancesourcesynthetique = chsta_distancesource) else .} %>% 
    {if(contexte$nmilieu != 1) mutate(., distancesourcesynthetique = ifelse(chsta_milieu == ecosystemeppal, chsta_distancesource, chsta_distancesource_confluencedrainprincipal)) else .}
  
  #### Paramètres de légende ####
  parametres <- contexte %>% chronique.figure.parametres(typefigure = "vmm30j")
  legendeY <- parametres$legendeY
  legendeTitre <- parametres$legendeTitre
  typemesureTitreSortie <- parametres$typemesureTitreSortie
  
  if(nchar(titre) == 0 & contexte$nmilieu == 1) titre <- contexte$milieu
  if(nchar(titre) == 0 & contexte$nmilieu != 1) titre <- ecosystemeppal

  #### Calcul d'une clé par écosystème/année ####
  data <-
    data %>%
    chronique.cle(formatcle = "MA")
  
  ## Palette de couleurs ##
  if(contexte$nannee != 1 | contexte$nstation != 1){
    # data(PaletteAnnees) # Couleurs trop proches pour années successives
    if(contexte$nannee <= 11){PaletteCouples <- RColorBrewer::brewer.pal(contexte$nannee, "Spectral")} #Set3
    if(contexte$nannee > 11){
      colourCount <- contexte$nannee
      getPalette <- colorRampPalette(RColorBrewer::brewer.pal(9, "Spectral")) #Set3
      PaletteCouples <- getPalette(colourCount)
    }
  }
    
  #### Représentation graphique
  if(contexte$nmilieu == 1) gg <- ggplot(data, aes(x = chsta_distancesource, y = chres_vmaxmoy30j, colour = as.character(chres_anneevmm)))
  if(contexte$nmilieu != 1) gg <- ggplot(data, aes(x = chsta_distancesource, y = chres_vmaxmoy30j, colour = str_replace(Cle, "_", " - ")))
  gg <- gg + geom_line()
  gg <- gg + xlim(0, max(data$chsta_distancesource))
  gg <- gg + ylim(0, 29)
  if(contexte$nmilieu == 1) gg <- gg + labs(x = "Distance à la source (km)", y = expression(Tmm30j~(degree*C)), colour = "Année biologique") # Pour changer le titre
  if(contexte$nmilieu != 1) gg <- gg + labs(x = "Distance à la source (km)", y = expression(Tmm30j~(degree*C)), colour = "Milieu - Année biologique") # Pour changer le titre
  # if(contexte$nannee != 1 | contexte$nstation != 1) gg <- gg + scale_colour_manual(values = PaletteCouples)
  gg <- gg + theme_minimal()
  gg <- gg + labs(title = titre)
  if(nchar(origine_donnees) != 0) gg <- gg + labs(caption = glue("Source des données : {origine_donnees}"))
  gg <- gg + theme(axis.title.x = element_text(size=10),
                   axis.title.y = element_text(size=10)
  )
  gg
  
  #### Affichage des résultats ####
  if(save==T){
    ggsave(file = glue("{projet}/Sorties/Vues/Intersites/Intersites{typemesureTitreSortie}{titre}{format}"))

  }
  if(save==F){return(gg)}
} # Fin de la fonction
