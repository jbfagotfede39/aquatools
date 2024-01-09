#' Représentation de résultats de PC
#'
#' Cette fonction permet de représenter des valeurs physico-chimiques
#' @name PC.figure
#' @param data Jeu de données de physico-chimie issu de \code{PC.mesures} ou \code{PC.hubeau}
#' @param theme Thème ggplot2 à appliquer à la figure \code{theme_minimal} (par défaut) ou \code{theme_bw}
#' @import glue
#' @import tidyverse
#' @export
#' @examples
#' PC.figure(bdd_pc)
#' bdd_pc %>% PC.figure()
#' vue_nitrates <- bdd_pc %>% filter(pcmes_parametrenom == "Nitrates") %>% PC.figure()
#' vue_nitrates <- bdd_pc %>% filter(pcmes_parametresandre == 1340) %>% PC.figure()

PC.figure <- function(
  data,
  theme = c("theme_minimal", "theme_bw")
  )
{
  #### Évaluation des choix ####
  theme <- match.arg(theme)
  
  #### Classes de qualité si non existantes ####
  if("ClasseQualite" %in% names(data) == FALSE){
    data <- PC.classes(data, Referentiel = "NV")
    warning("Les classes de qualité n'était pas calculées : le référentiel Nisbet/Verneaux a été utilisé")
  }
  
  #### Contexte ####
  contexte <- PC.contexte(data)
  parametres <- contexte %>% PC.figure.parametres()
  legende_y <- parametres$legende_y
  legende_titre <- parametres$legende_titre
  typemesure_titre_sortie <- parametres$typemesure_titre_sortie
  
  # nStations #
  if("pcmes_coderhj" %in% colnames(data)) contexte_1 <- tibble(nStations = n_distinct(data$pcmes_coderhj))
  if("pcmes_coderhj" %in% colnames(data) == FALSE) contexte_1 <- tibble(nStations = 1)
  
  # pcmes_parametresandre #
  if(contexte$n_typemesure != 1) stop("Plusieurs codes SANDRE de paramètres au sein du jeu de données")

  # pcmes_unitenom #
  if(contexte$n_unitenom != 1) stop("Plusieurs noms d'unités au sein du jeu de données")

  # Complément #
  data_3 <-
    data %>% 
    mutate(pcmes_date = ymd(pcmes_date))

  contexte_2 <- 
    contexte %>% 
    add_column(duree = as.numeric(max(data_3$pcmes_date) - min(data_3$pcmes_date)))
  
#### Représentation ####
figure <- ggplot(data_3, aes(x = pcmes_date, y = pcmes_valeur, shape = pcmes_coderhj, colour = ClasseQualite)) + geom_point()
figure <- figure + ylim(0, max(data_3$pcmes_valeur))
figure <- figure + scale_colour_manual(values = c("Classe 1" = "#0061ff", "Classe 2" = "#00ff1d", "Classe 3" = "#c7ffb7", "Classe 4" = "#fff600", "Classe 5" = "#ffae00", "Classe 6" = "#ff0000", "Classe 7" = "#e500ff", "Gris clair" = "#e5e3e5", "< seuil quantification" = "#8e8d8e", "Noir" = "#000000"))
#figure <- figure + scale_colour_manual(values = c("Bleu" = "#0061ff", "Vert" = "#00ff1d", "Vert clair" = "#c7ffb7", "Jaune" = "#fff600", "Orange" = "#ffae00", "Rouge" = "#ff0000", "Violet" = "#e500ff", "Gris clair" = "#e5e3e5", "Gris" = "#8e8d8e", "Noir" = "#000000"))
if(contexte_2$duree < 1000) figure <- figure + scale_x_date(date_breaks = "2 month", date_labels = "%m/%Y")
if(contexte_2$duree > 1000 & contexte_2$duree < 2000) figure <- figure + scale_x_date(date_breaks = "6 month", date_labels = "%m/%Y")
if(contexte_2$duree > 2000) figure <- figure + scale_x_date(date_breaks = "1 year", date_labels = "%Y")
figure <- figure + ggtitle(legende_titre) + xlab("Date") + ylab(legende_y)
figure <- figure + labs(colour = "Classe de qualité")
figure <- figure + labs(shape = "Station")
if(theme == "theme_minimal") figure <- figure + theme_minimal()
figure <- figure + theme(axis.title.x = element_blank()) # Suppression du titre de l'axe x
figure <- figure + guides(colour = "none")
if(contexte_2$n_station == 1) figure <- figure + guides(shape = "none")
figure

#### Sortie ####
return(figure)

} # Fin de la fonction
