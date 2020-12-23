#' Représentation de résultats de PC
#'
#' Cette fonction permet de représenter des pcmes_valeurs physico-chimiques
#' @name PC.figure
#' @param PC Jeu de données issu de la BDD PC
#' @import tidyverse
#' @export
#' @examples
#' PC.figure(PC)
#' PC %>% PC.figure()
#' spNitrates <- ChimieOrg %>% filter(ParametreNom == Parametre[3]) %>% PC.figure()

##### TODO LIST #####
# 
#####################

PC.figure <- function(
  PC
  )
{

#### Contexte ####
  if("ClasseQualite" %in% names(PC) == FALSE){
    PC <- PC.classes(PC, Referentiel="NV")
    warning("Les classes de qualité n'était pas calculées : le référentiel Nisbet/Verneaux a été utilisé")
  }
  
  if("pcmes_coderhj" %in% colnames(PC)) Contexte <- tibble(nStations = n_distinct(PC$pcmes_coderhj))
  if("pcmes_coderhj" %in% colnames(PC) == FALSE) Contexte <- tibble(nStations = 1)
  
  if(testit::has_error(PC %>% 
                       distinct(ParametreSANDRE) %>% 
                       bind_cols(Contexte)) == TRUE) stop("Plusieurs codes SANDRE de paramètres au sein du jeu de données")
  
  Contexte <- 
    PC %>% 
    distinct(ParametreSANDRE) %>% 
    bind_cols(Contexte)
  
  if(testit::has_error(PC %>% 
                       distinct(ParametreNom) %>% 
                       bind_cols(Contexte)) == TRUE) stop("Plusieurs noms de paramètres au sein du jeu de données alors qu'il n'y a qu'un code paramètre SANDRE")
  
  Contexte <- 
    PC %>% 
    distinct(ParametreNom) %>% 
    bind_cols(Contexte)
  
  Contexte <- PC.parametres(Contexte)
  
  PC <-
    PC %>% 
    mutate(pcmes_date = ymd(pcmes_date))

  Contexte <- 
    Contexte %>% 
    add_column(Duree = as.numeric(max(PC$pcmes_date) - min(PC$pcmes_date)))
  
#### Représentation ####
Figure <- ggplot(PC, aes(x = pcmes_date, y = pcmes_valeur, shape = pcmes_coderhj, color = ClasseQualite)) + geom_point()
Figure <- Figure + ylim(0, max(PC$pcmes_valeur))
Figure <- Figure + scale_colour_manual(values = c("Classe 1" = "#0061ff", "Classe 2" = "#00ff1d", "Classe 3" = "#c7ffb7", "Classe 4" = "#fff600", "Classe 5" = "#ffae00", "Classe 6" = "#ff0000", "Classe 7" = "#e500ff", "Gris clair" = "#e5e3e5", "< seuil quantification" = "#8e8d8e", "Noir" = "#000000"))
#Figure <- Figure + scale_colour_manual(values = c("Bleu" = "#0061ff", "Vert" = "#00ff1d", "Vert clair" = "#c7ffb7", "Jaune" = "#fff600", "Orange" = "#ffae00", "Rouge" = "#ff0000", "Violet" = "#e500ff", "Gris clair" = "#e5e3e5", "Gris" = "#8e8d8e", "Noir" = "#000000"))
if(Contexte$Duree < 1000) Figure <- Figure + scale_x_pcmes_date(pcmes_date_breaks = "2 month", pcmes_date_labels = "%m/%Y")
if(Contexte$Duree > 1000 & Contexte$Duree < 2000) Figure <- Figure + scale_x_pcmes_date(pcmes_date_breaks = "6 month", pcmes_date_labels = "%m/%Y")
if(Contexte$Duree > 2000) Figure <- Figure + scale_x_pcmes_date(pcmes_date_breaks = "1 year", pcmes_date_labels = "%Y")
Figure <- Figure + ggtitle(Contexte$ParametreNom) + xlab("pcmes_date") + ylab(paste0("Concentration (", Contexte$UniteNom, ")"))
Figure

#### Sortie ####
return(Figure)

} # Fin de la fonction
