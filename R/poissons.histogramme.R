#' Affichage d'histogrammes taille-effectif
#'
#' Cette fonction permet de réaliser des histogrammes taille-effectif de poissons
#' @name poissons.histogramme
#' @param Captures Dataframe contenant les captures
#' @keywords poissons
#' @import dplyr
#' @import lubridate
#' @export
#' @examples
#' poissons.histogramme(Captures)

##### TODO LIST #####
# Pour essai :
# Station <- "CTR1-0";Date <- "2019-10-02"
# Captures <- poissons.captures(Station, Date) #%>% 
# filter(codeespece == "TRF")
# 
# Station <- "BIE0-1"
# Captures <- poissons.captures(Station)
#####################

poissons.histogramme <- function(
  Captures = Captures,
  save = F
)
{

  #### Palette de couleurs ####
  data(PalettePoissons) # Pour charger la liste complète des espèces
  
  #### Calcul des classes de taille ####
  if("classetaille" %in% colnames(Captures) == FALSE){
    Captures <- poissons.classes(Captures)
  }
  
  #### Dénombrement par classes de taille ####
  CapturesV2 <-
    Captures %>% 
    group_by(nom, datedebut, codeespece, classetaille) %>% 
    summarise(nombre = sum(nombre)) %>% 
    ungroup()
  
  #### Mono-date mono-station mono-espece classe sans ####
if(nrow(distinct(Captures, nom, datedebut, codeespece)) == 1){
  MdMsMeCs <- ggplot(Captures, aes(x=taillemoy)) + geom_bar(aes(weight = nombre), width=1.5, fill="steelblue")
  MdMsMeCs <- MdMsMeCs + labs(y = "Nombre d'individus", x = "Taille moyenne (mm)",title = Station ) 
  MdMsMeCs <- MdMsMeCs + theme_linedraw() 
  MdMsMeCs <- MdMsMeCs + theme(strip.text = element_text(size = rel(1)))
  MdMsMeCs
  # Exportation/sortie #
  if(save==T){ggsave(paste0("./HistoTaille/",gsub("[[:punct:]]", "-", Station), ".png"))}
  if(save==F){return(MdMsMeCs)}
  }
  
  if(count(distinct(Captures, nom, datedebut, codeespece)) != 1){
  warning("Attention il y a plusieurs couples nom, datedebut, codeespece dans le jeu de données : représentations individuelles impossibles")
  }
  
  #### Traitement mono-date mono-station ####
  if(nrow(distinct(Captures, nom, datedebut)) == 1){
    
  ### Mono-date mono-station multi-espece classe avec ###
  MdMsMUeCa <- ggplot(Captures, aes(x = classetaille, y=nombre, fill = codeespece)) + geom_bar(stat="identity", position=position_dodge())
  MdMsMUeCa <- MdMsMUeCa + scale_fill_manual(values = PalettePoissons)
  MdMsMUeCa <- MdMsMUeCa + labs(y = "Nombre d'individus", x = "Taille moyenne (mm)", title = Station) 
  MdMsMUeCa <- MdMsMUeCa + labs(fill = "Espèce")
  MdMsMUeCa <- MdMsMUeCa + theme_linedraw() 
  MdMsMUeCa <- MdMsMUeCa + theme(strip.text = element_text(size = rel(1)))
  MdMsMUeCa
  # Exportation/sortie #
  if(save==T){ggsave(paste0("./HistoTaille/",gsub("[[:punct:]]", "-", Station), ".png"))}
  if(save==F){return(MdMsMUeCa)}
  }
  
  if(nrow(distinct(Captures, nom, datedebut)) != 1){
    warning("Attention il y a plusieurs couples nom, datedebut, dans le jeu de données : représentations mono-date mono-station impossibles")
    ### Multi-dates mono-station multi-espece classe avec ###
    MUdMsMUeCa <- ggplot(Captures, aes(x = classetaille, y=nombre, fill = codeespece)) + geom_bar(stat="identity", position=position_dodge())
    MUdMsMUeCa <- MUdMsMUeCa + scale_fill_manual(values = PalettePoissons)
    MUdMsMUeCa <- MUdMsMUeCa + labs(y = "Nombre d'individus", x = "Taille moyenne (mm)", title = Station) 
    MUdMsMUeCa <- MUdMsMUeCa + labs(fill = "Espèce")
    MUdMsMUeCa <- MUdMsMUeCa + theme_linedraw() 
    MUdMsMUeCa <- MUdMsMUeCa + theme(strip.text = element_text(size = rel(1)))
    MUdMsMUeCa <- MUdMsMUeCa + facet_wrap(datedebut~.,scales='fixed')
    MUdMsMUeCa
    
    # Exportation/sortie #
    if(save==T){ggsave(paste0("./HistoTaille/",gsub("[[:punct:]]", "-", Station), ".png"))}
    if(save==F){return(MUdMsMUeCa)}
  }

} # Fin de la fonction