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
# Contexte$nom <- "CTR1-0";Date <- "2019-10-02"
# Captures <- poissons.captures(Contexte$nom, Date) #%>% 
# filter(codeespece == "TRF")
# 
# Contexte$nom <- "BIE0-1"
# Captures <- poissons.captures(Contexte$nom)
#####################

poissons.histogramme <- function(
  Captures = Captures,
  Titre = "",
  format = ".pdf",
  export = F
)
{

  #### Palette de couleurs ####
  data(PalettePoissons) # Pour charger la liste complète des espèces
  
  #### Contexte ####
  Contexte <- Captures %>% select(nom) %>% distinct() %>% ungroup()
  Contexte$nbannees <- Captures %>% select(datedebut) %>% mutate(Annee = year(datedebut)) %>% n_distinct()
  if(Contexte$nbannees == 1) Contexte <- Captures %>% select(datedebut) %>% mutate(Annee = year(datedebut)) %>% distinct() %>% ungroup() %>% bind_cols(Contexte)
  Contexte$nbespeces <- Captures %>% select(codeespece) %>% n_distinct()
  if(Contexte$nbespeces == 1) Contexte <- Captures %>% select(codeespece) %>% distinct() %>% ungroup() %>% bind_cols(Contexte)

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
  MdMsMeCs <- MdMsMeCs + labs(y = "Nombre d'individus", x = "Taille moyenne (mm)",title = paste0(Contexte$nom," - ",Contexte$Annee, " - ", Contexte$codeespece )) 
  MdMsMeCs <- MdMsMeCs + theme_linedraw() 
  MdMsMeCs <- MdMsMeCs + theme(legend.position='none')
  MdMsMeCs <- MdMsMeCs + theme(strip.text = element_text(size = rel(1)))
  MdMsMeCs <-  MdMsMeCs   + theme (axis.text.x = element_text(face="bold", size=8))
  MdMsMeCs
  # Exportation/sortie #
  if(export==T){ggsave(paste0("./", Contexte$nom,"/PoissonsHistogramme/Annee/SansClasse/","PoissonsHistogramme-SansClasse-", Contexte$nom, "_", Contexte$Annee, "_", Contexte$codeespece, format , sep=""))}
  if(export==F){return(MdMsMeCs)}
 }
  
 if(count(distinct(Captures, nom, datedebut, codeespece)) != 1){
  warning("Attention il y a plusieurs couples nom, datedebut, codeespece dans le jeu de données : représentations individuelles impossibles")
  }
  
  #### Traitement mono-date mono-Contexte$nom ####
  if(nrow(distinct(Captures, nom, datedebut)) == 1){
    
  ### Mono-date mono-Contexte$nom multi-espece classe avec ###
  MdMsMUeCa <- ggplot(Captures, aes(x = classetaille, y=nombre, fill = codeespece)) + geom_bar(stat="identity", position=position_dodge())
  MdMsMUeCa <- MdMsMUeCa + scale_fill_manual(values = PalettePoissons)
  MdMsMUeCa <- MdMsMUeCa + labs(y = "Nombre d'individus", x = "Taille moyenne (mm)", title = paste0(Contexte$nom," - ",Contexte$Annee, " - ", Contexte$codeespece)) 
  MdMsMUeCa <- MdMsMUeCa + labs(fill = "Espèce")
  MdMsMUeCa <- MdMsMUeCa + theme_linedraw() 
  MdMsMeCs <- MdMsMeCs + theme(legend.position='none')
  MdMsMUeCa <- MdMsMUeCa + theme(strip.text = element_text(size = rel(1)))
  MdMsMUeCa <- MdMsMUeCa  + theme (axis.text.x = element_text(face="bold", size=8, angle=45))
  MdMsMUeCa
  # Exportation/sortie #
  if(export==T){ggsave(paste0("./", Contexte$nom,"/PoissonsHistogramme/Annee/Classe/","PoissonsHistogramme-Classe-", Contexte$nom, "_", Contexte$Annee, "_", Contexte$codeespece, format , sep=""))}
  if(export==F){return(MdMsMUeCa)}
  }
  
  if(nrow(distinct(Captures, nom, datedebut)) != 1){
    warning("Attention il y a plusieurs couples nom, datedebut, dans le jeu de données : représentations mono-date mono-Contexte$nom impossibles")
    ### Multi-dates mono-station multi-espece classe avec ###
    MUdMsMUeCa <- ggplot(Captures, aes(x = classetaille, y=nombre, fill = codeespece)) + geom_bar(stat="identity", position=position_dodge())
    MUdMsMUeCa <- MUdMsMUeCa + scale_fill_manual(values = PalettePoissons)
    MUdMsMUeCa <- MUdMsMUeCa + labs(y = "Nombre d'individus", x = "Taille moyenne (mm)", title = paste0(Contexte$nom," - ",Contexte$Annee))
    MUdMsMUeCa <- MUdMsMUeCa + labs(fill = "Espèce")
    MUdMsMUeCa <- MUdMsMUeCa + theme_linedraw() 
    MUdMsMUeCa <- MUdMsMUeCa + theme(strip.text = element_text(size = rel(1)))
    MUdMsMUeCa <- MUdMsMUeCa + facet_wrap(datedebut~.,scales='fixed')
    MUdMsMUeCa  <- MUdMsMUeCa  + theme (axis.text.x = element_text(face="bold", size=5, angle=45))
    MUdMsMUeCa  <- MUdMsMUeCa + theme(legend.position='none')
    MUdMsMUeCa

    # Exportation/sortie #
    if(export==T){ggsave(paste0("./",Contexte$nom,"/PoissonsHistogramme/Interannuelle/","PoissonsHistogramme-Interannuelle-",Contexte$nom,"_",Contexte$codeespece,format , sep=""))}
    if(export==F){return(MUdMsMUeCa)}
  }

} # Fin de la fonction

