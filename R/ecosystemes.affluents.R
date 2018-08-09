#' Listage des affluents
#'
#' Cette fonction permet de lister les affluents d'un cours d'eau donné
#' 
#' @param Nom du cours d'eau principal
#' @keywords écosystèmes
#' @import dplyr DBI RSQLite
#' @export
#' @examples
#' ecosystemes.affluents("Ain")

##### TODO LIST #####
# Rajouter le choix de l'écosystème avec les codes (avec recherche dans le champ observation) et de même avec le code de la masse d'eau, avec un interrupteur disant où la fonction doit chercher initialement. Le reste du code reste identique.
#####################

ecosystemes.affluents <- function(
  ecosysteme="Ain")
{
  
  ## Ouverture de la BDD ##
  if(exists("dbP") == FALSE){
    dbP <- BDD.ouverture(Type = "Poissons")
    assign("dbP", dbP, envir = .GlobalEnv)
  }
  
  ## Récupération des données ##
  Ecosystemes <- tbl(dbP,"ecosystemes") %>% collect(n = Inf)
  
  ## Extraction des afférences ##
  # Test si le nom existe bien, sinon message d'erreur et arrêt de la fonction #

if(dim(Ecosystemes %>% filter(nomecosysteme == ecosysteme)
       )[1] == 0) 
  stop("Attention : nom d'écosystème absent de la base de données")
  
EcosystemeRecherche <-
  Ecosystemes %>% 
  filter(nomecosysteme == ecosysteme)

  # Extraction du Codeecosysteme du CE qui nous concerne
#EcosystemeRecherche[,1]

  # Recherche des cours d'eau qui ont un codeaffluent = à ce Codeecosysteme
EcosystemeRechercheV2 <-
  EcosystemeRecherche %>% 
  full_join(Ecosystemes %>% 
  filter(codeaffluent == EcosystemeRecherche[,1])
  )

# 1ère itération pour remonter d'un niveau

EcosystemeRechercheV3 <- EcosystemeRechercheV2
for(i in 1:dim(EcosystemeRechercheV2)[1]){
EcosystemeRechercheV3 <-
  EcosystemeRechercheV3 %>% 
  full_join(Ecosystemes %>% 
              filter(codeaffluent == EcosystemeRechercheV2$Codeecosysteme[i])
  )
}

# 2ème itération pour remonter d'un niveau
EcosystemeRechercheV4 <- EcosystemeRechercheV3
for(i in 1:dim(EcosystemeRechercheV3)[1]){
  EcosystemeRechercheV4 <-
    EcosystemeRechercheV4 %>% 
    full_join(Ecosystemes %>% 
                filter(codeaffluent == EcosystemeRechercheV3$Codeecosysteme[i])
    )
}

# 3ème itération pour remonter d'un niveau
EcosystemeRechercheV5 <- EcosystemeRechercheV4
for(i in 1:dim(EcosystemeRechercheV4)[1]){
  EcosystemeRechercheV5 <-
    EcosystemeRechercheV5 %>% 
    full_join(Ecosystemes %>% 
                filter(codeaffluent == EcosystemeRechercheV4$Codeecosysteme[i])
    )
}

# 4ème itération pour remonter d'un niveau
EcosystemeRechercheV6 <- EcosystemeRechercheV5
for(i in 1:dim(EcosystemeRechercheV5)[1]){
  EcosystemeRechercheV6 <-
    EcosystemeRechercheV6 %>% 
    full_join(Ecosystemes %>% 
                filter(codeaffluent == EcosystemeRechercheV5$Codeecosysteme[i])
    )
}
  
rm(list=setdiff(ls(), "EcosystemeRechercheV6")) # Pour ne conserver que l'objet final
return(EcosystemeRechercheV6)

} # Fin de la fonction