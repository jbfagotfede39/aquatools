#' Regroupement de données temporelles
#'
#' Permet de mettre bout à bout des données temporelles
#' @name chronique.regroupement
#' @keywords donnees
#' @import dplyr
#' @export
#' @examples
#' synthese <- chronique.regroupement()

##### -------------- A FAIRE -------------- #####

# Ça ne fonctionne pas lorsqu'on veut recoler différents fichiers qui sont sur une même année
# Il semble également persister des NA, un filtre (dplyr) ne suffit peut être ?

##### -------------- A FAIRE -------------- #####


chronique.regroupement<-function(){
  
#### Lecture des fichiers ####
# Listing des fichiers d'entrée à traiter
entrees.csv <- list.files(path="./Regroupement", pattern = (".csv$")) # pour aller dans le sous-répertoire sélectionné
entrees <- substr(entrees.csv,1,nchar(entrees.csv)-4) # enlève les extensions .csv ou .txt des fichiers pour ne récupérer que le titre
entrees <- as.vector(entrees)

# Création à la main du premier morceau
synthese <- read.delim2(paste("Regroupement/",entrees.csv[1],sep=""),header=TRUE,sep=";")
synthese$Temp <- as.numeric( sub(",", ".", synthese$Temp)) # Pour remplacer les . par des , pour ne plus être en facteur
#colnames(synthese) <- c("Date","Heure","Temp")
#synthese$Date <- paste(synthese$Date,synthese$Heure,sep="/")
synthese$Date <- as.POSIXct(synthese$Date,format="%Y-%m-%d") #synthese$Date <- as.POSIXct(synthese$Date,format="%d/%m/%Y/%H:%M:%S")
synthese <- select(synthese,Date,Heure,Temp)
synthese <- synthese %>% 
filter(complete.cases(synthese) == TRUE)
#resume(synthese)

# Lecture de chaque fichier et fusion
  for(i in 1:length(entrees.csv)){
    temporaire <- read.delim2(paste("Regroupement/",entrees.csv[i],sep=""),header=TRUE,sep=";")
    temporaire$Temp <- as.numeric( sub(",", ".", temporaire$Temp)) # Pour remplacer les . par des , pour ne plus être en facteur
    #temporaire$Date <- paste(temporaire$Date,temporaire$Heure,sep="/")
    temporaire$Date <- as.POSIXct(temporaire$Date,format="%Y-%m-%d") #temporaire$Date <- as.POSIXct(temporaire$Date,format="%d/%m/%Y/%H:%M:%S")
    temporaire <- select(temporaire,Date,Heure,Temp)
    synthese <- merge(synthese,temporaire, all=T)
    synthese <- synthese %>% 
      filter(complete.cases(synthese) == TRUE)
  }

synthese <- rename(synthese, Temperature = Temp)
# write.table(synthese,file= paste("Regroupement/Regroupé_",entrees[1],".csv", sep= ""),col.names=T, sep= ";",row.names=F)
return(synthese)

}