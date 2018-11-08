#' Division en séries annuelles
#'
#' Permet de diviser une longue chronique temporelle en séries annuelles à partir du 1 octobre de l'année N-1 jusqu'au 30 septembre de l'année N
#' @name division.annuelle
#' @keywords donnees
#' @import dplyr
#' @export
#' @examples
#' division.annuelle()

division.annuelle<-function(){

  #### Lecture des fichiers ####
  ## Listing des fichiers d'entrée à traiter
  entrees.csv <- list.files(path="./Regroupement", pattern = (".csv$")) # pour aller dans le sous-répertoire sélectionné
  entrees <- substr(entrees.csv,1,nchar(entrees.csv)-4) # enlève les extensions .csv des fichiers pour ne récupérer que le titre
  entrees <- as.vector(entrees)
  
  for(z in 1:length(entrees)){
    
    ## Création à la main du premier morceau
    synthese <- read.delim2(paste("Regroupement/",entrees.csv[z],sep=""),header=TRUE,sep=";")
    colnames(synthese) <- c("Date","Heure","Temp")
    synthese$Temp <- as.numeric( sub(",", ".", synthese$Temp)) # Pour remplacer les . par des , pour ne plus être en facteur
    #synthese$Date<-paste(synthese$Date,synthese$Heure,sep="/") Ces deux lignes posent parfois problème en faisant apparaître des NA
    #synthese$Date<-as.POSIXct(synthese$Date,format="%d/%m/%Y/%H:%M:%S")
    
    synthese$Date <- as.POSIXct(synthese$Date,format="%Y-%m-%d")
    #synthese<-select(synthese,Datesimple,Heure,Temp,Date)
    #str(synthese)
    
    #### Séparation en sous-fichiers annuels ####
    ## Dénombrer le nombre de 01/10
    liste01<-unique(format(subset(synthese, format(Date,'%d')=='01' & format(Date,'%m')=='10')$Date,"%Y-%m-%d")) #Parfait
    liste01<-as.POSIXct(liste01,format="%Y-%m-%d")
    
    ## Création des différents sous data.frames annuels et d'une liste des noms de ces data.frames
    listeannee <- character(length(liste01)+1) # initialisation de la liste des différents sous-fichiers
    for(i in 2:length(liste01)){
      assign(paste(entrees[z],"-a",format(liste01[1],'%Y'),sep=""), subset(synthese, Date < liste01[1])) # Pour créer un nom dynamique type "a2011" en fct des années dans la liste et lui attribuer les valeurs du subset de la première année
      assign(paste(entrees[z],"-a",format(liste01[i],'%Y'),sep=""), subset(synthese, Date < liste01[i] & Date >= liste01[i-1])) # Pour créer un nom dynamique type "a2011" en fct et lui attribuer les valeurs du subset des années 2 à i dans la liste
      assign(paste(entrees[z],"-a",as.numeric(format(liste01[i],'%Y'))+1,sep=""), subset(synthese, Date >= liste01[i])) # Pour créer un nom dynamique type "a2011" en fct des années dans la liste et lui attribuer les valeurs du subset de l'année i+1
      listeannee[1]<-paste(entrees[z],"-a",format(liste01[1],'%Y'),sep="") # pour mettre la première année
      listeannee[i]<-paste(entrees[z],"-a",format(liste01[i],'%Y'),sep="") # pour mettre les années suivantes, mais il manque la dernière
    }
    
    assign(paste(entrees[z],"-a",format(liste01[1],'%Y'),sep=""), subset(synthese, Date < liste01[1])) # Pour créer un nom dynamique type "a2011" en fct des années dans la liste et lui attribuer les valeurs du subset de la première année
    
    if(exists(as.character(paste(entrees[z],"-a",as.numeric(format(liste01[i],'%Y'))+1,sep=""))) == TRUE) {
      listeannee[length(listeannee)] <- paste(entrees[z],"-a",as.numeric(format(liste01[i],'%Y'))+1,sep="")} # Pour ajouter à la liste la dernière année
    
    #### Écriture des fichiers en .txt
    for(i in 1:length(listeannee)){
      write.table(get(listeannee[i]),file=paste("Sorties/Brutsannuels/",listeannee[i],".csv", sep= ""),sep= ";",row.names=F,col.names=T)
    }
    
  }

} # Fin de la fonction