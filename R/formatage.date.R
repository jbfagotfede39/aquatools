#' Reformatage de dates
#'
#' Reformate les dates par lot de fichiers (répertoire /Regroupement/Bruts vers /Regroupement/) en format "2014-10-10"
#' 
#' @param style Indique le style en entrée (dmy par défault)
#' @keywords donnees
#' @import lubridate
#' @export
#' @examples
#' formatage.date()
#' formatage.date(style = "ydm")

formatage.date<-function(
  style = "dmy"
  )
  {

entrees.csv<-list.files(path="./Regroupement/Bruts", pattern = (".csv$")) # pour aller dans le sous-répertoire sélectionné
entrees<-substr(entrees.csv,1,nchar(entrees.csv)-4) # enlève les extensions .csv ou .txt des fichiers pour ne récupérer que le titre
entrees<-as.vector(entrees)
for(i in 1:length(entrees.csv)){
  temporaire <- read.delim2(paste("Regroupement/Bruts/",entrees.csv[i],sep=""),header=TRUE,sep=";")
  if(style == "dmy"){temporaire$Date <- dmy(temporaire$Date)}
  if(style == "ydm"){temporaire$Date <- ydm(temporaire$Date)}
  if(style == "mdy"){temporaire$Date <- mdy(temporaire$Date)}
  write.table(temporaire,file= paste("Regroupement/",entrees.csv[i], sep= ""),col.names=T, sep= ";",row.names=F)
}

} # Fin de la fonction