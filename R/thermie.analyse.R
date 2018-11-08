#' Calcul de paramètres clés sur des chroniques de température
#'
#' Cette fonction permet de calculer des paramètres clés sur des chroniques de température
#' @name thermie.analyse
#' @param CodeRDT Nom de la station
#' @param Annee Année du suivi
#' @param jour Colonne du data.frame contenant les dates
#' @param temp Colonne du data.frame contenant les températures
#' @param pasdetemps Indique le pas de temps entre chaque mesure : 1 pour une heure
#' @keywords température
#' @export
#' @examples
#' thermie.analyse("DRO14-8","2013",jour=tab[,1],temp=tab[,3],tempmin=4,tempmax=19,tempmaxextreme=25,pasdetemps=1)

thermie.analyse <- function(CodeRDT="",
                            Annee="",
                            jour=tab[,1],
                            temp=tab[,3],
                            tempmin=4,
                            tempmax=19,
                            tempmaxextreme=25,
                            pasdetemps=1)
  {

## Chargement des données pour travaux ##
#CodeRDT=StationsSMISA[i,2];Annee="2015";jour=DataTravail[,4];temp=DataTravail[,6];tempmin=4;tempmax=19;tempmaxextreme=25;pasdetemps=1
#thermie<-read.delim2("./Entrees/FUR0-4_10316500_FUR1.csv-a2014.txt",header=TRUE,sep=";")
#thermie<-read.delim2("./Entrees/saineamontconflemme2011_lacunaire.csv",header=TRUE,sep=";")
#thermie<-read.delim2("./Entrees/SAI18-5_été2012.csv",header=TRUE,sep=";")
#thermie <- read.delim2("./Données-test/ANG0-7_10316505_ANG1-a2014.txt",header=TRUE,sep=";")
#thermie$V3<- as.numeric( sub(",", ".", thermie$V3))
#thermie$Temp<- as.numeric( sub(",", ".", thermie$Temp))
#jour=thermie[,1];CodeRDT="saineamontconflemme2011";temp=thermie[,3];nbj=30;tempmin=4;tempmax=19;tempmaxextreme=25;pasdetemps=1;maladie=15;tempminextreme=1;save=T;format="png"
#library(aquatools)
#resume(thermie)

########## Vérification et transformation des données ##########
## Transformation du format des dates
jour<-as.Date(jour,format="%Y-%m-%d") # "%d/%m/%Y"
jourfac<-as.factor(jour)
jourautre<-strptime(jour, "%Y-%m-%d")

## Date début période
DateDPeriode<-levels(jourfac)[1]

## Date fin période
DateFPeriode<-levels(jourfac)[length(levels(jourfac))]

## Nb Total de Jours
NbJ<-length(levels(jourfac))

# Vérification de la cohérence avec le nb de données attendues par jour
NbTJattendues<-24/pasdetemps
JOK<-numeric(length(levels(jourfac)))
JpasOK<-numeric(length(levels(jourfac)))
for (i in 1:length(levels(jourfac))){
  if (length(temp[which(jourfac==levels(jourfac)[i])])==NbTJattendues) JOK[i]<-1 else JpasOK[i]<-1
  }

# Totaux des jours OK et pas OK
NbJOK<-sum(JOK)
NbJpasOK<-sum(JpasOK)

# Affichage de de la date des jours pas OK
#detailJpasOK<-levels(jourfac)[which(JpasOK==1)]  # fonctionne bien mais pose pb pour l'affichage, car cr?? une ligne dans le data.frame pour chaque date fausse

########## Statistiques descriptives ##########

##### Statistiques par jour #####

###T Min J
TMinJ<-numeric(length(levels(jourfac)))
for (i in 1:length(levels(jourfac))){
	TMin<-round(min(temp[which(jourfac==levels(jourfac)[i])]),1)
	TMinJ[i]<-TMin
}

###T Max J
TMaxJ<-numeric(length(levels(jourfac)))
for (i in 1:length(levels(jourfac))){
	TMax<-round(max(temp[which(jourfac==levels(jourfac)[i])]),1)
	TMaxJ[i]<-TMax
}

###T Moy J = °/j
TMoyJ<-numeric(length(levels(jourfac)))
for (i in 1:length(levels(jourfac))){
	TMoy<-round(mean(temp[which(jourfac==levels(jourfac)[i])]),1)
	TMoyJ[i]<-TMoy
}

###T Med J
TMedJ<-numeric(length(levels(jourfac)))
for (i in 1:length(levels(jourfac))){
	TMed<-round(median(temp[which(jourfac==levels(jourfac)[i])]),1)
	TMedJ[i]<-TMed
}

###Variance J 
VarJ<-numeric(length(levels(jourfac)))
for (i in 1:length(levels(jourfac))){
	Var<-round(var(temp[which(jourfac==levels(jourfac)[i])]),2)
	VarJ[i]<-Var
}

###Amplitude J
AmplitudeJ<-numeric(length(levels(jourfac)))
for (i in 1:length(levels(jourfac))){
	AmplitudeJ[i]<-TMaxJ[i]-TMinJ[i]}

### Coefficient de variation des moyennes journalières
CVJ<-sd(TMoyJ)/mean(TMoyJ)

##### Valeurs journalières remarquables #####

### T Moy J Min sur la période
TMoyJMinPer<-round(min(TMoyJ),1)

### Date T Moy J Min sur la période
DateTMoyJMinPer<-levels(jourfac)[which(TMoyJ==TMoyJMinPer)]

### T Moy J Max sur la période
TMoyJMaxPer<-round(max(TMoyJ),1)

###Date T Moy J Max sur la période
DateTMoyJMaxPer<-levels(jourfac)[which(TMoyJ==TMoyJMaxPer)]

###Amplitude Moy J Per
AmplitudeMoyJPer<-round(TMoyJMaxPer-TMoyJMinPer,1)

##### Statistiques par mois #####
### Nb de jours par mois

###T Moy M
# TmoyM<-numeric(length(levels(as.factor(jourautre$mon))))
# for (i in 1:length(levels(as.factor(jourautre$mon)))){
# 	MoyM<-round(mean(temp[which(jourautre$mon==levels(as.factor(jourautre$mon))[i])]),1)
# 	TmoyM[i]<-MoyM
# }
# TmoyM<-as.data.frame(t(TmoyM))
# colnames(TmoyM) <- unique(format(jourautre, format = "%B"))

#TmoyM # OK, calcul bon !! 
# levels(jourautre[which(levels(as.factor(jourautre$mon))=="11")]) # si besoin

##### Statistiques sur l'ensemble du jeu de données #####

TMiniH<-round(min(temp),1)
TMaxiH<-round(max(temp),1)
TMoyPer<-round(mean(temp),1)
TMedPer<-round(median(temp),1)
VarPer<-round(var(temp),1)
AmplitudePerH<-TMaxiH-TMiniH
DegresPer<-round(sum(TMoyJ),0)

##### Valeurs sur périodes mobiles
###T Moymax 7 J
cumuleTMaxJ <- numeric(length(TMaxJ)-7)
for (i in 1:length(TMaxJ)){
	if (i+6<=length(TMaxJ)) cumuleTMaxJ[i]<-sum(TMaxJ[i:(i+6)])}	
TMaxMoy7J <- round(max(cumuleTMaxJ)/7,1)

###T Moymax 30 J
cumuleTMaxJ <- numeric(length(TMaxJ)-30)
for (i in 1:length(TMaxJ)){
	if (i+29<=length(TMaxJ)) cumuleTMaxJ[i]<-sum(TMaxJ[i:(i+29)])}	
TMaxMoy30J<-round(max(cumuleTMaxJ)/30,1)

DateDebutTMaxMoy30J<-levels(jourfac)[which(cumuleTMaxJ==max(cumuleTMaxJ))][1] # Le [1] permet de ne conserver que le premier niveau s'il y a plusieurs dates
DateFinTMaxMoy30J<-levels(jourfac)[which(cumuleTMaxJ==max(cumuleTMaxJ))+29][1]

### Extraction de l'année du TMM ##
AnneeTMM <- substr(levels(jourfac)[which(cumuleTMaxJ==max(cumuleTMaxJ))+30][1], 1, 4)

###T Moymax 50 J
# cumuleTMaxJ<-numeric(length(TMaxJ)-50)
# for (i in 1:length(TMaxJ)){
# 	if (i+49<=length(TMaxJ)) cumuleTMaxJ[i]<-sum(TMaxJ[i:(i+49)])}	
# TMaxMoy50J<-round(max(cumuleTMaxJ)/50,1)

### Affichage généralités temporelles ###
#if (NbJOK==length(levels(jourfac))) detailJpasOK<-"Pas de journée problématique" else detailJpasOK<-summary(detailJpasOK) #ceci à mettre dans affichage
# GeneraliteDates<-c(DateDPeriode, DateFPeriode,NbJ, NbJOK,NbJpasOK,as.factor(detailJpasOK));names(GeneraliteDates)<-c("Date.debut","Date.fin","Nb.total.J","Nb.jours.OK","Nb.jours.pas.OK","Dates.jours.pas.OK")

# GeneraliteDates #revoir affichage detailJpasOK

####mise en forme descriptif série de T

####mise en forme TmoyJ
#recapTJ<-data.frame("date"=as.Date(levels(jourfac)),"Min.T°C"=TMinJ, "Max.T°C"=TMaxJ, "Moy.T°C"=TMoyJ,"Med.T°C"=TMedJ,"Var.T°C"=VarJ,"Ampl"=AmplitudeJ)

# ancienne version au 11/06/13Periode<-data.frame(NbJ,NbJOK,NbJpasOK,DateDPeriode,DateFPeriode,TMiniH,TMaxiH,TMoyPer,TMedPer,VarPer,AmplitudePerH,DegresPer,TMoyJMinPer,DateTMoyJMinPer,TMoyJMaxPer,DateTMoyJMaxPer,AmplitudeMoyJPer,TMaxMoy7J,TMaxMoy30J,TMaxMoy50J) 
# Periode<-data.frame(NbJ,NbJOK,NbJpasOK,TMiniH,TMaxiH,TMoyPer,TMedPer,VarPer,AmplitudePerH,DegresPer,TMoyJMinPer,TMoyJMaxPer,AmplitudeMoyJPer,TMaxMoy7J,TMaxMoy30J,TMaxMoy50J) # nouvelle version au 11/06/13 
Periode <- data.frame(NbJ,NbJOK,NbJpasOK,DateDPeriode,DateFPeriode, CodeRDT, Annee, AnneeTMM, TMiniH,TMaxiH,TMoyPer,TMedPer,VarPer,AmplitudePerH,DegresPer,TMoyJMinPer,TMoyJMaxPer,CVJ,AmplitudeMoyJPer,TMaxMoy7J,TMaxMoy30J,DateDebutTMaxMoy30J,DateFinTMaxMoy30J) 

#data.frame(NbJ,NbJOK,NbJpasOK,DateDPeriode,DateFPeriode,TMiniH,TMaxiH,TMoyPer,TMedPer,VarPer,AmplitudePerH,DegresPer,TMoyJMinPer,TMoyJMaxPer,CVJ,AmplitudeMoyJPer,TMaxMoy7J,TMaxMoy30J,DateDebutTMaxMoy30J,DateFinTMaxMoy30J) 

#write.table(Periode,file=paste("Sorties/Elaboresannuels/",CodeRDT,".txt", sep= ""),col.names=T, sep= ";",row.names=F)
#write.table(TmoyM,file=paste("Sorties/Mensuelles/",CodeRDT,".txt", sep= ""),col.names=T, sep= ";",row.names=F)

#return(Periode)

} # Fin de la fonction