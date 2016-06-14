#' Création de fiches de résultats de pêche
#'
#' Cette fonction permet de créer une fiche de synthèse des résultats de pêche
#' 
#' @param station Code de la station
#' @param date Date de la pêche
#' @param commentaires \code{FALSE} par défault
#' @keywords poissons
#' @import stringr DBI RSQLite lubridate knitr
#' @export
#' @examples
#' poissons.fiche("AIN18-4", "2013-09-27", commentaires = FALSE)

poissons.fiche <- function(
  station="AIN18-4",
  date="2013-09-27",
  commentaires = FALSE)
{

if(commentaires == FALSE) fileName <- system.file("extdata", "ModeleRenduPeche.Rnw", package = "aquatools") else fileName <- system.file("extdata", "ModeleRenduPecheCommente.Rnw", package = "aquatools")
ModeleRenduPeche <- readChar(fileName, file.info(fileName)$size)

date <- ymd(date)
#datejolie <- ymd_hms(date)
#datejolie <- paste(day(datejolie), month(datejolie), year(datejolie),sep="-")

ModeleRenduPeche <- str_replace_all(ModeleRenduPeche, "SOR10-2", station)
ModeleRenduPeche <- str_replace_all(ModeleRenduPeche, "2015-05-19", date)

write.table(ModeleRenduPeche,file=paste0(station, "_", date, ".rnw"), quote=F, row.names=F, col.names=F)

# knit2pdf le fichier en question en supprimant les fichiers temporaires
knit2pdf(paste0(station, "_", date, ".rnw"))

## Suppression des fichiers temporaires de compilation
liste <- c("bib", "aux", "bbl", "blg", "log", "out", "rnw", "xml", "tex")

for(i in 1:length(liste)){
  temp <- list.files(path=".", pattern = (paste0(liste[i],"$")))
  file.remove(temp)
}

} # Fin de la fonction