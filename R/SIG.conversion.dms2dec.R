#' Conversion de coordonnées en degrés-minutes-secondes vers degrés décimaux
#'
#' Cette fonction permet de convertir des coordonnées en degrés-minutes-secondes vers des degrés décimaux
#' @name SIG.conversion.dms2dec
#' @param dms Coordonnées à convertir
#' @param separators Séparateurs de valeurs utilisés
#' @keywords SIG
#' @export
#' @examples
#' SIG.conversion.dms2dec("41° 34.1826' N")

    SIG.conversion.dms2dec <- function(dms, separators = c("º", "°","’’", "\'\'","\'", "’",  "\"",  "\\?")) {
      
      # by A. Marcia Barbosa (https://modtools.wordpress.com/)
      #   contributions by PaulMelloy (https://github.com/PaulMelloy)
      # license: CC BY-SA 4.0 (Creative Commons)
      # like R, this function is free, open-source and comes with absolutely no warranty; bug reports welcome!
      
      # version 1.5 (4 Feb 2022)
      # dms: a vector of latitude or longitude in degrees-minutes-seconds-hemisphere,
      #   e.g. 41° 34' 10.956" N (with or without spaces) or
      # ddm: a vector of latitude or longitude in degrees-decimal minutes
      #   e.g. 41° 34.1826' N (with or without spaces)
      # separators: the characters that are separating degrees, minutes and seconds in dms
      
      # to source this function, remember to add encoding: source("https://raw.githubusercontent.com/AMBarbosa/unpackaged/master/dms2dec", encoding = "UTF-8")
      
      
      dms <- as.character(dms)
      dms <- gsub(pattern = " ", replacement = "", x = dms)
      for (s in separators) dms <- gsub(pattern = s, replacement = "_splitHere_", x = dms)
      
      splits <- strsplit(dms, split = "_splitHere_")
      n <- length(dms)
      deg <- min <- sec <- hem <- vector("character", n)
      
      for (i in 1:n) {
        deg[i] <- splits[[i]][1]
        min[i] <- splits[[i]][2]
        
        # Check if Degrees decimal minutes
        dm <- unlist(strsplit(min[i], "[.]"))
        if (length(dm) > 1) {
          if (length(dm) > 2)
            stop("Input minutes format is not recongnisable")
          
          sec[i] <- as.numeric(paste0("0.", dm[2], collapse = "")) * 60
          min[i] <- dm[1]
          if(min[i] > 60) stop("minutes entry exceeds 60, check minutes input")
          
          hem[i] <-
            splits[[i]][splits[[i]] %in% c("N", "S", "E", "W", "n", "s", "e", "w")]
          
        } else{
          if (length(splits[[i]]) < 4) {
            hem[i] <- splits[[i]][3]
          } else {
            sec[i] <- splits[[i]][3]
            hem[i] <- splits[[i]][4]
          }
        }
      }
      
      dec <- colSums(rbind(as.numeric(deg), (as.numeric(min) / 60), (as.numeric(sec) / 3600)), na.rm = TRUE)
      sign <- ifelse (hem %in% c("N", "E"), 1, -1)
      hem_miss <- which(is.na(hem))
      if (length(hem_miss) > 0) {
        warning("Hemisphere not specified in position(s) ", hem_miss, ", so the sign of the resulting coordinates may be wrong.")
      }
      dec <- sign * dec
      return(dec)
    }  # end dms2dec function
