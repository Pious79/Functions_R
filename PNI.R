##____________________________________________________________________________##
##  Function to calculate Percent of Normal Precipitation (PNI)               ##
##  Pierre L'HERMITE - 2017-10-17 - fc.PNI.R                                  ##
##____________________________________________________________________________##
##----------------------------------------------------------------------------##
#   Fonction : Calcul du PNI                                                  ##
##----------------------------------------------------------------------------##
#   Arguments : MonthlyData [zoo] : vecteur contenant les donnees mensuelles 
#                                   avec la date au format %Y-%m-%d
#               Delta [numeric] : defaut = 12, période pour le cumul des 
#                                 précipitations mensuelles (1 à 48)
##----------------------------------------------------------------------------##
#   Sortie    : ResPNI [list] : liste contenant :
#               PNI [zoo] : vecteur contenant les valeurs du PNI
#               Nombre [dataframe] : tableau recapitulant les types de periodes
#                                   seches et humides
##----------------------------------------------------------------------------##
#-------------------------------------------------------------------------------

fc.PNI <- function(MonthlyData, Delta = 12){
  library(hydroTSM)
  ## Verification arguments d'entree
  if (!is.zoo(MonthlyData)) { stop("MonthlyData must be a zoo"); return(NULL) }
  # --- Verification du pas de temps
  if (sfreq(MonthlyData) != "monthly") {
    stop("MonthlyData must be a daily serie \n"); return(NULL)
  }
  
  #Somme selon le pas de temps
  tmp_mean <- as.data.frame(matrix(NA, ncol = Delta, nrow = length(MonthlyData)))
  for(i in 1:Delta) {
    tmp_mean[, i] <- coredata(MonthlyData)[i:(length(MonthlyData)+i-1)]
  }
  somme <- apply(tmp_mean, 1, sum, na.rm = FALSE)
  moy<-mean(somme, na.rm = TRUE)
  cumul <- zoo(somme, order.by = index(MonthlyData))
  
  #Calcul de l'indice PNI
  PNI <- (cumul/moy)*100
  
  #Caracterisation des periodes
  ExWet <- VWet <- Wet <- Normal <- Dry <- VDry <- ExDry <- 0
  Sech <- rep(NA, length(PNI))
  for(iLength in 1:length(PNI)){
    if( is.na(PNI[iLength])){
    } else if (PNI[iLength] >= 160){
      ExWet <- ExWet + 1
      Sech[iLength] <- 3
    } else if ((PNI[iLength] >= 145 && (PNI[iLength] < 160))) {
      VWet <- VWet + 1
      Sech[iLength] <- 2
    } else if ((PNI[iLength] > 120 && (PNI[iLength] < 145))) {
      Wet <- Wet + 1
      Sech[iLength] <- 1
    } else if((120 >= PNI[iLength]) && (PNI[iLength] > 80)){
      Normal <- Normal + 1
      Sech[iLength] <- 0
    } else if((80 >= PNI[iLength]) && (PNI[iLength] > 55)){
      Dry <- Dry + 1
      Sech[iLength] <- - 1
    } else if((55 >= PNI[iLength]) && (PNI[iLength] > 40)){
      VDry <- VDry + 1
      Sech[iLength] <- - 2
    } else if((PNI[iLength] <= 40)){
      ExDry <- ExDry + 1
      Sech[iLength] <- - 3
    } else {}
  }
  
  nombre <- rbind.data.frame(ExWet, VWet, Wet, Normal, Dry, VDry, ExDry)
  colnames(nombre) <- c("Pluvio")
  row.names(nombre) <- c("Extreme Wet", "Very Wet", "Wet", "Normal", "Dry",
                       "Very Dry", "Extreme Dry")
  
  #calcul la duree des secheresses
  duree <- numeric()
  neg <- 0
  pos <- 0
  for (iLength in 1:length(PNI)) {
    if (is.na(PNI[iLength])){
      duree[iLength] <- NA
    } else if (PNI[iLength] > 100) {
      neg <- 0
      pos <- pos + 1
      duree[iLength] <- pos
    } else {
      pos <- 0
      neg <- neg - 1 
      duree[iLength] <- neg
    }
  }
  
  dureezoo <- zoo(as.numeric(duree), index(MonthlyData))
  
  ResPNI <- list(PNI = PNI, Drought = nombre, Duree = dureezoo, SechTime = Sech)
  return(ResPNI)
}