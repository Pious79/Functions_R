##______________________________________________________________________________##
##  Function to calculate Rainfall Anomaly Index (RAI)                          ##
##  Pierre L'HERMITE - 2017-10-17 - fc.RAI.R                                    ##
##______________________________________________________________________________##
##------------------------------------------------------------------------------##
#   Fonction : Calcul de l'indice des anomalies pluviometriques                 ##
##------------------------------------------------------------------------------##
#   Arguments : MonthlyData [zoo] : vecteur contenant les donnees mensuelles 
#                                   des precipitations avec la date au format 
#                                   %Y-%m-%d [mm/month]
##------------------------------------------------------------------------------##
#   Sortie    : ResRAI [list] : liste contenant :
#               RAI [zoo] : zoo contenant les valeurs du RAI
#               Nombre [dataframe] : tableau recapitulant les types de periodes
#                                    seches et humides
##------------------------------------------------------------------------------##
#---------------------------------------------------------------------------------

rai <- function(MonthlyData, Delta = 12){
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
  
  #Calcul de la moyenne des precipitations, des 10 plus grandes et 10 plus petites
  #precipitations (na.last permet de placer les NA en debut ou fin de vecteur) 
  avgP<-mean(cumul, na.rm = TRUE)
  avgEh<-mean(sort(coredata(cumul),
                   na.last = FALSE)[(length(cumul)-9):length(cumul)], na.rm = TRUE)
  avgEm<-mean(sort(coredata(cumul),
                   na.last = TRUE)[1:10], na.rm = TRUE)
  
  #Calcul du rainfall anomaly index
  RAI <- rep(NA, length(cumul))
  res <- cumul - avgP
  for (i in c(1:length(res))){
    if (is.na(coredata(res)[i])) {
      RAI[i] <- NA
    } else if(coredata(res)[i] <= 0){
      RAI[i] <- (coredata(res)[i]*-3)/(avgEm-avgP)
    } else {
      RAI[i] <- (coredata(res)[i]*3)/(avgEh-avgP)
    }
  }
  RAI <- zoo(RAI, order.by = index(cumul))
  
  #Decompte des types de periodes
  ExWet <- VWet <- Wet <- Normal <- Dry <- VDry <- ExDry <- 0
  Sech <- rep(NA, length(RAI))
  
  for (i in 1:length(coredata(RAI))) {
    if (is.na(coredata(RAI)[i])) {
    } else if ((coredata(RAI)[i] >= 3)) {
      ExWet <- ExWet + 1
      Sech[i] <- 3
    } else if ((2.99 > coredata(RAI)[i]) && (coredata(RAI)[i] > 2)) {
      VWet <- VWet + 1
      Sech[i] <- 2
    } else if ((1.99 > coredata(RAI)[i]) && (coredata(RAI)[i] > 1)) {
      Wet <- Wet + 1
      Sech[i] <- 1
    } else if ((0.99 > coredata(RAI)[i]) && (coredata(RAI)[i] > -0.99)) {
      Normal <- Normal+1
      Sech[i] <- 0
    } else if ((-1 >= coredata(RAI)[i]) && (coredata(RAI)[i] > -1.99)) {
      Dry <- Dry + 1
      Sech[i] <- - 1
    } else if ((-2 >= coredata(RAI)[i]) && (coredata(RAI)[i] > -2.99)) {
      VDry <- VDry + 1
      Sech[i] <- - 2
    } else if ((coredata(RAI)[i] <= -3)) {
      ExDry <- ExDry + 1
      Sech[i] <- - 3
    } else {}
  }
  
  nombre <- rbind.data.frame(ExWet, VWet, Wet, Normal, Dry, VDry, ExDry)
  colnames(nombre) <- c("Pluvio")
  row.names(nombre) <- c("Exteme Wet", "Very Wet", "Wet", "Normal", "Dry",
                       "Very Dry", "Extreme Dry")
  
  #calcul la duree des secheresses
  duree <- numeric()
  neg <- 0
  pos <- 0
  for (iLength in 1:length(RAI)) {
    if (is.na(RAI[iLength])){
      duree[iLength] <- NA
    } else if (RAI[iLength] > 0) {
      neg <- 0
      pos <- pos + 1
      duree[iLength] <- pos
    } else {
      pos <- 0
      neg <- neg - 1 
      duree[iLength] <- neg
    }
  }
  
  dureezoo <- zoo(as.numeric(duree), index(RAI))
  
  ResRAI <- list(RAI = RAI, Drought = nombre, Duree = dureezoo, SechTime = Sech)
}