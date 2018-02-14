##____________________________________________________________________________##
##  Function to calculate SPEI                                                ##
##  Pierre L'HERMITE - 2017-10-12 - fc.SPEI.R                                 ##
##____________________________________________________________________________##
##----------------------------------------------------------------------------##
#   Fonction : Calcul l'indice standardise de precipitation-evapotranspiration##
#              secheresse et leur duree                                       ##
##----------------------------------------------------------------------------##
#   Arguments : Data [zoo] : vecteur contenant la difference entre la pluie 
#                            et l'evapotranspiration
#                            avec la date au format %Y-%m-%d [mm/month]
#               Delta [numeric] : valeur du pas de temps pour le SSFI (1, 3, 6,
#                                 9, 12, 24 en general) [en nombre de mois]
#               Distribution [character] : distribution des donnees 
#                                          (log_Logistic, gamma,
#                                          grev, genlog, normal)
##----------------------------------------------------------------------------##
#   Sortie    : ResSPEI [list] : liste contenant 2 zoo et un data frame
#                               (SPEI , Drought, Time)
#               SPEI [zoo] : vecteur contenant les valeurs du SPEI avec la
#                           date au format %Y-%m-%d
#               Drought [df] : recapitulatif du nombre de secheresse par les
#                             differentes categories de le SPEI (ExtWet [SPEI>2],
#                             VeryWet [1.99>SPEI>1.5], Wet [1.49>SPEI>1], Normal
#                             [0.99>SPEI>-0.99], Dry [-1>SPEI>-1.49], VeryDry
#                             [-1.5>SPEI>-1.99], ExtDry [-2>SPEI])
#               Time [zoo] : vecteur contenant la duree des differentes periodes
#                            avec la date au format %Y-%m-%d. Lorsque l'indice
#                            positif, c'est une periode humide et lorsque
#                            l'indice est negatif, la periode est seche
##----------------------------------------------------------------------------##
#-------------------------------------------------------------------------------

spei <- function(Data, Delta=12, Distribution = "log-Logistic"){
  library(SPEI)
  library(hydroTSM)
  ## Verification arguments d'entree
  if (!is.zoo(Data)) { stop("Data must be a zoo"); return(NULL) }
  # --- Verification du pas de temps
  if (sfreq(Data) != "monthly") {
    stop("Data must be a daily serie \n"); return(NULL)
  }
  
  #Utilisation du package SCI pour calculer le SSFI
  Res <- spei(coredata(Data[which(!is.na(Data))]), scale = Delta, distribution = Distribution,
       na.rm = TRUE)
  SPEI <- zoo(as.numeric(Res$fitted), order.by = index(Data[which(!is.na(Data))]))
  
  #Comptage du nombre de secheresse
  ExWet <- VWet <- Wet <- Normal <- Dry <- VDry <- ExDry<-0
  Sech <- rep(NA, length(SPEI))
  
  for(iLength in 1:length(coredata(SPEI))){
    if( is.na(coredata(SPEI)[iLength])){
    } else if((coredata(SPEI)[iLength] >= 2)){
      ExWet <- ExWet + 1
      Sech[iLength] <- 3
    } else if((1.99 > coredata(SPEI)[iLength]) && (coredata(SPEI)[iLength] > 1.5)){
      VWet <- VWet + 1
      Sech[iLength] <- 2
    } else if((1.49 > coredata(SPEI)[iLength]) && (coredata(SPEI)[iLength] > 1)){
      Wet <- Wet + 1
      Sech[iLength] <- 1
    } else if((0.99 > coredata(SPEI)[iLength]) && (coredata(SPEI)[iLength] > -0.99)){
      Normal <- Normal + 1
      Sech[iLength] <- 0
    } else if((-1 >= coredata(SPEI)[iLength]) && (coredata(SPEI)[iLength] > -1.49)){
      Dry <- Dry + 1
      Sech[iLength] <- - 1
    } else if((-1.5 >= coredata(SPEI)[iLength]) && (coredata(SPEI)[iLength] > -1.99)){
      VDry <- VDry + 1
      Sech[iLength] <- - 2
    } else {
      ExDry <- ExDry + 1
      Sech[iLength] <- - 3
    } 
  }
  nombre <- as.data.frame(c(ExWet, VWet, Wet, Normal, Dry, VDry, ExDry))
  colnames(nombre)<-c("Pluvio")
  row.names(nombre)<-c("ExWet", "VWet", "Wet", "Normal", "Dry", "VDry", "ExDry")
  
  #calcul la duree des periodes seches et humides
  duree <- numeric()
  neg <- 0
  pos <- 0
  for (iLength in 1:length(SPEI)) {
    if (is.na(SPEI[iLength])){
      duree[iLength] <- NA
    } else if (SPEI[iLength] > 0) {
      neg <- 0
      pos <- pos + 1
      duree[iLength] <- pos
    } else {
      pos <- 0
      neg <- neg - 1 
      duree[iLength] <- neg
    }
  }
  
  dureezoo<-zoo(as.numeric(duree), index(SPEI))
  
  Resultat<-list(SPEI = SPEI, Drought = nombre, Time = dureezoo, SechTime = Sech)
  
  return(Resultat)
}