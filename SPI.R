##____________________________________________________________________________##
##  Function to calculate SPI index                                           ##
##  Pierre L'HERMITE - 2017-10-12 - fc.SPI.R                                  ##
##____________________________________________________________________________##
##----------------------------------------------------------------------------##
#   Fonction : Calcul l'indice standardise des precipitations avec les types  ##
#              de secheresse et la duree                                      ##
##----------------------------------------------------------------------------##
#   Arguments : MonthlyData [zoo] : vecteur contenant les donnees mensuelles 
#                                   des precipitations avec la date au format 
#                                   %Y-%m-%d [mm/month]
#               Delta [numeric] : valeur du pas de temps pour le SPI (1, 3, 6, 9
#                                 ,12, 24, et 48 en general) [en nombre de mois]
#               Distribution [character] : distribution des donnees (gamma,
#                                          grev, genlog, normal)
##----------------------------------------------------------------------------##
#   Sortie    : ResSPI [list] : liste contenant 2 zoo et un data frame
#                               (SPI , Drought, Time)
#               SPI [zoo] : vecteur contenant les valeurs du SPI avec la
#                           date au format %Y-%m-%d
#               Drought [df] : recapitulatif du nombre de secheresse par les
#                             differentes categories de l'SPI (ExtWet [SPI>2],
#                             VeryWet [1.99>SPI>1.5], Wet [1.49>SPI>1], Normal
#                             [0.99>SPI>-0.99], Dry [-1>SPI>-1.49], VeryDry
#                             [-1.5>SPI>-1.99], ExtDry [-2>SPI])
#               Time [zoo] : vecteur contenant la duree des differentes periodes
#                            avec la date au format %Y-%m-%d. Lorsque l'indice
#                            positif, c'est une periode humide et lorsque
#                            l'indice est negatif, la periode est seche
##----------------------------------------------------------------------------##
#-------------------------------------------------------------------------------

spi <- function(MonthlyData, Delta=12, Distribution = "gamma"){
  
  library(hydroTSM)
  library(SCI)
  ## Verification arguments d'entree
  if (!is.zoo(MonthlyData)) { stop("MonthlyData must be a zoo"); return(NULL) }
  # --- Verification du pas de temps
  if (sfreq(MonthlyData) != "monthly") {
    stop("MonthlyData must be a daily serie \n"); return(NULL)
  }
  
  #Premier mois
  firstmonth <- as.numeric(substr(index(MonthlyData[1]), 6, 7))
  
  #Utilisation du package SCI pour calculer le SPI et creation zoo
  spi.para <- fitSCI(coredata(MonthlyData), first.mon = firstmonth,
                     distr = Distribution, time.scale = Delta, p0 = TRUE)
  SPI_val <- transformSCI(coredata(MonthlyData), first.mon = firstmonth,
                      obj = spi.para)
  SPI <- zoo(SPI_val, order.by = index(MonthlyData))
  
  #Comptage du nombre de secheresse
  ExWet <- VWet <- Wet <- Normal <- Dry <- VDry <- ExDry<-0
  Sech <- rep(NA, length(SPI))
  
  for(iLength in 1:length(coredata(SPI))){
    if( is.na(coredata(SPI)[iLength])){
    } else if((coredata(SPI)[iLength] >= 2)){
      ExWet <- ExWet + 1
      Sech[iLength] <- 3
    } else if((1.99 > coredata(SPI)[iLength]) && (coredata(SPI)[iLength] > 1.5)){
      VWet <- VWet + 1
      Sech[iLength] <- 2
    } else if((1.49 > coredata(SPI)[iLength]) && (coredata(SPI)[iLength] > 1)){
      Wet <- Wet + 1
      Sech[iLength] <- 1
    } else if((0.99 > coredata(SPI)[iLength]) && (coredata(SPI)[iLength] > -0.99)){
      Normal <- Normal + 1
      Sech[iLength] <- 0
    } else if((-1 >= coredata(SPI)[iLength]) && (coredata(SPI)[iLength] > -1.49)){
      Dry <- Dry + 1
      Sech[iLength] <- - 1
    } else if((-1.5 >= coredata(SPI)[iLength]) && (coredata(SPI)[iLength] > -1.99)){
      VDry <- VDry + 1
      Sech[iLength] <- - 2
    } else {
      ExDry <- ExDry + 1
      Sech[iLength] <- - 3
    } 
  }
  nombre <- as.data.frame(c(ExWet, VWet, Wet, Normal, Dry, VDry, ExDry))
  colnames(nombre) <- c("Pluvio")
  row.names(nombre) <- c("ExWet", "VWet", "Wet", "Normal", "Dry", "VDry", "ExDry")
  
  #calcul la duree des periodes seches et humides
  duree <- numeric()
  neg <- 0
  pos <- 0
  for (iLength in 1:length(SPI)) {
    if (is.na(SPI[iLength])){
      duree[iLength] <- NA
    } else if (SPI[iLength] > 0) {
      neg <- 0
      pos <- pos + 1
      duree[iLength] <- pos
    } else {
      pos <- 0
      neg <- neg - 1 
      duree[iLength] <- neg
    }
  }
  
  dureezoo <- zoo(as.numeric(duree), index(SPI))
  
  Resultat <- list(SPI = SPI, Drought = nombre, Duree = dureezoo, SechTime = Sech)
  return(Resultat)
}