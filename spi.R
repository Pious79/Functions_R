##____________________________________________________________________________##
##  Function to calculate SPI                                                 ##
##  Pierre L'HERMITE - 2017-10-12 - spi.R                                     ##
##____________________________________________________________________________##
##----------------------------------------------------------------------------##
#   Description: Calculate the standardized precipitation index and the       ##
#                drought specifications                                       ##
##----------------------------------------------------------------------------##
#   Arguments: prec_data [zoo]: rainfall monthly data in zoo class 
#                               with date in %Y-%m-%d
#              time_step [numeric] : default = 12, time step to sum monthly 
#                                   precipitation (1, 3, 6, 9, 12, 24 and 48)
#              distribution [character] : distribution of data (log_Logistic,
#                                           gamma, grev, genlog, normal)
##----------------------------------------------------------------------------##
#   Values: resspi [list] : list with 3 zoo et 1 dataframe
#                           (spi, length_zoo, drought_type, drought_number)
#           spi [zoo] : zoo with the spi values with date in %Y-%m-%d
#           length_zoo [zoo] : zoo with the length of drought with date
#                              in %Y-%m-%d
#           drought_type [zoo] : zoo with the type of the period for
#                                 each month 
#           drought_number [dataframe] : dataframe with the number of 
#                           different period by type
#                           Extwet [spi>2], Verywet [1.99>spi>1.5],
#                           wet [1.49>spi>1], Normal [0.99>spi>-0.99],
#                           Dry [-1>spi>-1.49], VeryDry [-1.5>spi>-1.99],
#                           ExtDry [-2>spi])
##----------------------------------------------------------------------------##
#-------------------------------------------------------------------------------

spi <- function(prec_data, time_step = 12, distribution = "gamma"){
  
  library(SCI)
  ## Verification arguments d'entree
  if (!is.zoo(prec_data)) { stop("prec_data must be a zoo"); return(NULL) }
  # --- Verification du pas de temps
  if (sfreq(prec_data) != "monthly") {
    stop("prec_data must be a daily serie \n"); return(NULL)
  }
  
  #Premier mois
  firstmonth <- as.numeric(substr(index(prec_data[1]), 6, 7))
  
  #Utilisation du package SCI pour calculer le SPI et creation zoo
  spi.para <- fitSCI(coredata(prec_data), first.mon = firstmonth,
                     distr = Distribution, time.scale = time_step, p0 = TRUE)
  SPI_val <- transformSCI(coredata(prec_data), first.mon = firstmonth,
                      obj = spi.para)
  SPI <- zoo(SPI_val, order.by = index(prec_data))
  
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