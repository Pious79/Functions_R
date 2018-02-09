##______________________________________________________________________________##
##  Function to calculate ISSQ index of Pita                                    ##
##  Pierre L'HERMITE - 2018-01-19 - fc.ISSQ.R                                   ##
##______________________________________________________________________________##
##------------------------------------------------------------------------------##
#   Fonction : Calcul l'indice standardise de secheresses hydrologiques         ##
#              de Pita avec la duree, l'intensite et le type de secheresse      ##
##------------------------------------------------------------------------------##
#   Arguments : data [zoo] : vecteur contenant les donnees mensuelles avec la
#                            date au format %Y-%m-%d
##------------------------------------------------------------------------------##
#   Sortie    : ResISSQ [list] : liste contenant 2 zoo et un df
#                                (ISSQ, Duree, Sech)
#               ISSQ [zoo] : vecteur contenant les valeurs de l'ISSQ avec la
#                            date au format %Y-%m-%d
#               Duree [zoo] : vecteur contenant le nombre de mois cumule pour
#                             chaque secheresse avec la date au format %Y-%m-%d
#               Sech [df] : Dataframe contenant le nombre de secheresses selon 
#                           leur type (ExtWet [ISSQ>2], VeryWet [1.99>ISSQ>1.5],
#                           Wet [1.49>ISSQ>1], Normal [0.99>ISSQ>-0.99],
#                           Dry [-1>ISSQ>-1.49], VeryDry [-1.5>ISSQ>-1.99],
#                           ExtDry [-2>ISSQ])
##------------------------------------------------------------------------------##
#---------------------------------------------------------------------------------

fc.ISSQ<-function(MonthlyData) {
  library(hydroTSM)
  ## Verification arguments d'entree
  if (!is.zoo(MonthlyData)) { stop("MonthlyData must be a zoo"); return(NULL) }
  # --- Verification du pas de temps
  if (sfreq(MonthlyData) != "monthly") {
    stop("MonthlyData must be a daily serie \n"); return(NULL)
  }
  
  months <- substr(index(MonthlyData), 6, 7)
  mediane <- aggregate(MonthlyData, by = months, FUN = median, na.rm = TRUE)
  diff <- MonthlyData - coredata(mediane)[as.numeric(months)]
  
  res <- rep(NA, length(diff))
  som <- 0
  
  for (i in 1:length(coredata(diff))){
    if(is.na(coredata(diff)[i])){
      res[i] <- coredata(diff)[i]
    }
    else if(coredata(diff)[i] > 0){
      som <- som + coredata(diff)[i]
      res[i] <- som
    }
    else if (som >= 0 || is.na(som)){
      som <- 0
      som <- som + coredata(diff)[i]
      res[i] <- som
    }
    else {
      som <- som + coredata(diff)[i]
      res[i] <- som
    }
  }  
  
  #calcul de l'indice de PITA, ISSQ
  Indice <- numeric()
  
  Mapa <- mean(res, na.rm = T)  
  ECapa <- sd(res, na.rm = T)
  
  Indice <- ((res-Mapa)/ECapa)
  
  ISSQ <- zoo(as.numeric(Indice), index(MonthlyData))
  
  #calcul la duree des secheresses
  duree <- numeric()
  n <- 0
  p <- 0
  for (i in 1:length(ISSQ)){
    if (is.na(ISSQ[i])){
      duree[i] <- NA
    } else if (ISSQ[i] > 0){
      n <- 0
      p <- p + 1
      duree[i] <- p
    } else{
      p <- 0
      n <- n - 1 
      duree[i] <- n
    }
  }
  
  dureezoo <- zoo(as.numeric(duree), index(MonthlyData))
  
  #Classement secheresse
  ExWet <- VWet <- Wet <- Normal <- Dry <- VDry <- ExDry <-0
  Sech <- rep(NA, length(ISSQ))
  for(i in 1:length(ISSQ)){
    if( is.na(ISSQ[i])){
    } 
    else if((ISSQ[i] >= 2)){
      ExWet <- ExWet + 1
      Sech[i] <- 3
    } else if((1.99 > ISSQ[i]) && (ISSQ[i]> 1.5)){
      VWet <- VWet + 1
      Sech[i] <- 2
    } else if((1.49 > ISSQ[i]) && (ISSQ[i]> 1)){
      Wet <- Wet + 1
      Sech[i] <- 1
    } else if((0.99 > ISSQ[i]) && (ISSQ[i]> -0.99)){
      Normal <- Normal + 1
      Sech[i] <- 0
    } else if((-1 >= ISSQ[i]) && (ISSQ[i]> -1.49)){
      Dry <- Dry + 1
      Sech[i] <- - 1
    } else if((-1.5 >= ISSQ[i]) && (ISSQ[i]> -1.99)){
      VDry <- VDry + 1
      Sech[i] <- - 2
    } else if((ISSQ[i] <= -2)){
      ExDry <- ExDry + 1
      Sech[i] <- - 3
    } else {}
  }
  
  nombre <- rbind.data.frame(ExWet, VWet, Wet, Normal, Dry, VDry, ExDry)
  colnames(nombre) <- c("Pluvio")
  row.names(nombre) <- c("ExWet", "VWet", "Wet", "Normal", "Dry", "VDry",
                         "ExDry")
  
  ResISSQ <- list(ISSQ = ISSQ, Duree = dureezoo, Drought = nombre, SechTime = Sech)
  return(ResISSQ)
  
}