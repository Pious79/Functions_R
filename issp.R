##____________________________________________________________________________##
##  Function to calculate ISSP index of Pita                                  ##
##  Pierre L'HERMITE - 2017-10-11 - issp.R                                    ##
##____________________________________________________________________________##
##----------------------------------------------------------------------------##
#   Function : Calcul the index about rainfall anomaly by month with the
#              length, the drought type and the intensity
##----------------------------------------------------------------------------##
#   Input : monthly_data [zoo] : monthly data in zoo class with date in %Y-%m-%d
##----------------------------------------------------------------------------##
#   Output    : resissp [list] : list with 3 zoo et 1 dataframe
#                                (issp, lengthzoo, drought_type, drought_number)
#               issp [zoo] : zoo with the issp values with date in %Y-%m-%d
#               lengthzoo [zoo] : zoo with the length of drought with date
#                                 in %Y-%m-%d
#               drought_type [zoo] :
#               Dataframe contenant le nombre de secheresses selon 
#                           leur type (Extwet [ISSP>2], Verywet [1.99>ISSP>1.5],
#                           wet [1.49>ISSP>1], Normal [0.99>ISSP>-0.99],
#                           Dry [-1>ISSP>-1.49], VeryDry [-1.5>ISSP>-1.99],
#                           ExtDry [-2>ISSP])
##----------------------------------------------------------------------------##
#-------------------------------------------------------------------------------

issp <- function(monthly_data) {
  
  library(hydroTSM)
  
  ## Data input checking
  if (!is.zoo(monthly_data)) { stop("monthly_data must be a zoo"); return(NULL)}
  
  # Times step checking
  if (sfreq(monthly_data) != "monthly") {
    stop("monthly_data must be a daily serie \n"); return(NULL)
  }
  
  # Anomaly between precipitation and monthly median
  months <- substr(index(monthly_data), 6, 7)
  median_calc <- aggregate(monthly_data, by = months, FUN = median, na.rm = TRUE)
  diff <- monthly_data - coredata(median_calc)[as.numeric(months)]
  
  # 
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
  
  # Index calcul, ISSP
  vect_index <- numeric()
  
  mapa <- mean(res, na.rm = T)  
  ecapa <- sd(res, na.rm = T)
  
  vect_index <- ((res-mapa) / ecapa)
  
  issp <- zoo(as.numeric(vect_index), index(monthly_data))
  
  # Calcul the length of drought
  length_drought <- numeric()
  
  n <- 0
  p <- 0
  
  for (i in 1:length(issp)){
    if (is.na(issp[i])){
      length_drought[i] <- NA
    } else if (issp[i] > 0){
      n <- 0
      p <- p + 1
      length_drought[i] <- p
    } else{
      p <- 0
      n <- n - 1 
      length_drought[i] <- n
    }
  }
  
  lengthzoo <- zoo(as.numeric(length_drought), index(monthly_data))
  
  # Drought type and number of drought
  ext_wet <- very_wet <- wet <- normal <- dry <- very_dry <- ext_dry <-0
  
  drought_type <- rep(NA, length(issp))
  
  for(i in 1:length(issp)){
    if( is.na(issp[i])){
    } 
    else if((issp[i] >= 2)){
      ext_wet <- ext_wet + 1
      drought_type[i] <- 3
    } else if((1.99 > issp[i]) && (issp[i]> 1.5)){
      very_wet <- very_wet + 1
      drought_type[i] <- 2
    } else if((1.49 > issp[i]) && (issp[i]> 1)){
      wet <- wet + 1
      drought_type[i] <- 1
    } else if((0.99 > issp[i]) && (issp[i]> -0.99)){
      normal <- normal + 1
      drought_type[i] <- 0
    } else if((-1 >= issp[i]) && (issp[i]> -1.49)){
      dry <- dry + 1
      drought_type[i] <- - 1
    } else if((-1.5 >= issp[i]) && (issp[i]> -1.99)){
      very_dry <- very_dry + 1
      drought_type[i] <- - 2
    } else if((issp[i] <= -2)){
      ext_dry <- ext_dry + 1
      drought_type[i] <- - 3
    } else {}
  }
  
  drought_number <- rbind.data.frame(ext_wet, very_wet, wet, normal,
                                     dry, very_dry, ext_dry)
  colnames(drought_number) <- c("Pluvio")
  row.names(drought_number) <- c("ExWet", "VWet", "Wet", "Normal", "Dry",
                                 "VDry", "ExDry")
  
resissp <- list(issp = issp, length = lengthzoo, drought = drought_number,
                drought_time = drought_type)
return(resissp)

}