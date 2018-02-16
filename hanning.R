##____________________________________________________________________________##
##  Function to filter data with Hanning filter order 2                       ##
##  Pierre L'HERMITE - 2017-07-19 - hanning                                   ##
##____________________________________________________________________________##
##----------------------------------------------------------------------------##
#   Fonction : Calcul les donnees avec le filtre passe bas d'Hanning d'ordre 2##
##----------------------------------------------------------------------------##
#   Arguments : data [zoo] : vecteur contenant les donnees journalieres,
#                            mensuelles, saisonnieres et annuelles, 
#                            avec la date au format %Y-%m-%d
##----------------------------------------------------------------------------##
#   Sortie    : Hanzoo [zoo] : vecteur contenant les donnees filtrees avec la 
#                              date au format %Y-%m-%d
##----------------------------------------------------------------------------##
#-------------------------------------------------------------------------------

hanning <- function(data){
  
  library(hydroTSM)
  
  ## Verification arguments d'entree
  if (!is.zoo(data)) { stop("Data must be a zoo"); return(NULL) }
  
  ind_run <- which(!is.na(coredata(data)))
  
  #Creation et remplissage du vecteur par le filtre d'Hanning
  han <- rep(NA, length(data))
  
  han[ind_run[1]] <- ((0.54*coredata(data[ind_run[1]]))
                      + (0.46*coredata(data[ind_run[2]])))
  han[ind_run[2]] <- ((0.25*coredata(data[ind_run[1]]))
                      + (0.5*coredata(data[ind_run[2]]))
                      + (0.25*coredata(data[ind_run[3]])))
  
  for (i in c(3:(length(ind_run)-2))){
    han[ind_run[i]] <- ((0.06*coredata(data[ind_run[i-2]]))
                        + (0.25*coredata(data[ind_run[i-1]]))
                        + (0.38*coredata(data[ind_run[i]]))
                        + (0.25*coredata(data[ind_run[i+1]]))
                        + (0.06*coredata(data[ind_run[i+2]])))
  }
  
  han[ind_run[length(ind_run)]-1] <- ((0.25*coredata(data[ind_run[length(ind_run)]-2]))
                                      + (0.5*coredata(data[ind_run[length(ind_run)]-1])) 
                                      + (0.25*coredata(data[ind_run[length(ind_run)]])))
  han[ind_run[length(ind_run)]] <- ((0.54*coredata(data[ind_run[length(ind_run)]]))
                                    +(0.46*coredata(data[ind_run[length(ind_run)]-1])))
  
  hanzoo <- zoo(as.numeric(han), index(data))
  return(hanzoo)
}